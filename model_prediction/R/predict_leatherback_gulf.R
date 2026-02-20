# ==============================================================================
# PREDICT LEATHERBACK GULF (GitHub Actions Version)
# ==============================================================================

# --- 1. GHA SYSTEM PATCHES ---
library(methods)
library(INLA)
library(inlabru)

# Override internal version checks that cause strsplit crashes on Linux
assignInNamespace("inla.version", function(...) "25.10.19", ns = "INLA")
assignInNamespace("bru_info_upgrade", function(object, ...) return(object), ns = "inlabru")

options(inlabru_upgrade_check = FALSE)
sf::sf_use_s2(FALSE) 

# --- 2. GOOGLE CLOUD & GITHUB DOWNLOADS ---
library(googleCloudStorageR) # For authenticated GCS downloads

local_dir <- "model_prediction/gulf/results"
if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

# A. Download INLA Model from Google Cloud Storage
message("Fetching INLA model from GCS...")

# Authenticate using the environment variable created by GitHub Actions
gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))

# Download the model
gcs_get_object("Models/inla.st.etag.2to1.rds",
               bucket = "mitchellrider-speciesdistmodel-seaturtles_gulfatlantic",
               saveToDisk = file.path(local_dir, "inla.st.etag.2to1.rds"),
               overwrite = TRUE)


# B. Download Scaling Params from GitHub
message("Fetching scaling params from GitHub...")
github_raw_url <- "https://raw.githubusercontent.com/EcoCast-Plus/operationalization/main/model_prediction/gulf/results/scaling_params_2026.02.05.rds"

download.file(github_raw_url, 
              destfile = file.path(local_dir, "scaling_params_2026.02.05.rds"), 
              mode = "wb", 
              quiet = TRUE)

# --- 3. LIBRARIES & SETTINGS ---
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyterra)

MODEL_RDS   <- file.path(local_dir, "inla.st.etag.2to1.rds")
SCALING_RDS <- file.path(local_dir, "scaling_params_2026.02.05.rds")
GRID_DIR    <- "model_prediction/gulf/predictions"
OUTPUT_DIR  <- "model_prediction/gulf/predictions"
GOM_CRS     <- 32616 

date_str <- as.character(Sys.Date() + 1)
target_month <- as.numeric(format(Sys.Date() + 1, "%m"))

# --- 4. LOAD & TRANSFORM ---
model <- readRDS(MODEL_RDS)
scaling_params <- readRDS(SCALING_RDS)

# Force object metadata compatibility
class(model) <- c("bru", "iinla", "inla", "list")
model$bru_info$inlabru_version <- package_version("2.12.0")
attr(model$bru_info, "upgraded") <- TRUE

# --- 5. ENVIRONMENTAL DATA PREP ---
all_files <- list.files(GRID_DIR, full.names = TRUE)
env_vars <- c("thetao", "zos", "z", "l.chl", "l.tke_mean")
name_map <- list("thetao"="sst", "zos"="ssh", "z"="bathymetry", "l.chl"="chlorophyll", "l.tke_mean"="tke")

layers <- list()
for (v in env_vars) {
  suffix <- name_map[[v]]
  target_path <- all_files[str_detect(all_files, paste0("PRED_", date_str, "_", suffix, ".tif$"))][1]
  if (is.na(target_path)) target_path <- tail(sort(all_files[str_detect(all_files, paste0("_", suffix, ".tif$"))]), 1)
  
  r <- terra::rast(target_path)
  if (suffix %in% c("chlorophyll", "tke")) r <- log(r + 0.001)
  
  mu_name <- if(v == "l.tke_mean") "l.tke_mean_mean" else paste0(v, "_mean")
  sd_name <- if(v == "l.tke_mean") "l.tke_mean_sd"   else paste0(v, "_sd")
  
  r <- (r - scaling_params[[mu_name]]) / scaling_params[[sd_name]]
  names(r) <- v
  layers[[v]] <- r
}

pred_df <- as.data.frame(terra::rast(layers), xy = TRUE, na.rm = TRUE)
pred_sf <- st_as_sf(pred_df, coords = c(1,2), crs = 4326) %>% st_transform(GOM_CRS)
pred_sf$month <- target_month

# --- 6. PREDICT & EXPORT ---
message("Generating prediction...")
inla_pred <- predict(model, newdata = pred_sf, 
                     formula = ~ plogis(intercept_etag + thetao + zos + z + l.chl + l.tke_mean + etag_field), 
                     n.samples = 50, num.threads = 2)

# Map back to grid
r_out <- terra::rast(layers[[1]])
r_out <- terra::setValues(r_out, NA)
pred_points_fixed <- sf::st_transform(inla_pred, terra::crs(r_out))
cells <- terra::cellFromXY(r_out, sf::st_coordinates(pred_points_fixed))
r_out[cells[!is.na(cells)]] <- inla_pred$mean[!is.na(cells)]

# Save outputs
writeRaster(r_out, file.path(OUTPUT_DIR, paste0("PRED_", date_str, "_leatherback.tif")), overwrite=TRUE)
writeRaster(r_out > 0.71, file.path(OUTPUT_DIR, paste0("CORE_", date_str, "_leatherback.tif")), overwrite=TRUE)

# Final plot
p <- ggplot() + geom_spatraster(data = r_out) + scale_fill_viridis_c(option="mako") + theme_minimal()
ggsave(file.path(OUTPUT_DIR, paste0("PLOT_", date_str, "_leatherback.png")), p, width=10, height=7)

# --- 7. CLEANUP ---
file.remove(MODEL_RDS, SCALING_RDS)
# Optional: Remove the auth file for security
if (file.exists(Sys.getenv("GCS_AUTH_FILE"))) file.remove(Sys.getenv("GCS_AUTH_FILE"))
message("Process complete.")
