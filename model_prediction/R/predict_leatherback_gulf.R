# --- GHA ENVIRONMENT FIXES ---
# Bypass version-check crashes and prevent automatic upgrades in the cloud
assignInNamespace("inla.version", function(...) "24.12.11", ns = "INLA")
options(inlabru_upgrade_check = FALSE)

# --- GOOGLE DRIVE DOWNLOAD ---
library(googledrive)
# Use the relative path for the GitHub runner environment
local_dir <- "model_prediction/gulf/results"
if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

drive_deauth() # Public access mode
folder_id <- "1a7yxmaZUm7RLolJ-svD5QYFXQZxJW5If"
targets <- c("inla.st.etag.2to1.rds", "scaling_params_2026.02.05.rds")

message("Checking for model files on Google Drive...")
for (target in targets) {
  drive_download(
    file = as_id(drive_ls(as_id(folder_id), pattern = target)$id),
    path = file.path(local_dir, target),
    overwrite = TRUE
  )
}

# --- LIBRARIES ---
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyterra)
library(INLA)
library(inlabru)
library(fmesher)

assignInNamespace("bru_info_upgrade", function(object, ...) {
  message("GHA Patch: Skipping bru_info_upgrade to prevent strsplit crash.")
  return(object)
}, ns = "inlabru")

# Double-check the version override is also active
assignInNamespace("inla.version", function(...) "25.10.19", ns = "INLA")                  

# --- SETTINGS ---
MODEL_RDS   <- file.path(local_dir, "inla.st.etag.2to1.rds")
SCALING_RDS <- file.path(local_dir, "scaling_params_2026.02.05.rds")
GRID_DIR    <- "model_prediction/gulf/predictions"
OUTPUT_DIR  <- "model_prediction/gulf/predictions"
GOM_CRS     <- 32616 

# Date Logic
date_forecast <- Sys.Date() + 1
date_str      <- as.character(date_forecast)
target_month  <- as.numeric(format(date_forecast, "%m"))

# --- LOAD & FIX MODEL ---
message("Loading Model...")
model <- readRDS(MODEL_RDS)
scaling_params <- readRDS(SCALING_RDS)

# Force Class and Version metadata (Crucial for inlabru stability)
class(model) <- c("bru", "iinla", "inla", "list")
model$bru_info$inlabru_version <- package_version("2.12.0")
attr(model$bru_info, "upgraded") <- TRUE

# --- DATA PREPARATION ---
all_files <- list.files(GRID_DIR, full.names = TRUE)
env_vars <- c("thetao", "zos", "z", "l.chl", "l.tke_mean")
name_map <- list("thetao"="sst", "zos"="ssh", "z"="bathymetry", "l.chl"="chlorophyll", "l.tke_mean"="tke")

layers <- list()
for (v in env_vars) {
  suffix <- name_map[[v]]
  # Search for the exact date or the most recent fallback
  target_path <- all_files[str_detect(all_files, paste0("PRED_", date_str, "_", suffix, ".tif$"))][1]
  if (is.na(target_path)) target_path <- tail(sort(all_files[str_detect(all_files, paste0("_", suffix, ".tif$"))]), 1)
  
  if (is.null(target_path) || is.na(target_path)) stop(paste("Missing env layer:", suffix))
  
  r <- terra::rast(target_path)
  if (suffix %in% c("chlorophyll", "tke")) r <- log(r + 0.001)
  
  mu_name <- if(v == "l.tke_mean") "l.tke_mean_mean" else paste0(v, "_mean")
  sd_name <- if(v == "l.tke_mean") "l.tke_mean_sd"   else paste0(v, "_sd")
  
  r <- (r - scaling_params[[mu_name]]) / scaling_params[[sd_name]]
  names(r) <- v
  layers[[v]] <- r
}

predictor_stack <- terra::rast(layers)
pred_df <- as.data.frame(predictor_stack, xy = TRUE, na.rm = TRUE)
pred_sf <- st_as_sf(pred_df, coords = c(1,2), crs = 4326) %>% st_transform(GOM_CRS)
pred_sf$month <- target_month

# --- PREDICTION ---
message("Running inlabru prediction (50 samples)...")
pred_formula <- as.formula(paste0("~ plogis(intercept_etag + ", 
                                  paste(env_vars, collapse = " + "), 
                                  " + etag_field)"))

inla_pred <- predict(model, newdata = pred_sf, formula = pred_formula, 
                     n.samples = 50, num.threads = 2)

# --- EXPORT TO GRID ---
r_out <- terra::rast(layers[[1]])
r_out <- terra::setValues(r_out, NA)

# Match projections (Transform UTM points to match Raster Template)
pred_points_fixed <- sf::st_transform(inla_pred, terra::crs(r_out))
coords <- sf::st_coordinates(pred_points_fixed)
cells  <- terra::cellFromXY(r_out, coords)

valid <- !is.na(cells)
r_out[cells[valid]] <- inla_pred$mean[valid]

# Save Files
writeRaster(r_out, file.path(OUTPUT_DIR, paste0("PRED_", date_str, "_leatherback.tif")), overwrite=TRUE)
writeRaster(r_out > 0.71, file.path(OUTPUT_DIR, paste0("CORE_", date_str, "_leatherback.tif")), overwrite=TRUE)

# --- VISUALIZATION ---
p <- ggplot() +
  geom_spatraster(data = r_out) +
  scale_fill_viridis_c(name = "Prob", option = "mako", limits = c(0, 1), na.value = "transparent") +
  labs(title = paste("Leatherback Forecast:", date_str)) +
  theme_minimal()

ggsave(file.path(OUTPUT_DIR, paste0("PLOT_", date_str, "_leatherback.png")), p, width=10, height=7)

# --- CLEANUP ---
# Remove heavy model files so they aren't stored in the GitHub Action runner
file.remove(MODEL_RDS)
file.remove(SCALING_RDS)
message("Success! Daily forecast generated and cleanup complete.")
