# ==============================================================================
# PREDICT LEATHERBACK GULF - Final Marginal Effects + Spatial Baseline
# ==============================================================================

library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyterra)
library(purrr)
library(glue)

sf::sf_use_s2(FALSE)

# --- 1. SETTINGS ---
GRID_DIR   <- "model_prediction/gulf/predictions"
OUTPUT_DIR <- "model_prediction/gulf/predictions"
local_dir  <- "model_prediction/gulf/results"
if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

date_str     <- as.character(Sys.Date() + 1)
target_month <- as.numeric(format(Sys.Date() + 1, "%m"))

# --- 2. DOWNLOAD MARGINAL EFFECTS PAYLOAD FROM GITHUB ---
message("Fetching marginal effects payload from GitHub...")
marg_eff_path <- file.path(local_dir, "iSDM_marg_effs_for_Sarah.rds")

download.file(
  "https://raw.githubusercontent.com/EcoCast-Plus/operationalization/main/model_prediction/gulf/results/iSDM_marg_effs_for_Sarah.rds",
  destfile = marg_eff_path,
  mode = "wb",
  quiet = TRUE
)

post_marg_eff <- readRDS(marg_eff_path)

# --- 3. INTERPOLATION HELPER ---
interpolate_samples <- function(new_x_values, lookup_element) {
  n_obs   <- length(new_x_values)
  n_samps <- ncol(lookup_element$y_samps)
  out_matrix <- matrix(0, nrow = n_obs, ncol = n_samps)
  for (i in seq_len(n_samps)) {
    interp <- approx(
      x    = lookup_element$x,
      y    = lookup_element$y_samps[, i],
      xout = new_x_values,
      rule = 2
    )
    out_matrix[, i] <- interp$y
  }
  return(out_matrix)
}

# --- 4. LOAD ENVIRONMENTAL RASTERS ---
message("Loading environmental rasters...")
all_files <- list.files(GRID_DIR, full.names = TRUE)

find_tif <- function(suffix) {
  hit <- all_files[str_detect(all_files, paste0("PRED_", date_str, "_", suffix, "\\.tif$"))]
  if (length(hit) == 0 || is.na(hit[1]))
    hit <- tail(sort(all_files[str_detect(all_files, paste0("_", suffix, "\\.tif$"))]), 1)
  if (length(hit) == 0) stop(glue("Could not find .tif for suffix: {suffix}"))
  terra::rast(hit[1])
}

# Payload covariate names -> .tif suffixes produced by predict_gulf.R
covar_to_tif <- list(
  thetao_s     = "sst",
  zos_s        = "ssh",
  z_s          = "bathymetry",
  l.chl_s      = "chlorophyll",
  l.tke_mean_s = "tke",
  so_s         = "salinity"
)

covars_list <- map(covar_to_tif, find_tif)

# Log-transform covariates that were log-scaled at model training time
covars_list[["l.chl_s"]]      <- log(covars_list[["l.chl_s"]]      + 0.001)
covars_list[["l.tke_mean_s"]] <- log(covars_list[["l.tke_mean_s"]] + 0.001)

# --- 5. PREDICT VIA RUNNING SUM (Environmental Only) ---
message("Generating environmental prediction via marginal effects lookup...")
running_sum <- NULL

for (j in seq_along(covars_list)) {
  cov_name     <- names(covars_list)[j]
  current_rast <- covars_list[[j]]
  current_marg <- post_marg_eff[[cov_name]]
  
  tmp <- as.vector(values(current_rast[[1]]))
  
  interpolated_vals <- interpolate_samples(tmp, current_marg)
  
  running_sum <- if (is.null(running_sum)) interpolated_vals else running_sum + interpolated_vals
  
  rm(tmp, interpolated_vals)
  gc()
  message(glue("  Done: {cov_name}"))
}

# Average across the posterior samples to get the mean linear predictor
mean_pred_env <- rowMeans(running_sum)
rm(running_sum); gc()

# --- 5.5 ADD SPATIAL FIELD & INTERCEPT ---
message(glue("Adding spatial field and intercept for Month: {target_month}..."))

# Load Mitch's 12-band spatial TIF
spatial_tif_path <- file.path(local_dir, "etag_spatial_baseline_12months.tif")
if (!file.exists(spatial_tif_path)) stop("CRITICAL: Spatial baseline TIF not found in results folder!")

r_spatial_stack <- terra::rast(spatial_tif_path)

# Pull out the specific layer for tomorrow's forecast month
r_spatial_current <- r_spatial_stack[[target_month]]

# Safety check: Ensure geometries perfectly match the environmental template
r_template <- covars_list[["thetao_s"]][[1]]
if (!terra::compareGeom(r_spatial_current, r_template, stopOnError = FALSE)) {
  message("  -> Resampling spatial baseline to match environmental grid...")
  r_spatial_current <- terra::resample(r_spatial_current, r_template, method = "bilinear")
}

# Extract spatial baseline values
spatial_baseline_vals <- as.vector(values(r_spatial_current))

# Combine environmental and spatial components
final_linear_predictor <- mean_pred_env + spatial_baseline_vals


# --- 6. BACK TO RASTER & EXPORT ---
message("Applying logit transform and writing outputs...")

# 1. Apply probabilities directly on the vector (safer and faster than terra::app in GitHub Actions)
final_probabilities <- plogis(final_linear_predictor)

# 2. Map the final probabilities back to the grid
r_prob <- terra::setValues(r_template, final_probabilities)
names(r_prob) <- "leatherback_probability"

# Export Main Prediction
message("Saving PRED tif...")
writeRaster(r_prob,
            file.path(OUTPUT_DIR, paste0("PRED_", date_str, "_leatherback.tif")),
            overwrite = TRUE)

# 3. Create Core Area and explicitly convert TRUE/FALSE to 1/0 for safe GDAL writing
message("Saving CORE tif...")
r_core <- r_prob > 0.71
r_core <- terra::as.numeric(r_core) 
names(r_core) <- "core_habitat"

writeRaster(r_core,
            file.path(OUTPUT_DIR, paste0("CORE_", date_str, "_leatherback.tif")),
            overwrite = TRUE)

# 4. Generate Plot with explicit white background
message("Saving PLOT png...")
p <- ggplot() +
  geom_spatraster(data = r_prob) +
  scale_fill_viridis_c(option = "mako", na.value = "transparent", limits = c(0, 1)) +
  theme_minimal() +
  labs(title = paste("Leatherback Prediction:", date_str),
       fill = "Probability")

ggsave(file.path(OUTPUT_DIR, paste0("PLOT_", date_str, "_leatherback.png")),
       plot = p, width = 10, height = 7, bg = "white") 

# --- 7. CLEANUP ---
file.remove(marg_eff_path)
message("Process complete.")
