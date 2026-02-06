library(terra)
library(sf)
library(dplyr)
library(stringr)

# --- 1. SETTINGS & DATE LOGIC ---
MODEL_RDS   <- "model_prediction/gulf/results/lean_leatherback_model.rds"
SCALING_RDS <- "model_prediction/gulf/results/scaling_params_2026.02.05.rds"
GRID_DIR    <- "model_prediction/gulf/predictions"
OUTPUT_DIR  <- "model_prediction/gulf/predictions"

# [CRITICAL UPDATE] Target Tomorrow (Forecast Date)
date_forecast <- Sys.Date() + 1
date_str      <- as.character(date_forecast)

message(paste("Starting Leatherback Prediction for Forecast Date:", date_str))

# --- 2. HELPER: SMART FILE FINDER ---
find_best_file <- function(all_files, var_suffix, target_date) {
  # 1. Try for the exact FORECAST date (e.g., PRED_2026-02-07_sst.tif)
  # This matches the output format from your predict_gulf.R script
  pattern <- paste0("PRED_", target_date, "_", var_suffix, ".tif$")
  match <- all_files[str_detect(all_files, pattern)]
  
  if (length(match) > 0) return(match[1])
  
  # 2. Fallback: Find the most recent file available
  # This handles cases where a specific var might be missing for tomorrow
  message(paste("!!! Exact forecast file missing for", var_suffix, "- seeking most recent..."))
  
  # Search for any file ending in _{suffix}.tif
  all_var_files <- all_files[str_detect(all_files, paste0("_", var_suffix, ".tif$"))]
  
  if (length(all_var_files) == 0) return(NULL)
  
  # Sort alphabetically (YYYY-MM-DD sorts correctly) and take the last one
  sorted_files <- sort(all_var_files)
  best_fallback <- tail(sorted_files, 1)
  
  message(paste(" -> Using fallback:", basename(best_fallback)))
  return(best_fallback)
}

# Load Lean Model & Scaling
if (!file.exists(MODEL_RDS)) stop("Lean model RDS not found!")
m <- readRDS(MODEL_RDS)
scaling_params <- readRDS(SCALING_RDS)

all_files <- list.files(GRID_DIR, full.names = TRUE)

# --- 3. LOAD & SCALE ENVIRONMENTAL DATA ---
env_vars <- c("thetao", "zos", "z", "l.chl", "l.tke_mean")
name_map <- list(
  "thetao"     = "sst", 
  "zos"        = "ssh", 
  "z"          = "bathymetry", 
  "l.chl"      = "chlorophyll", 
  "l.tke_mean" = "tke"
)

layers <- list()
for (v in env_vars) {
  suffix <- name_map[[v]]
  target_path <- find_best_file(all_files, suffix, date_str)
  
  if (is.null(target_path)) stop(paste("No files found at all for:", suffix))
  
  r <- terra::rast(target_path)
  
  # Log transform for bio vars (Chlorophyll and TKE)
  if (suffix %in% c("chlorophyll", "tke")) {
    # Adding small constant to avoid log(0)
    r <- log(r + 0.001)
  }
  
  names(r) <- v
  layers[[v]] <- r
}

# Stack and Align
predictor_stack <- terra::rast(layers)

# Handle potential grid mismatches if fallback files were used
if (length(unique(as.character(terra::ext(predictor_stack)))) > 1) {
  message("Aligning grids due to fallback file usage...")
  predictor_stack <- terra::resample(predictor_stack, layers[[1]])
}

pred_df <- as.data.frame(predictor_stack, xy = TRUE, na.rm = TRUE)
colnames(pred_df)[1:2] <- c("lon", "lat")

# Apply Scaling (Z-Score)
for (v in env_vars) {
  # specific check for your RDS naming quirk
  mu_name <- if(v == "l.tke_mean") "l.tke_mean_mean" else paste0(v, "_mean")
  sd_name <- if(v == "l.tke_mean") "l.tke_mean_sd"   else paste0(v, "_sd")
  
  pred_df[[v]] <- (pred_df[[v]] - scaling_params[[mu_name]]) / scaling_params[[sd_name]]
}

# --- 4. MANUAL PREDICTION ENGINE ---
message("Running Manual Prediction Engine...")

# A. Covariate Effects
get_val <- function(var_name, grid_vals) {
  tab <- m$smooths[[var_name]]
  idx <- findInterval(grid_vals, tab$ID)
  idx[idx == 0] <- 1
  return(as.numeric(tab$mean[idx]))
}

lp_covs <- m$intercept + 
           get_val("thetao", pred_df$thetao) + 
           get_val("zos", pred_df$zos) +
           get_val("z", pred_df$z) + 
           get_val("l.chl", pred_df$l.chl) +
           get_val("l.tke_mean", pred_df$l.tke_mean)

# B. Spatial Field (Nearest Neighbor Mapping)
# We project the grid points to the model's CRS (Zone 16N) to find the nearest mesh node

grid_sf <- st_as_sf(pred_df, coords = c("lon", "lat"), crs = 4326) %>% st_transform(32616)
mesh_sf <- st_as_sf(m$spatial$mesh_locs, coords = c("X", "Y"), crs = 32616)
nn_idx  <- st_nearest_feature(grid_sf, mesh_sf)
lp_spatial <- m$spatial$values[nn_idx]

# C. Final Probability
final_prob <- 1 / (1 + exp(-(lp_covs + lp_spatial)))

# --- 5. EXPORT RESULTS ---
# Build output raster from template
r_out <- predictor_stack[[1]]
values(r_out) <- NA
cells <- cellFromXY(r_out, as.matrix(pred_df[,c("lon", "lat")]))
r_out[cells] <- final_prob

# Save Continuous Probability
# Note: Using date_str (Tomorrow's Date) in the filename
out_prob <- file.path(OUTPUT_DIR, paste0("PRED_", date_str, "_leatherback.tif"))
writeRaster(r_out, out_prob, overwrite=TRUE)

# Save Binary Core Habitat Mask (>0.71)

out_core <- file.path(OUTPUT_DIR, paste0("CORE_", date_str, "_leatherback.tif"))
writeRaster(r_out > 0.71, out_core, overwrite=TRUE)

message(paste("Success! Generated Leatherback Forecast for:", date_str))
