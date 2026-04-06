# Predict Manta Ray (GAM) - Unified Gulf & South Atlantic
# Optimized for pre-masked static layers and a single combined bounding box

# --- 1. Load Libraries ---
library(terra)
library(sf)
library(dplyr)
library(glue)
library(lubridate)
library(bundle)
library(mgcv)        # Required for Manta Ray (GAM) prediction
library(grec)        # Required for SST Fronts
library(raster)      # Required for grec compatibility

# --- 2. Define Directories ---
# Update these paths to match your new unified repo structure
static_dir <- "model_prediction/gulf/data/manta"
preds_dir  <- "model_prediction/gulf/predictions"
model_path <- "model_prediction/gulf/results/MANTA_RAY_final_ensemble.rds"

# Raw NetCDF download directory
raw_dir <- "data_acquisition/netcdfs/cmems_ncdfs"

# --- 3. Date Logic ---
date_forecast <- Sys.Date() + 1
date_obs      <- Sys.Date() - 1

message(glue("Prediction Run for Forecast Date: {date_forecast}"))

# ----------------------------------------------------------------
# HELPER: Smart File Finder 
# ----------------------------------------------------------------
find_file <- function(var_name, search_dir) {
  target_date <- date_forecast
  
  # Observation variables (Chl, SST) use yesterday's date
  if (var_name %in% c("l.chl")) target_date <- date_obs 
  
  pattern_exact <- glue("_{var_name}_{target_date}")
  files_exact <- list.files(search_dir, pattern = pattern_exact, full.names = TRUE)
  
  if (length(files_exact) > 0) return(files_exact[1])
  
  message(glue("NOTICE: Exact file missing for {var_name} on {target_date}. Searching for most recent..."))
  pattern_general <- glue("_{var_name}_\\d{{4}}-\\d{{2}}-\\d{{2}}")
  files_all <- list.files(search_dir, pattern = pattern_general, full.names = TRUE)
  
  if (length(files_all) == 0) stop(glue("CRITICAL ERROR: No files found for '{var_name}' in {search_dir}"))
  
  files_sorted <- sort(files_all, decreasing = TRUE)
  best_file <- files_sorted[1]
  
  message(glue(" -> Found substitute: {basename(best_file)}"))
  return(best_file)
}

# ----------------------------------------------------------------
# HELPER: Layer Validator & Cropper
# ----------------------------------------------------------------
validate_layer <- function(r, name, master_grid) {
  # Crop the large CMEMS extent down to match the bathymetry bounding box
  r <- terra::crop(r, master_grid, snap = "out")
  
  if (!compareGeom(r, master_grid, stopOnError = FALSE)) {
    r <- resample(r, master_grid, method = "bilinear")
  }
  if (all(is.na(values(r)))) {
    message(glue("WARNING: Layer '{name}' is 100% NA. Filling with 0."))
    r <- master_grid * 0
  }
  names(r) <- name
  
  # Apply the master grid's NA mask so we only keep data inside the survey footprint
  r <- terra::mask(r, master_grid)
  
  return(r)
}

# ----------------------------------------------------------------
# 4. MAIN PROCESSING
# ----------------------------------------------------------------

message(glue("\n=================================================="))
message(glue("STARTING MANTA RAY PREDICTIONS (UNIFIED EXTENT)"))
message(glue("=================================================="))

# --- A. Load Static Data (Master Grid) ---
message("Loading Static Variables & Establishing Master Grid...")
bathy_path <- file.path(static_dir, "bathymetry.tif")
slope_path <- file.path(static_dir, "slope_deg.tif")

if(!file.exists(bathy_path) || !file.exists(slope_path)) {
  stop(glue("CRITICAL ERROR: Static .tif files missing in {static_dir}. Cannot proceed."))
}

# 1. Load Pre-Masked Bathymetry
master_grid <- rast(bathy_path)
r_depth <- master_grid
names(r_depth) <- "Depth_m"

# 2. Load Pre-Calculated Slope
r_slope <- rast(slope_path)
names(r_slope) <- "Slope_deg"

# 3. Striparea Offset Constant
r_striparea <- master_grid * 0 + 1876712
names(r_striparea) <- "striparea"

# --- B. Load Dynamic Data & Crop to Master Grid ---
message("Loading and Cropping Dynamic Environmental Data...")

# 1. SST (thetao)
f_sst <- find_file("thetao", raw_dir)
r_sst_full <- rast(f_sst)["thetao"][[1]]
if (ext(r_sst_full)$xmax > 180) r_sst_full <- rotate(r_sst_full)
r_sst <- validate_layer(r_sst_full, "SST", master_grid)

# 2. Chlorophyll (l.chl)
f_chl <- find_file("l.chl", raw_dir)
r_chl_full <- rast(f_chl)["CHL"][[1]]
if (ext(r_chl_full)$xmax > 180) r_chl_full <- rotate(r_chl_full)
r_chl <- validate_layer(r_chl_full, "ChlA", master_grid)

# 3. SST Fronts (Front_Z)
message(" -> Detecting SST Fronts (BelkinOReilly2009)...")
r_sst_raster <- raster::raster(r_sst) 
r_fronts_raw <- grec::detectFronts(r_sst_raster, method = "BelkinOReilly2009", intermediate = FALSE)
r_fronts <- rast(r_fronts_raw)

max_val <- global(r_fronts, "max", na.rm = TRUE)$max
if (is.na(max_val) || max_val == 0) {
  r_fronts <- master_grid * 0
} else {
  r_fronts <- r_fronts / max_val
}
r_fronts <- validate_layer(r_fronts, "Front_Z", master_grid)

# --- C. Stack & Format Dataframe ---
message("Formatting Prediction Stack...")

manta_stack <- c(
  r_sst,
  r_chl,
  r_fronts,
  r_depth,
  r_slope,
  r_striparea
)

# Because the static layers were pre-masked, na.rm = TRUE drops everything outside the footprint
pred_df <- as.data.frame(manta_stack, xy = TRUE, na.rm = TRUE)

if(nrow(pred_df) == 0) {
  stop("CRITICAL ERROR: Empty prediction frame. Check if your bathymetry mask aligns with CMEMS data.")
}

# --- D. Predict & Export ---
if (!file.exists(model_path)) {
  stop(glue("CRITICAL ERROR: Model not found at {model_path}."))
}

message(glue("Predicting Manta Rays (Points: {nrow(pred_df)})..."))

tryCatch({
  model_bundled <- readRDS(model_path)
  model_obj     <- bundle::unbundle(model_bundled)
  
  # Generate Predictions
  preds <- predict(model_obj, newdata = pred_df, type = "response")
  preds <- as.numeric(preds)
  
  # Rasterize Predictions
  r_out <- master_grid
  values(r_out) <- NA
  r_out[cellFromXY(r_out, pred_df[, c("x", "y")])] <- preds
  names(r_out) <- "MANTA_RAY_PRED"
  
  # Save the Final TIF
  save_name <- glue("PRED_{date_forecast}_MANTA_RAY.tif")
  save_path <- file.path(preds_dir, save_name)
  
  writeRaster(r_out, save_path, overwrite = TRUE)
  message(glue(" -> SUCCESS: Saved to {save_path}"))
  
}, error = function(e) {
  stop(glue(" -> ERROR during prediction: {e$message}"))
})

message("\nAll Manta Ray predictions completed successfully!")



