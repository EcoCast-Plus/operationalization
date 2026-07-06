# Predict Manta Ray (GAM) - Unified Gulf & South Atlantic
# Optimized for pre-masked static layers and bathymetry-based land masking

# --- 1. Load Libraries ---
library(terra)
library(sf)
library(dplyr)
library(glue)
library(lubridate)
library(bundle)
library(mgcv)          # Required for Manta Ray (GAM) prediction
library(grec)          # Required for SST Fronts
library(raster)        # Required for grec compatibility

# --- 2. Define Directories ---
static_dir <- "model_prediction/gulf/data/manta"
preds_dir  <- "model_prediction/gulf/predictions"
model_path <- "model_prediction/gulf/results/MANTA_RAY_final_ensemble.rds"
raw_dir <- "data_acquisition/netcdfs/cmems_ncdfs"

# --- 3. Date Logic ---
date_forecast <- Sys.Date() + 1
date_obs      <- Sys.Date() - 1

message(glue("Prediction Run for Forecast Date: {date_forecast}"))

# ----------------------------------------------------------------
# HELPER: Smart File Loader with "Look-Back" Logic
# ----------------------------------------------------------------
load_raw <- function(var_name, nc_var) {
  pattern_general <- glue("_{var_name}_\\d{{4}}-\\d{{2}}-\\d{{2}}")
  files_all <- list.files(raw_dir, pattern = pattern_general, full.names = TRUE)
  
  if (length(files_all) == 0) {
    stop(glue("CRITICAL ERROR: No files found for variable '{var_name}' in {raw_dir}"))
  }
  
  # Sort descending (newest dates first)
  files_sorted <- sort(files_all, decreasing = TRUE)
  
  # Loop through files from newest to oldest until we find valid data
  for (f in files_sorted) {
    r <- rast(f)[nc_var][[1]]
    
    # Calculate raster statistics
    stats <- global(r, fun = c("min", "max", "notNA"), na.rm = TRUE)
    
    is_empty     <- stats$notNA == 0
    is_all_zeros <- (stats$notNA > 0) && (stats$min == 0) && (stats$max == 0)
    
    if (!is_empty && !is_all_zeros) {
      if (ext(r)$xmax > 180) r <- rotate(r)
      
      if (f != files_sorted[1]) {
        message(glue(" -> NOTICE: Latest '{var_name}' was empty/zero. Used fallback: {basename(f)}"))
      }
      return(r)
    } else {
      message(glue(" -> Skipping {basename(f)} (Data is 100% NA or 0). Looking for older data..."))
    }
  }
  
  # If every file is empty, return 0s as absolute last resort
  message(glue("CRITICAL WARNING: All available files for {var_name} are empty! Returning 0s as absolute last resort."))
  r <- rast(files_sorted[1])[nc_var][[1]]
  if (ext(r)$xmax > 180) r <- rotate(r)
  r <- r * 0 
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


# --- B. Load Dynamic Data & Process Gaps ---
message("Loading and Processing Dynamic Environmental Data...")

# 1. SST (thetao)
r_sst_full <- load_raw("thetao", "thetao")

# Crop to the general bounding box to save memory, but DO NOT mask it yet
r_sst_cropped <- terra::crop(r_sst_full, master_grid, snap = "out")
if (!compareGeom(r_sst_cropped, master_grid, stopOnError = FALSE)) {
  r_sst_cropped <- resample(r_sst_cropped, master_grid, method = "bilinear")
}

# Extrapolate SST into the nearshore/land gaps to act as a buffer for the front calculation
message(" -> Extrapolating SST to prevent edge-effect clipping...")
r_sst_filled <- terra::focal(r_sst_cropped, w = 5, fun = mean, na.rm = TRUE, na.policy = "only")
r_sst_filled <- terra::focal(r_sst_filled, w = 5, fun = mean, na.rm = TRUE, na.policy = "only")

# 2. SST Fronts (Front_Z)
message(" -> Detecting SST Fronts (BelkinOReilly2009)...")
r_sst_raster <- raster::raster(r_sst_filled) 
r_fronts_raw <- grec::detectFronts(r_sst_raster, method = "BelkinOReilly2009", intermediate = FALSE)
r_fronts <- rast(r_fronts_raw)

# Normalize the fronts
max_val <- global(r_fronts, "max", na.rm = TRUE)$max
if (is.na(max_val) || max_val == 0) {
  r_fronts <- master_grid * 0
} else {
  r_fronts <- r_fronts / max_val
}

# 3. Apply Final Mask to SST and Fronts
r_sst    <- terra::mask(r_sst_filled, master_grid)
names(r_sst) <- "SST"

r_fronts <- terra::mask(r_fronts, master_grid)
names(r_fronts) <- "Front_Z"

# 4. Chlorophyll (l.chl)
r_chl_full <- load_raw("l.chl", "chl")

# Crop Chl to grid
r_chl_cropped <- terra::crop(r_chl_full, master_grid, snap = "out")
if (!compareGeom(r_chl_cropped, master_grid, stopOnError = FALSE)) {
  r_chl_cropped <- resample(r_chl_cropped, master_grid, method = "bilinear")
}

# Fill any nearshore gaps in Chlorophyll, then mask
message(" -> Extrapolating ChlA nearshore gaps...")
r_chl_filled <- terra::focal(r_chl_cropped, w = 3, fun = mean, na.rm = TRUE, na.policy = "only")
r_chl <- terra::mask(r_chl_filled, master_grid)
names(r_chl) <- "ChlA"


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

# --- NEW SAFETY CHECK: Prevent Empty Dataframes ---
message("Checking for 100% NA layers before creating dataframe...")

for (i in 1:nlyr(manta_stack)) {
  if (all(is.na(values(manta_stack[[i]])))) {
    layer_name <- names(manta_stack)[i]
    message(glue("  -> WARNING: Layer '{layer_name}' is 100% NA! Filling with 0 to save prediction frame."))
    
    r_zero <- master_grid * 0
    names(r_zero) <- layer_name
    manta_stack[[i]] <- r_zero
  }
}

# Because the static layers were pre-masked, na.rm = TRUE drops everything outside the footprint
pred_df <- as.data.frame(manta_stack, xy = TRUE, na.rm = TRUE)

# --- NEW BATHYMETRY MASKING LOGIC ---
# Drop any pixels where elevation is > 0 (Land)
pred_df <- pred_df[pred_df$Depth_m <= 0, ] 

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
  
  # Save the Final Prediction TIF (Added _PRED to make it distinct)
  save_name <- glue("PRED_{date_forecast}_MANTA_RAY_PRED.tif")
  save_path <- file.path(preds_dir, save_name)
  writeRaster(r_out, save_path, overwrite = TRUE)
  message(glue(" -> SUCCESS: Saved to {save_path}"))
  
}, error = function(e) {
  stop(glue(" -> ERROR during prediction: {e$message}"))
})

# --- E. Export Environmental Layers for Manta Viewer ---
message("Exporting Environmental Layers for Viewer...")

# Naming these with MANTA_RAY ensures the GulfCast app ignores them automatically
env_export <- list(
  "MANTA_RAY_SST" = r_sst,
  "MANTA_RAY_CHL" = r_chl,
  "MANTA_RAY_FRONTS" = r_fronts,
  "MANTA_RAY_BATHYMETRY" = r_depth,
  "MANTA_RAY_SLOPE" = r_slope
)

for (var_name in names(env_export)) {
  save_name_env <- glue("PRED_{date_forecast}_{var_name}.tif")
  save_path_env <- file.path(preds_dir, save_name_env)
  writeRaster(env_export[[var_name]], save_path_env, overwrite = TRUE)
  message(glue(" -> Exported Env: {save_name_env}"))
}

message("\nAll Manta Ray predictions and environmental exports completed successfully!")
