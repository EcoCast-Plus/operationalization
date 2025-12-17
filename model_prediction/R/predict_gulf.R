# Predict Gulf of Mexico - Hybrid Loading Strategy

library(terra)
library(sf)
library(dplyr)
library(glue)
library(lubridate)

# ----------------------------------------------------------------
# DATE LOGIC (Must match acquire_cmems.R)
# ----------------------------------------------------------------
date_forecast <- Sys.Date() + 1
date_obs      <- Sys.Date() - 1

message(glue("Prediction Run for Forecast Date: {date_forecast}"))
message(glue("Using Observation Data from: {date_obs}"))

# Paths
processed_dir <- "data_processing/TopPredatorWatch/rasters"
raw_dir       <- "data_acquisition/netcdfs/cmems_ncdfs"
static_dir    <- "app/data"  # For climatology files if needed locally

# ----------------------------------------------------------------
# HELPER: Smart File Finder
# ----------------------------------------------------------------
# Looks for the variable name in the filename.
# Automatically detects if it should look for the Forecast Date or Obs Date.
find_file <- function(var_name, search_dir) {
  
  # Default to forecast date
  target_date <- date_forecast
  
  # If we know certain vars are ALWAYS observations, force that date
  # Adjust this list based on your 'obs' products in metadata
  obs_vars <- c("l.chl", "sla", "sst", "mld", "analysed_sst", "ugosa", "vgosa")
  
  if (var_name %in% obs_vars) {
    target_date <- date_obs
  }
  
  # Search pattern: Variable Name + Date
  pattern <- glue("{var_name}_{target_date}")
  
  files <- list.files(search_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    # Fallback: Try fuzzy search just by variable name if date match fails
    # This helps if 'sst' was processed as 'sst_2025...' but we missed the exact string
    files <- list.files(search_dir, pattern = var_name, full.names = TRUE)
    # Filter for the date manually
    files <- files[grepl(as.character(target_date), files)]
  }
  
  if (length(files) == 0) stop(glue("Could not find file for {var_name} on {target_date} in {search_dir}"))
  
  return(files[1])
}

# ----------------------------------------------------------------
# 1. LOAD DATA
# ----------------------------------------------------------------

# A. Load Processed Surface Variables (TIFFs)
# These come from process_cmems.R
r_chl <- rast(find_file("l.chl", processed_dir))
r_eke <- rast(find_file("eke", processed_dir))
r_ssh <- rast(find_file("ssh", processed_dir))
r_sst <- rast(find_file("sst", processed_dir)) # Note: verify if this is 'sst' or 'analysed_sst'

# B. Load Raw Deep Variables (NetCDFs)
# These were skipped by process_cmems.R and must be read raw
# Note: 'bottom_t' and 'thetao_150m' are likely Forecast models -> Tomorrow's date
r_bottom_t   <- rast(find_file("bottom_t", raw_dir))["tob"] # Select 'tob' layer if multi-layer
r_thetao_150 <- rast(find_file("thetao_150m", raw_dir))["thetao"]
r_thetao_500 <- rast(find_file("thetao_500m", raw_dir))["thetao"]

# ----------------------------------------------------------------
# 2. ALIGN GRIDS
# ----------------------------------------------------------------
# Define the master grid (using SST as reference)
master_grid <- r_sst

# Resample everything to match SST
r_chl        <- resample(r_chl, master_grid, method = "near")
r_eke        <- resample(r_eke, master_grid, method = "bilinear")
r_ssh        <- resample(r_ssh, master_grid, method = "bilinear")
r_bottom_t   <- resample(r_bottom_t, master_grid, method = "bilinear")
r_thetao_150 <- resample(r_thetao_150, master_grid, method = "bilinear")
r_thetao_500 <- resample(r_thetao_500, master_grid, method = "bilinear")

# Stack them
env_stack <- c(r_sst, r_chl, r_eke, r_ssh, r_bottom_t, r_thetao_150, r_thetao_500)
names(env_stack) <- c("sst", "chl", "eke", "ssh", "bottom_t", "thetao_150", "thetao_500")

# ----------------------------------------------------------------
# 3. PREDICT
# ----------------------------------------------------------------
message("Loading Model...")
model <- readRDS("model_prediction/Gulf_Model_v1.rds") # Update path if needed

message("Running Prediction...")
# Convert raster to dataframe for prediction (safest for complex models)
new_data <- as.data.frame(env_stack, xy = TRUE, na.rm = TRUE)

# Ensure column names match what the model expects
# (You might need to rename columns here if your model uses specific names)

preds <- predict(model, new_data)

# Map back to raster
r_pred <- r_sst
values(r_pred) <- NA
# Fill in values at the coordinates we predicted
# (Simplified: assumes row order preserved. Use cell numbers for robustness if needed)
r_pred[cellFromXY(r_pred, new_data[,c("x","y")])] <- preds

# ----------------------------------------------------------------
# 4. SAVE
# ----------------------------------------------------------------
outfile <- "app/data/latest_predictions.rds"
saveRDS(r_pred, outfile)
message(glue("Predictions saved to {outfile}"))
