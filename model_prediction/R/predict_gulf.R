# Predict Gulf of Mexico - Full Raw Data Strategy

library(terra)
library(sf)
library(dplyr)
library(glue)
library(lubridate)

# ----------------------------------------------------------------
# DATE LOGIC
# ----------------------------------------------------------------
date_forecast <- Sys.Date() + 1
date_obs      <- Sys.Date() - 1

message(glue("Prediction Run for Forecast Date: {date_forecast}"))
message(glue("Using Observation Data from: {date_obs}"))

# Paths
raw_dir       <- "data_acquisition/netcdfs/cmems_ncdfs"
# We IGNORE processed_dir because it contains Pacific data.
# We will process Gulf data on the fly here.

# ----------------------------------------------------------------
# HELPER: Smart File Finder
# ----------------------------------------------------------------
find_file <- function(var_name, search_dir) {
  
  # Default to forecast date
  target_date <- date_forecast
  
  # Identify Observation variables (must match CSV 'model_var_name')
  obs_vars <- c("l.chl", "sla", "sst", "mld", "analysed_sst", "ugosa", "vgosa")
  
  if (var_name %in% obs_vars) {
    target_date <- date_obs
  }
  
  # Search Pattern: {product}_{model_var_name}_{date}
  # We search for the exact var_name followed by the date to avoid partial matches
  pattern <- glue("_{var_name}_{target_date}")
  
  files <- list.files(search_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) stop(glue("MISSING FILE: Could not find {var_name} for {target_date} in {search_dir}"))
  
  return(files[1])
}

# ----------------------------------------------------------------
# 1. LOAD RAW DATA (All Gulf Variables)
# ----------------------------------------------------------------
message("Loading Raw NetCDFs...")

# Surface Variables
# Note: In your metadata, 'thetao' (0m) serves as SST
r_sst <- rast(find_file("thetao", raw_dir))["thetao"] 
r_ssh <- rast(find_file("ssh", raw_dir))["zos"]       # 'zos' is the NetCDF var name for ssh
r_chl <- rast(find_file("l.chl", raw_dir))["CHL"]     # 'CHL' is the var name

# Physics for EKE Calculation
r_uo  <- rast(find_file("uo", raw_dir))["uo"]
r_vo  <- rast(find_file("vo", raw_dir))["vo"]

# Deep Variables
r_bottom_t   <- rast(find_file("bottom_t", raw_dir))["tob"] # 'tob' is bottom temp
r_thetao_150 <- rast(find_file("thetao_150m", raw_dir))["thetao"]
r_thetao_500 <- rast(find_file("thetao_500m", raw_dir))["thetao"]

# ----------------------------------------------------------------
# 2. PROCESS & ALIGN
# ----------------------------------------------------------------
message("Aligning Grids...")

# Define master grid (SST is usually a good reference)
master_grid <- r_sst

# Calculate EKE (Eddy Kinetic Energy) -> 0.5 * (u^2 + v^2)
message("Calculating EKE...")
r_eke <- 0.5 * (r_uo^2 + r_vo^2)
names(r_eke) <- "eke"

# Resample everything to master grid
# Use 'bilinear' for continuous data, 'near' for categorical (none here)
r_chl        <- resample(r_chl, master_grid, method = "bilinear")
r_eke        <- resample(r_eke, master_grid, method = "bilinear")
r_ssh        <- resample(r_ssh, master_grid, method = "bilinear")
r_bottom_t   <- resample(r_bottom_t, master_grid, method = "bilinear")
r_thetao_150 <- resample(r_thetao_150, master_grid, method = "bilinear")
r_thetao_500 <- resample(r_thetao_500, master_grid, method = "bilinear")

# Stack all layers
env_stack <- c(r_sst, r_chl, r_eke, r_ssh, r_bottom_t, r_thetao_150, r_thetao_500)
# Rename to match EXACTLY what your model expects
names(env_stack) <- c("sst", "chl", "eke", "ssh", "bottom_t", "thetao_150", "thetao_500")

# ----------------------------------------------------------------
# 3. PREDICT
# ----------------------------------------------------------------
message("Loading Model...")
# Ensure this path is correct in your repo
model <- readRDS("model_prediction/Gulf_Model_v1.rds") 

message("Running Prediction...")
# Convert to dataframe for prediction
# xy=TRUE keeps coordinates so we can map it back later
new_data <- as.data.frame(env_stack, xy = TRUE, na.rm = TRUE)

if (nrow(new_data) == 0) stop("Error: No data available for prediction (all pixels are NA). Check overlapping extents.")

# Run prediction
preds <- predict(model, new_data)

# Map back to raster
r_pred <- r_sst
values(r_pred) <- NA
r_pred[cellFromXY(r_pred, new_data[,c("x","y")])] <- preds
names(r_pred) <- "habitat_suitability"

# ----------------------------------------------------------------
# 4. SAVE
# ----------------------------------------------------------------
# Ensure directory exists
if (!dir.exists("app/data")) dir.create("app/data", recursive = TRUE)

outfile <- "app/data/latest_predictions.rds"
saveRDS(r_pred, outfile)
message(glue("Success! Predictions saved to {outfile}"))
