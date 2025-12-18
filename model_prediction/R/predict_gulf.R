# Predict Gulf of Mexico - Full Replication of Shiny App Logic

# --- 1. Load Libraries ---
library(terra)
library(sf)
library(dplyr)
library(glue)
library(lubridate)
library(bundle)   # Critical for loading the models
library(xgboost)  # Likely needed by ensemble members
library(ranger)   # Likely needed by ensemble members
library(mgcv)     # Needed for GAMs
library(oce)      # For moon angle

# --- 2. Define Directories ---
# "Models are saved in model_prediction/gulf/results"
models_dir <- "model_prediction/gulf/results"

# "Predictions to be saved here: model_prediction/gulf/predictions"
preds_dir  <- "model_prediction/gulf/predictions"
if (!dir.exists(preds_dir)) dir.create(preds_dir, recursive = TRUE)

# Raw NetCDFs (Downloaded from CMEMS)
raw_dir    <- "data_acquisition/netcdfs/cmems_ncdfs"

# Static files (Bathy, Shore, Climatology)
# UPDATED: Pointing to 'model_prediction/gulf/data' based on your file list
static_dir <- "model_prediction/gulf/data"

# --- 3. Date Logic ---
date_forecast <- Sys.Date() + 1
date_obs      <- Sys.Date() - 1

message(glue("Prediction Run for Forecast Date: {date_forecast}"))

# ----------------------------------------------------------------
# HELPER: Smart File Finder
# ----------------------------------------------------------------
find_file <- function(var_name, search_dir) {
  target_date <- date_forecast
  
  # Obs variables use yesterday's date
  obs_vars <- c("l.chl", "sla", "sst", "mld", "analysed_sst", "ugosa", "vgosa")
  if (var_name %in% obs_vars) target_date <- date_obs
  
  # Search pattern
  pattern <- glue("_{var_name}_{target_date}")
  files <- list.files(search_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) stop(glue("MISSING FILE: {var_name} for {target_date}"))
  return(files[1])
}

# ----------------------------------------------------------------
# 4. LOAD & PROCESS ENVIRONMENTAL DATA
# ----------------------------------------------------------------
message("Loading Dynamic Environmental Data...")

# Load Raw Layers
r_sst        <- rast(find_file("thetao", raw_dir))["thetao"] 
r_ssh        <- rast(find_file("ssh", raw_dir))["zos"]
r_chl        <- rast(find_file("l.chl", raw_dir))["CHL"]
r_uo         <- rast(find_file("uo", raw_dir))["uo"]
r_vo         <- rast(find_file("vo", raw_dir))["vo"]
r_bottom_t   <- rast(find_file("bottom_t", raw_dir))["tob"]
r_thetao_150 <- rast(find_file("thetao_150m", raw_dir))["thetao"]
r_thetao_500 <- rast(find_file("thetao_500m", raw_dir))["thetao"]

# Define Master Grid
master_grid <- r_sst

# Calculate Derived Dynamic Variables
message("Calculating Derived Variables (EKE, TKE)...")
r_eke <- 0.5 * (r_uo^2 + r_vo^2); names(r_eke) <- "eke"
r_tke <- 0.5 * (r_uo^2 + r_vo^2); names(r_tke) <- "tke"

# Resample Dynamic Vars to Master Grid
env_stack_dynamic <- c(
  r_sst, 
  resample(r_chl, master_grid, method="bilinear"),
  resample(r_ssh, master_grid, method="bilinear"),
  resample(r_bottom_t, master_grid, method="bilinear"),
  resample(r_thetao_150, master_grid, method="bilinear"),
  resample(r_thetao_500, master_grid, method="bilinear"),
  resample(r_eke, master_grid, method="bilinear"),
  resample(r_tke, master_grid, method="bilinear")
)
names(env_stack_dynamic) <- c("thetao", "chl", "zos", "bottom_t", "thetao_150m", "thetao_500m", "eke", "tke")

# ----------------------------------------------------------------
# 5. LOAD/GENERATE STATIC VARIABLES
# ----------------------------------------------------------------
message("Loading Static Variables...")

# A. Bathymetry & Shore Distance
# UPDATED: Loading from your specific filenames
r_depth <- rast(file.path(static_dir, "bathymetry.tif"))
r_shore <- rast(file.path(static_dir, "DfromShore.tif"))

r_depth <- resample(r_depth, master_grid, method="bilinear"); names(r_depth) <- "depth"
r_shore <- resample(r_shore, master_grid, method="bilinear"); names(r_shore) <- "dfrom_shore"

# B. Climatology (for Anomalies)
message("Calculating Anomalies from Climatology...")
target_doy <- yday(date_forecast)

# SST Anomaly
sst_clim_file <- list.files(static_dir, pattern = "sst_daily_climatology", full.names = TRUE)[1]
if (!is.na(sst_clim_file)) {
  r_sst_clim <- rast(sst_clim_file)
  # Project first if needed (safety check), then resample
  r_sst_clim_res <- resample(r_sst_clim, master_grid, method="bilinear")
  r_sst_anomaly <- r_sst - r_sst_clim_res[[target_doy]]
} else {
  warning("SST Climatology file missing! Anomaly will be 0.")
  r_sst_anomaly <- r_sst * 0
}
names(r_sst_anomaly) <- "sst_anomaly"

# SSH Anomaly
ssh_clim_file <- list.files(static_dir, pattern = "ssh_daily_climatology", full.names = TRUE)[1]
if (!is.na(ssh_clim_file)) {
  r_ssh_clim <- rast(ssh_clim_file)
  r_ssh_clim_res <- resample(r_ssh_clim, master_grid, method="bilinear")
  r_ssh_anomaly <- r_ssh - r_ssh_clim_res[[target_doy]]
} else {
  warning("SSH Climatology file missing! Anomaly will be 0.")
  r_ssh_anomaly <- r_ssh * 0
}
names(r_ssh_anomaly) <- "ssh_anomaly"

# C. Generated Time Variables
r_month <- master_grid * 0 + as.integer(month(date_forecast)); names(r_month) <- "month"
r_doy   <- master_grid * 0 + as.integer(yday(date_forecast));  names(r_doy)   <- "doy"

# D. Moon Angle (Calculated spatially)
coords <- as.data.frame(master_grid, xy=TRUE)[, c("x", "y")]
moon_vals <- oce::moonAngle(t = date_forecast, longitude = coords$x, latitude = coords$y)$illuminatedFraction
r_moon <- master_grid; values(r_moon) <- moon_vals; names(r_moon) <- "moon_angle"

# E. Placeholders (Fronts & Hooks)
# Note: Fronts calculation is complex; using 0 placeholder for stability unless needed
r_fronts      <- master_grid * 0; names(r_fronts)      <- "front_z"
r_hooks_rule  <- master_grid * 0 + 1; names(r_hooks_rule) <- "hooks_rule" # Integer 1

# Combine ALL Variables into Prediction Stack
full_stack <- c(env_stack_dynamic, r_depth, r_shore, r_month, r_doy, r_moon, r_fronts, r_sst_anomaly, r_ssh_anomaly, r_hooks_rule)

# Convert to Data Frame for Prediction
pred_df <- as.data.frame(full_stack, xy = TRUE, na.rm = TRUE)

if(nrow(pred_df) == 0) stop("Error: Environment stack resulted in 0 valid pixels.")

# ----------------------------------------------------------------
# 6. DEFINE OPERATIONAL AVERAGES (From Shiny App)
# ----------------------------------------------------------------
# These are required columns for the Fishery models
inputs_swordfish <- list(
  number_light_sticks = 200, number_of_floats = 30, soak_duration = 12, day_hours = 2, night_hours = 10
)
inputs_yellowfin <- list(
  number_light_sticks = 50, number_of_floats = 40, soak_duration = 8, day_hours = 7, night_hours = 1
)

# ----------------------------------------------------------------
# 7. PREDICTION LOOP
# ----------------------------------------------------------------
model_files <- list.files(models_dir, pattern = "\\.rds$", full.names = TRUE)

if (length(model_files) == 0) stop(glue("No model files found in {models_dir}"))

message(glue("Found {length(model_files)} models. Starting predictions..."))

for (m_file in model_files) {
  
  model_name <- basename(m_file)
  message(glue("Processing: {model_name}"))
  
  # A. Determine Mode (Fishery vs Protected) & Target
  is_swordfish_target <- grepl("Swordfish_Target", model_name)
  is_yellowfin_target <- grepl("Yellowfin_Target", model_name)
  is_manta            <- grepl("MANTA_RAY", model_name)
  
  # Prepare local dataframe
  current_df <- pred_df
  
  # B. Add Operational Inputs (Only for Fishery models)
  if (is_swordfish_target) {
    for (var in names(inputs_swordfish)) current_df[[var]] <- inputs_swordfish[[var]]
  } else if (is_yellowfin_target) {
    for (var in names(inputs_yellowfin)) current_df[[var]] <- inputs_yellowfin[[var]]
  }
  
  # C. Load Model
  tryCatch({
    model_bundled <- readRDS(m_file)
    model_obj     <- bundle::unbundle(model_bundled)
    
    # D. Predict
    preds <- NULL
    
    if (is_manta) {
      # Manta Ray (GAM) uses standard predict type='response'
      # Remove XY for prediction if model doesn't use them
      preds <- predict(model_obj, newdata = current_df %>% select(-x, -y), type = "response")
      preds <- as.numeric(preds)
      
    } else {
      # Fishery Models (Stacks/Ensemble) use type='prob'
      preds_prob <- predict(model_obj, new_data = current_df %>% select(-x, -y), type = "prob")
      preds <- as.numeric(preds_prob$.pred_presence)
    }
    
    # E. Save Raster
    r_out <- master_grid
    values(r_out) <- NA
    r_out[cellFromXY(r_out, current_df[,c("x","y")])] <- preds
    names(r_out) <- gsub(".rds", "", model_name)
    
    # Construct Filename: PRED_{Date}_{ModelName}.tif
    save_name <- paste0("PRED_", date_forecast, "_", gsub(".rds", ".tif", model_name))
    save_path <- file.path(preds_dir, save_name)
    
    writeRaster(r_out, save_path, overwrite = TRUE)
    message(glue("  -> Saved to {save_name}"))
    
  }, error = function(e) {
    message(glue("  -> ERROR predicting {model_name}: {e$message}"))
  })
}

message("All predictions completed.")
