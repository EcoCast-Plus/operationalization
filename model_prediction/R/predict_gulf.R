# Predict Gulf of Mexico - Fault Tolerant Version

# --- 1. Load Libraries ---
library(terra)
library(sf)
library(dplyr)
library(glue)
library(lubridate)
library(bundle)
library(xgboost)
library(ranger)
library(mgcv)
library(oce)
library(stacks)
library(tidymodels)
library(workflows)

# --- 2. Define Directories ---
models_dir <- "model_prediction/gulf/results"
preds_dir  <- "model_prediction/gulf/predictions"
if (!dir.exists(preds_dir)) dir.create(preds_dir, recursive = TRUE)

raw_dir    <- "data_acquisition/netcdfs/cmems_ncdfs"
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
  
  # Note: MLD (mld) is usually a Forecast variable in CMEMS (like thetao)
  # Obs variables are strictly observed past data.
  obs_vars <- c("l.chl", "sla", "sst", "analysed_sst", "ugosa", "vgosa")
  
  if (var_name %in% obs_vars) target_date <- date_obs
  
  pattern <- glue("_{var_name}_{target_date}")
  files <- list.files(search_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) stop(glue("MISSING FILE: {var_name} for {target_date}"))
  return(files[1])
}

# ----------------------------------------------------------------
# HELPER: Safety Validator
# ----------------------------------------------------------------
# Checks if a layer is valid. If all NA, replaces with 0 to prevent crash.
validate_layer <- function(r, name, master_grid) {
  # 1. Resample to master grid if needed
  if (!compareGeom(r, master_grid, stopOnError = FALSE)) {
    r <- resample(r, master_grid, method = "bilinear")
  }
  
  # 2. Check for empty (All NA)
  if (all(is.na(values(r)))) {
    message(glue("WARNING: Layer '{name}' is 100% NA. Filling with 0 to prevent crash."))
    r <- master_grid * 0
  }
  
  # 3. Rename
  names(r) <- name
  return(r)
}

# ----------------------------------------------------------------
# 4. LOAD & PROCESS ENVIRONMENTAL DATA
# ----------------------------------------------------------------
message("Loading Dynamic Environmental Data...")

load_raw <- function(var_name, nc_var) {
  r <- rast(find_file(var_name, raw_dir))[nc_var]
  return(r[[1]]) 
}

# Load Raw
r_sst        <- load_raw("thetao", "thetao") 
master_grid  <- r_sst # Set Master Grid

r_ssh        <- load_raw("ssh", "zos")
r_chl        <- load_raw("l.chl", "CHL")
r_uo         <- load_raw("uo", "uo")
r_vo         <- load_raw("vo", "vo")
r_bottom_t   <- load_raw("bottom_t", "tob")
r_thetao_150 <- load_raw("thetao_150m", "thetao")
r_thetao_500 <- load_raw("thetao_500m", "thetao")
r_mld        <- load_raw("mld", "mlotst")
r_so         <- load_raw("so", "so")

# Calculate Derived
message("Calculating Derived Variables (EKE, TKE)...")
r_eke <- 0.5 * (r_uo^2 + r_vo^2)
r_tke <- 0.5 * (r_uo^2 + r_vo^2)

# Stack and Validate Dynamic Layers
env_stack_dynamic <- c(
  validate_layer(r_sst, "thetao", master_grid),
  validate_layer(r_chl, "chl", master_grid),
  validate_layer(r_ssh, "zos", master_grid),
  validate_layer(r_bottom_t, "bottom_t", master_grid),
  validate_layer(r_thetao_150, "thetao_150m", master_grid),
  validate_layer(r_thetao_500, "thetao_500m", master_grid),
  validate_layer(r_eke, "eke", master_grid),
  validate_layer(r_tke, "tke", master_grid),
  validate_layer(r_mld, "mlotst", master_grid),
  validate_layer(r_so, "so", master_grid)
)

# ----------------------------------------------------------------
# 5. LOAD/GENERATE STATIC VARIABLES
# ----------------------------------------------------------------
message("Loading Static Variables...")

# A. Bathymetry & Shore
if(file.exists(file.path(static_dir, "bathymetry.tif"))) {
  r_depth_raw <- rast(file.path(static_dir, "bathymetry.tif"))
  r_shore_raw <- rast(file.path(static_dir, "DfromShore.tif"))
  
  r_depth <- validate_layer(r_depth_raw, "depth", master_grid)
  r_shore <- validate_layer(r_shore_raw, "dfrom_shore", master_grid)
} else {
  message("WARNING: Static files missing. Using 0 placeholders.")
  r_depth <- master_grid * 0; names(r_depth) <- "depth"
  r_shore <- master_grid * 0; names(r_shore) <- "dfrom_shore"
}

# B. Constants
r_striparea <- master_grid * 0 + 1876712; names(r_striparea) <- "striparea"

# C. Climatology (Anomalies)
message("Calculating Anomalies...")
target_doy <- yday(date_forecast)

# SST Anomaly
sst_clim_file <- list.files(static_dir, pattern = "sst_daily_climatology", full.names = TRUE)[1]
if (!is.na(sst_clim_file)) {
  r_sst_clim_res <- resample(rast(sst_clim_file), master_grid, method="bilinear")
  # Safety check for DOY
  val <- if(target_doy <= nlyr(r_sst_clim_res)) r_sst - r_sst_clim_res[[target_doy]] else r_sst * 0
  r_sst_anomaly <- validate_layer(val, "sst_anomaly", master_grid)
} else { 
  r_sst_anomaly <- master_grid * 0; names(r_sst_anomaly) <- "sst_anomaly" 
}

# SSH Anomaly
ssh_clim_file <- list.files(static_dir, pattern = "ssh_daily_climatology", full.names = TRUE)[1]
if (!is.na(ssh_clim_file)) {
  r_ssh_clim_res <- resample(rast(ssh_clim_file), master_grid, method="bilinear")
  val <- if(target_doy <= nlyr(r_ssh_clim_res)) r_ssh - r_ssh_clim_res[[target_doy]] else r_ssh * 0
  r_ssh_anomaly <- validate_layer(val, "ssh_anomaly", master_grid)
} else { 
  r_ssh_anomaly <- master_grid * 0; names(r_ssh_anomaly) <- "ssh_anomaly" 
}

# D. Time & Space
r_month <- master_grid * 0 + as.integer(month(date_forecast)); names(r_month) <- "month"
r_doy   <- master_grid * 0 + as.integer(yday(date_forecast));  names(r_doy)   <- "doy"

coords <- as.data.frame(master_grid, xy=TRUE)[, c("x", "y")]
moon_vals <- oce::moonAngle(t = date_forecast, longitude = coords$x, latitude = coords$y)$illuminatedFraction
r_moon <- master_grid; values(r_moon) <- moon_vals; names(r_moon) <- "moon_angle"

# E. Placeholders
r_fronts      <- master_grid * 0; names(r_fronts)      <- "front_z"
r_hooks_rule  <- master_grid * 0 + 1; names(r_hooks_rule) <- "hooks_rule" 

# Combine Final Stack
full_stack <- c(env_stack_dynamic, r_depth, r_shore, r_month, r_doy, r_moon, r_fronts, r_sst_anomaly, r_ssh_anomaly, r_hooks_rule, r_striparea)

# Final Dataframe
# Using na.omit() here. If validate_layer() worked, we shouldn't lose everything.
pred_df <- as.data.frame(full_stack, xy = TRUE, na.rm = TRUE)

# ALIAS CREATION (For Manta Ray)
pred_df$ChlA       <- pred_df$chl
pred_df$SST        <- pred_df$thetao
pred_df$SSH        <- pred_df$zos
pred_df$Front_Z    <- pred_df$front_z
pred_df$Depth      <- pred_df$depth
pred_df$DfromShore <- pred_df$dfrom_shore

# Safety Check
if(nrow(pred_df) == 0) {
  # If still 0, we print which columns have NAs to help debug
  message("CRITICAL ERROR: Dataframe has 0 rows. Checking NA counts per column:")
  df_check <- as.data.frame(full_stack, xy=FALSE, na.rm=FALSE)
  print(colSums(is.na(df_check)))
  stop("Terminating due to empty prediction frame.")
}

# ----------------------------------------------------------------
# 6. DEFINE PREDICTORS & INPUTS
# ----------------------------------------------------------------
fishery_predictors <- c(
  "soak_duration", "doy", "mlotst", "so", "thetao", "uo", "vo", "zos", 
  "sst_anomaly", "ssh_anomaly", "moon_angle", "chl", "front_z", "eke", 
  "tke", "thetao_150m", "thetao_500m", "day_hours", "night_hours", 
  "hooks_rule", "number_light_sticks", "number_of_floats", "depth", "dfrom_shore"
)

inputs_swordfish <- list(number_light_sticks = 200, number_of_floats = 30, soak_duration = 12, day_hours = 2, night_hours = 10)
inputs_yellowfin <- list(number_light_sticks = 50, number_of_floats = 40, soak_duration = 8, day_hours = 7, night_hours = 1)

# ----------------------------------------------------------------
# 7. PREDICTION LOOP
# ----------------------------------------------------------------
model_files <- list.files(models_dir, pattern = "\\.rds$", full.names = TRUE)
message(glue("Found {length(model_files)} models. Starting predictions..."))

for (m_file in model_files) {
  model_name <- basename(m_file)
  message(glue("Processing: {model_name}"))
  
  is_swordfish_target <- grepl("Swordfish_Target", model_name)
  is_yellowfin_target <- grepl("Yellowfin_Target", model_name)
  is_manta            <- grepl("MANTA_RAY", model_name)
  
  # Reset & Setup Operational Vars
  current_df <- pred_df
  current_df$hooks_rule <- as.factor(current_df$hooks_rule)
  
  if (is_swordfish_target) {
    for (var in names(inputs_swordfish)) current_df[[var]] <- inputs_swordfish[[var]]
  } else if (is_yellowfin_target) {
    for (var in names(inputs_yellowfin)) current_df[[var]] <- inputs_yellowfin[[var]]
  }
  
  tryCatch({
    model_bundled <- readRDS(m_file)
    model_obj     <- bundle::unbundle(model_bundled)
    
    preds <- NULL
    
    if (is_manta) {
      preds <- predict(model_obj, newdata = current_df, type = "response")
      preds <- as.numeric(preds)
    } else {
      # Filter to Exact Predictors
      clean_df <- current_df %>% dplyr::select(dplyr::all_of(fishery_predictors))
      preds_prob <- predict(model_obj, new_data = clean_df, type = "prob")
      preds <- as.numeric(preds_prob$.pred_presence)
    }
    
    # Save
    if (!is.null(preds)) {
      r_out <- master_grid
      values(r_out) <- NA
      r_out[cellFromXY(r_out, current_df[,c("x","y")])] <- preds
      names(r_out) <- gsub(".rds", "", model_name)
      
      save_name <- paste0("PRED_", date_forecast, "_", gsub(".rds", ".tif", model_name))
      save_path <- file.path(preds_dir, save_name)
      
      writeRaster(r_out, save_path, overwrite = TRUE)
      message(glue("  -> Saved to {save_name}"))
    }
    
  }, error = function(e) {
    message(glue("  -> ERROR predicting {model_name}: {e$message}"))
  })
}

message("All predictions completed.")
