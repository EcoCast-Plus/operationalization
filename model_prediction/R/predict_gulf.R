# Predict Gulf of Mexico - Final Logic Fix

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
  
  # [CRITICAL FIX] Removed "mld" from this list. 
  # MLD is a model forecast (Dec 19), not an observation (Dec 17).
  obs_vars <- c("l.chl", "sla", "sst", "analysed_sst", "ugosa", "vgosa")
  
  if (var_name %in% obs_vars) target_date <- date_obs
  
  pattern <- glue("_{var_name}_{target_date}")
  files <- list.files(search_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) stop(glue("MISSING FILE: {var_name} for {target_date}"))
  return(files[1])
}

# ----------------------------------------------------------------
# 4. LOAD & PROCESS ENVIRONMENTAL DATA
# ----------------------------------------------------------------
message("Loading Dynamic Environmental Data...")

load_layer <- function(var_name, nc_var) {
  r <- rast(find_file(var_name, raw_dir))[nc_var]
  return(r[[1]]) 
}

r_sst        <- load_layer("thetao", "thetao") 
r_ssh        <- load_layer("ssh", "zos")
r_chl        <- load_layer("l.chl", "CHL")
r_uo         <- load_layer("uo", "uo")
r_vo         <- load_layer("vo", "vo")
r_bottom_t   <- load_layer("bottom_t", "tob")
r_thetao_150 <- load_layer("thetao_150m", "thetao")
r_thetao_500 <- load_layer("thetao_500m", "thetao")
r_mld        <- load_layer("mld", "mlotst")
r_so         <- load_layer("so", "so")

master_grid <- r_sst

message("Calculating Derived Variables (EKE, TKE)...")
r_eke <- 0.5 * (r_uo^2 + r_vo^2); names(r_eke) <- "eke"
r_tke <- 0.5 * (r_uo^2 + r_vo^2); names(r_tke) <- "tke"

env_stack_dynamic <- c(
  r_sst, 
  resample(r_chl, master_grid, method="bilinear"),
  resample(r_ssh, master_grid, method="bilinear"),
  resample(r_bottom_t, master_grid, method="bilinear"),
  resample(r_thetao_150, master_grid, method="bilinear"),
  resample(r_thetao_500, master_grid, method="bilinear"),
  resample(r_eke, master_grid, method="bilinear"),
  resample(r_tke, master_grid, method="bilinear"),
  resample(r_mld, master_grid, method="bilinear"),
  resample(r_so, master_grid, method="bilinear")
)
names(env_stack_dynamic) <- c("thetao", "chl", "zos", "bottom_t", "thetao_150m", "thetao_500m", "eke", "tke", "mlotst", "so")

# ----------------------------------------------------------------
# 5. LOAD/GENERATE STATIC VARIABLES
# ----------------------------------------------------------------
message("Loading Static Variables...")

if(file.exists(file.path(static_dir, "bathymetry.tif"))) {
  r_depth <- rast(file.path(static_dir, "bathymetry.tif"))
  r_shore <- rast(file.path(static_dir, "DfromShore.tif"))
  
  r_depth <- resample(r_depth, master_grid, method="bilinear"); names(r_depth) <- "depth"
  r_shore <- resample(r_shore, master_grid, method="bilinear"); names(r_shore) <- "dfrom_shore"
} else {
  stop("Static files missing in model_prediction/gulf/data/")
}

# Constant for striparea
r_striparea <- master_grid * 0 + 1876712
names(r_striparea) <- "striparea"

# Climatology
target_doy <- yday(date_forecast)
sst_clim_file <- list.files(static_dir, pattern = "sst_daily_climatology", full.names = TRUE)[1]
if (!is.na(sst_clim_file)) {
  r_sst_clim_res <- resample(rast(sst_clim_file), master_grid, method="bilinear")
  r_sst_anomaly <- if(target_doy <= nlyr(r_sst_clim_res)) r_sst - r_sst_clim_res[[target_doy]] else r_sst * 0
} else { r_sst_anomaly <- r_sst * 0 }
names(r_sst_anomaly) <- "sst_anomaly"

ssh_clim_file <- list.files(static_dir, pattern = "ssh_daily_climatology", full.names = TRUE)[1]
if (!is.na(ssh_clim_file)) {
  r_ssh_clim_res <- resample(rast(ssh_clim_file), master_grid, method="bilinear")
  r_ssh_anomaly <- if(target_doy <= nlyr(r_ssh_clim_res)) r_ssh - r_ssh_clim_res[[target_doy]] else r_ssh * 0
} else { r_ssh_anomaly <- r_ssh * 0 }
names(r_ssh_anomaly) <- "ssh_anomaly"

# Time Variables
r_month <- master_grid * 0 + as.integer(month(date_forecast)); names(r_month) <- "month"
r_doy   <- master_grid * 0 + as.integer(yday(date_forecast));  names(r_doy)   <- "doy"

# Moon Angle
coords <- as.data.frame(master_grid, xy=TRUE)[, c("x", "y")]
moon_vals <- oce::moonAngle(t = date_forecast, longitude = coords$x, latitude = coords$y)$illuminatedFraction
r_moon <- master_grid; values(r_moon) <- moon_vals; names(r_moon) <- "moon_angle"

# Placeholders
r_fronts      <- master_grid * 0; names(r_fronts)      <- "front_z"
r_hooks_rule  <- master_grid * 0 + 1; names(r_hooks_rule) <- "hooks_rule" 

# Combine ALL Variables
full_stack <- c(env_stack_dynamic, r_depth, r_shore, r_month, r_doy, r_moon, r_fronts, r_sst_anomaly, r_ssh_anomaly, r_hooks_rule, r_striparea)

pred_df <- as.data.frame(full_stack, xy = TRUE, na.rm = TRUE)

if(nrow(pred_df) == 0) stop("Error: Environment stack resulted in 0 valid pixels.")

# ----------------------------------------------------------------
# 6. DEFINE PREDICTOR LIST & ALIASES
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
      current_df$ChlA       <- current_df$chl
      current_df$SST        <- current_df$thetao
      current_df$Front_Z    <- current_df$front_z
      current_df$DfromShore <- current_df$dfrom_shore
      
      preds <- predict(model_obj, newdata = current_df, type = "response")
      preds <- as.numeric(preds)
      
    } else {
      clean_df <- current_df %>% 
        dplyr::select(dplyr::all_of(fishery_predictors))
      
      preds_prob <- predict(model_obj, new_data = clean_df, type = "prob")
      preds <- as.numeric(preds_prob$.pred_presence)
    }
    
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
