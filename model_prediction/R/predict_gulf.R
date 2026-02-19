# Predict Gulf of Mexico - Coastal Manta Fix + Env Export

# --- 1. Load Libraries ---
library(terra)
library(sf)
library(dplyr)
library(glue)
library(lubridate)
library(bundle)
library(xgboost)
library(ranger)
library(mgcv)        # [CRITICAL] Required for Manta Ray (GAM) prediction
library(oce)
library(stacks)
library(tidymodels)
library(workflows)
library(tidysdm)     # [CRITICAL] Required for Ensemble model dispatch
library(grec)
library(raster)
library(ncdf4)

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
# HELPER: Smart File Finder (Updated logic)
# ----------------------------------------------------------------
find_file <- function(var_name, search_dir) {
  # 1. Determine the ideal target date
  target_date <- date_forecast
  
  # Forecast vars use date_forecast (Tomorrow)
  # Observation vars use date_obs (Yesterday)
  # Note: Added "ugosa" and "vgosa" explicitly here if they are observational NRT products
  obs_vars <- c("l.chl", "sla", "sst", "analysed_sst", "ugosa", "vgosa")
  if (var_name %in% obs_vars) target_date <- date_obs
  
  # 2. Try to find the EXACT match first
  pattern_exact <- glue("_{var_name}_{target_date}")
  files_exact <- list.files(search_dir, pattern = pattern_exact, full.names = TRUE)
  
  if (length(files_exact) > 0) {
    return(files_exact[1])
  }
  
  # 3. Fallback: Search for the most recent available file
  message(glue("NOTICE: Exact file missing for {var_name} on {target_date}. Searching for most recent..."))
  
  pattern_general <- glue("_{var_name}_\\d{{4}}-\\d{{2}}-\\d{{2}}")
  files_all <- list.files(search_dir, pattern = pattern_general, full.names = TRUE)
  
  if (length(files_all) == 0) {
    stop(glue("CRITICAL ERROR: No files found for variable '{var_name}' in {search_dir}"))
  }
  
  # Sort descending (newest dates first) and pick the top one
  files_sorted <- sort(files_all, decreasing = TRUE)
  best_file <- files_sorted[1]
  
  message(glue(" -> Found substitute: {basename(best_file)}"))
  return(best_file)
}

# ----------------------------------------------------------------
# HELPER: Safety Validator
# ----------------------------------------------------------------
validate_layer <- function(r, name, master_grid) {
  if (!compareGeom(r, master_grid, stopOnError = FALSE)) {
    r <- resample(r, master_grid, method = "bilinear")
  }
  if (all(is.na(values(r)))) {
    message(glue("WARNING: Layer '{name}' is 100% NA. Filling with 0."))
    r <- master_grid * 0
  }
  names(r) <- name
  return(r)
}

# --- 4. LOAD & PROCESS ENVIRONMENTAL DATA (CORRECTED) ---
message("Loading Dynamic Environmental Data...")

load_raw <- function(var_name, nc_var) {
  f <- find_file(var_name, raw_dir)
  
  # Load the raster
  r <- rast(f)[nc_var]
  r <- r[[1]] # Take first layer if multiple exist
  
  # [FIX] Handle 0-360 Longitude issue
  # If the raster extent goes beyond 180 (e.g., up to 360), rotate it to -180/180
  if (ext(r)$xmax > 180) {
    message(glue(" -> Rotating '{var_name}' from 0-360 to -180/180 longitude..."))
    r <- rotate(r)
  }
  
  return(r) 
}

# 1. Load Standard Layers
r_sst        <- load_raw("thetao", "thetao") 
master_grid  <- r_sst 

r_ssh        <- load_raw("ssh", "zos")
r_chl        <- load_raw("l.chl", "CHL")
r_uo         <- load_raw("uo", "uo")     # Total Eastward Current
r_vo         <- load_raw("vo", "vo")     # Total Northward Current

# 2. [UPDATED] Load Geostrophic Anomalies (Required for EKE)
# Now looking for files with "ugosa" and "vgosa" in the filename
# AND extracting the variable "ugosa" and "vgosa" respectively.
r_ugosa      <- load_raw("ugosa", "ugosa") 
r_vgosa      <- load_raw("vgosa", "vgosa")

# 3. Load Subsurface & Others
r_bottom_t   <- load_raw("bottom_t", "tob")
r_thetao_150 <- load_raw("thetao_150m", "thetao")
r_thetao_500 <- load_raw("thetao_500m", "thetao")
r_mld        <- load_raw("mld", "mlotst")
r_so         <- load_raw("so", "so")

# ---------------------------------------------------------
# 4. Calculate Derived Variables Correctly
# ---------------------------------------------------------
message("Calculating Derived Variables (EKE, TKE, Fronts)...")

# A. TKE (Total Kinetic Energy) -> Uses Total Currents (uo/vo)
r_tke <- 0.5 * (r_uo^2 + r_vo^2)

# B. EKE (Eddy Kinetic Energy) -> Uses Geostrophic Anomalies (ugosa/vgosa)
# Check for NAs in ugosa/vgosa specifically to debug "all 0" issue
if(all(is.na(values(r_ugosa))) || all(is.na(values(r_vgosa)))) {
    message("WARNING: ugosa or vgosa layers are empty. EKE will be 0.")
}
r_eke <- 0.5 * (r_ugosa^2 + r_vgosa^2)

# C. SST Fronts (Frontal Intensity)
message(" -> Detecting SST Fronts (BelkinOReilly2009)...")
r_sst_raster <- raster::raster(r_sst) 

r_fronts_raw <- grec::detectFronts(r_sst_raster, method = "BelkinOReilly2009", intermediate = FALSE)
r_fronts <- rast(r_fronts_raw)

# Normalize
max_val <- global(r_fronts, "max", na.rm = TRUE)$max
if (is.na(max_val) || max_val == 0) {
  r_fronts <- r_fronts * 0
} else {
  r_fronts <- r_fronts / max_val
}

r_fronts <- resample(r_fronts, master_grid)
names(r_fronts) <- "front_z"

# ---------------------------------------------------------
# 5. Stack Everything
# ---------------------------------------------------------
env_stack_dynamic <- c(
  validate_layer(r_sst, "thetao", master_grid),
  validate_layer(r_chl, "chl", master_grid),
  validate_layer(r_ssh, "zos", master_grid),
  validate_layer(r_bottom_t, "bottom_t", master_grid),
  validate_layer(r_thetao_150, "thetao_150m", master_grid),
  validate_layer(r_thetao_500, "thetao_500m", master_grid),
  validate_layer(r_eke, "eke", master_grid),       
  validate_layer(r_tke, "tke", master_grid),       
  validate_layer(r_fronts, "front_z", master_grid), 
  validate_layer(r_mld, "mlotst", master_grid),
  validate_layer(r_so, "so", master_grid),
  validate_layer(r_uo, "uo", master_grid),
  validate_layer(r_vo, "vo", master_grid)
)

# ----------------------------------------------------------------
# 5. LOAD/GENERATE STATIC VARIABLES
# ----------------------------------------------------------------
message("Loading Static Variables...")

if(file.exists(file.path(static_dir, "bathymetry.tif"))) {
  r_depth_raw <- rast(file.path(static_dir, "bathymetry.tif"))
  r_shore_raw <- rast(file.path(static_dir, "DfromShore.tif"))
  
  r_depth <- validate_layer(r_depth_raw, "depth", master_grid)
  r_shore <- validate_layer(r_shore_raw, "dfrom_shore", master_grid)
} else {
  message("WARNING: Static files missing. Using placeholders.")
  r_depth <- master_grid * 0; names(r_depth) <- "depth"
  r_shore <- master_grid * 0; names(r_shore) <- "dfrom_shore"
}

r_striparea <- master_grid * 0 + 1876712; names(r_striparea) <- "striparea"

# Climatology
message("Calculating Anomalies...")
target_doy <- yday(date_forecast)

sst_clim_file <- list.files(static_dir, pattern = "sst_daily_climatology", full.names = TRUE)[1]
if (!is.na(sst_clim_file)) {
  r_sst_clim_res <- resample(rast(sst_clim_file), master_grid, method="bilinear")
  val <- if(target_doy <= nlyr(r_sst_clim_res)) r_sst - r_sst_clim_res[[target_doy]] else r_sst * 0
  r_sst_anomaly <- validate_layer(val, "sst_anomaly", master_grid)
} else { r_sst_anomaly <- master_grid * 0; names(r_sst_anomaly) <- "sst_anomaly" }

ssh_clim_file <- list.files(static_dir, pattern = "ssh_daily_climatology", full.names = TRUE)[1]
if (!is.na(ssh_clim_file)) {
  r_ssh_clim_res <- resample(rast(ssh_clim_file), master_grid, method="bilinear")
  val <- if(target_doy <= nlyr(r_ssh_clim_res)) r_ssh - r_ssh_clim_res[[target_doy]] else r_ssh * 0
  r_ssh_anomaly <- validate_layer(val, "ssh_anomaly", master_grid)
} else { r_ssh_anomaly <- master_grid * 0; names(r_ssh_anomaly) <- "ssh_anomaly" }

# Time & Space
r_month <- master_grid * 0 + as.integer(month(date_forecast)); names(r_month) <- "month"
r_doy   <- master_grid * 0 + as.integer(yday(date_forecast));  names(r_doy)   <- "doy"

coords <- as.data.frame(master_grid, xy=TRUE)[, c("x", "y")]
moon_vals <- oce::moonAngle(t = date_forecast, longitude = coords$x, latitude = coords$y)$illuminatedFraction
r_moon <- master_grid; values(r_moon) <- moon_vals; names(r_moon) <- "moon_angle"

# Placeholders
r_hooks_rule  <- master_grid * 0 + 1L; names(r_hooks_rule) <- "hooks_rule" 

# Combine Final Stack
full_stack <- c(env_stack_dynamic, r_depth, r_shore, r_month, r_doy, r_moon, r_sst_anomaly, r_ssh_anomaly, r_hooks_rule, r_striparea)

# ----------------------------------------------------------------
# 5b. Create Two Prediction Dataframes
# ----------------------------------------------------------------
message("Preparing Prediction Dataframes...")

prepare_df <- function(stack_in) {
  df <- as.data.frame(stack_in, xy = TRUE, na.rm = TRUE)
  
  if (nrow(df) > 0) {
    # Type Casting
    if("hooks_rule" %in% names(df)) df$hooks_rule <- as.integer(df$hooks_rule)
    if("doy" %in% names(df))        df$doy        <- as.integer(df$doy)
    if("month" %in% names(df))      df$month      <- as.integer(df$month)
    
    # Aliases
    if("chl" %in% names(df))         df$ChlA       <- df$chl
    if("thetao" %in% names(df))      df$SST        <- df$thetao
    if("zos" %in% names(df))         df$SSH        <- df$zos
    if("front_z" %in% names(df))     df$Front_Z    <- df$front_z
    if("depth" %in% names(df))       df$Depth      <- df$depth
    if("dfrom_shore" %in% names(df)) df$DfromShore <- df$dfrom_shore
  }
  return(df)
}

# 1. Fishery DF
pred_df_fishery <- prepare_df(full_stack)

# 2. Manta DF
manta_vars_to_exclude <- c("thetao_150m", "thetao_500m")
manta_stack_names <- names(full_stack)[!names(full_stack) %in% manta_vars_to_exclude]
manta_stack <- full_stack[[manta_stack_names]]

pred_df_manta <- prepare_df(manta_stack)

message(glue("Fishery Prediction Points: {nrow(pred_df_fishery)}"))
message(glue("Manta Ray Prediction Points: {nrow(pred_df_manta)}"))

if(nrow(pred_df_fishery) == 0 && nrow(pred_df_manta) == 0) stop("Terminating due to empty prediction frames.")

# ----------------------------------------------------------------
# 6. PREDICTION LOOP
# ----------------------------------------------------------------
fishery_predictors <- c(
  "soak_duration", "doy", "mlotst", "so", "thetao", "uo", "vo", "zos", 
  "sst_anomaly", "ssh_anomaly", "moon_angle", "chl", "front_z", "eke", 
  "tke", "thetao_150m", "thetao_500m", "day_hours", "night_hours", 
  "hooks_rule", "number_light_sticks", "number_of_floats", "depth", "dfrom_shore"
)

inputs_swordfish <- list(number_light_sticks = 352, number_of_floats = 193, soak_duration = 8, day_hours = 2, night_hours = 2)
inputs_yellowfin <- list(number_light_sticks = 22, number_of_floats = 167, soak_duration = 7, day_hours = 4, night_hours = .2)

model_files <- list.files(models_dir, pattern = "\\.rds$", full.names = TRUE)
message(glue("Found {length(model_files)} models. Starting predictions..."))

for (m_file in model_files) {
  model_name <- basename(m_file)
  
  # --- Model Identification ---
  is_swordfish_target <- grepl("Swordfish_Target", model_name)
  is_yellowfin_target <- grepl("Yellowfin_Target", model_name)
  is_manta            <- grepl("MANTA_RAY", model_name)
  is_depredation      <- grepl("Depredation", model_name)
  
  # Dynamic logging based on model type
  model_type_log <- case_when(
    is_manta ~ "Manta Ray",
    is_depredation ~ "Shark Depredation",
    TRUE ~ "Fishery Species"
  )
  message(glue("Processing [{model_type_log}]: {model_name}"))
  
  # --- Select Correct Dataframe ---
  if (is_manta) {
    current_df <- pred_df_manta
  } else {
    current_df <- pred_df_fishery
  }
  
  # --- Inject Objective Constants ---
  if (is_swordfish_target) {
    for (var in names(inputs_swordfish)) current_df[[var]] <- inputs_swordfish[[var]]
  } else if (is_yellowfin_target) {
    for (var in names(inputs_yellowfin)) current_df[[var]] <- inputs_yellowfin[[var]]
  }
  
  # --- Load and Predict ---
  tryCatch({
    model_bundled <- readRDS(m_file)
    model_obj     <- bundle::unbundle(model_bundled)
    
    preds <- NULL
    
    if (is_manta) {
      # Manta uses standard mgcv/GAM prediction
      preds <- predict(model_obj, newdata = current_df, type = "response")
      preds <- as.numeric(preds)
    } else {
      # Fishery & Depredation use tidymodels stacked ensembles
      clean_df <- current_df %>% 
        dplyr::select(dplyr::all_of(fishery_predictors)) %>%
        as_tibble()
      
      preds_prob <- predict(model_obj, new_data = clean_df, type = "prob")
      preds <- as.numeric(preds_prob$.pred_presence)
    }
    
    # --- Rasterize and Save ---
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

# ----------------------------------------------------------------
# 7. EXPORT ENVIRONMENTAL LAYERS
# ----------------------------------------------------------------
message("Exporting Environmental Layers for Viewer...")

env_map <- list(
  "thetao"      = "sst",
  "zos"         = "ssh",
  "chl"         = "chlorophyll",
  "so"          = "salinity",
  "eke"         = "eke",
  "tke"         = "tke",
  "mlotst"      = "mld",
  "bottom_t"    = "bottom_temp",
  "depth"       = "bathymetry",
  "dfrom_shore" = "distance_to_shore",
  "front_z"     = "front_z",       
  "thetao_150m" = "thetao_150m",   
  "thetao_500m" = "thetao_500m",   
  "uo"          = "uo",            
  "vo"          = "vo"             
)

for (layer_name in names(env_map)) {
  if (layer_name %in% names(full_stack)) {
    r_out <- full_stack[[layer_name]]
    out_suffix <- env_map[[layer_name]]
    
    save_name <- glue("PRED_{date_forecast}_{out_suffix}.tif")
    save_path <- file.path(preds_dir, save_name)
    
    writeRaster(r_out, save_path, overwrite = TRUE)
    message(glue("  -> Exported Env: {save_name}"))
  } else {
    message(glue("  -> WARNING: Layer '{layer_name}' not found in stack. Skipping."))
  }
}

if (all(c("uo", "vo") %in% names(full_stack))) {
  r_curr <- sqrt(full_stack[["uo"]]^2 + full_stack[["vo"]]^2)
  names(r_curr) <- "current_speed"
  
  save_name <- glue("PRED_{date_forecast}_current_speed.tif")
  save_path <- file.path(preds_dir, save_name)
  
  writeRaster(r_curr, save_path, overwrite = TRUE)
  message(glue("  -> Exported Env: {save_name}"))
}

message("All predictions and environmental layers completed.")
