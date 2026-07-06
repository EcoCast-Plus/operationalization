# Predict Gulf of Mexico - Fishery Targets & Shark Depredation

# --- 1. Load Libraries ---
library(terra)
library(sf)
library(dplyr)
library(glue)
library(lubridate)
library(bundle)
library(xgboost)
library(ranger)
library(oce)
library(stacks)
library(tidymodels)
library(workflows)
library(tidysdm)     # Required for Ensemble model dispatch
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
  
  message(glue("CRITICAL WARNING: All available files for {var_name} are empty! Returning 0s as absolute last resort."))
  r <- rast(files_sorted[1])[nc_var][[1]]
  if (ext(r)$xmax > 180) r <- rotate(r)
  r <- r * 0 
  return(r)
}

# ----------------------------------------------------------------
# HELPER: Safety Validator
# ----------------------------------------------------------------
validate_layer <- function(r, name, master_grid) {
  if (!compareGeom(r, master_grid, stopOnError = FALSE)) {
    r <- resample(r, master_grid, method = "bilinear")
  }
  names(r) <- name
  return(r)
}

# --- 4. LOAD & PROCESS ENVIRONMENTAL DATA ---
message("Loading Dynamic Environmental Data...")

# 1. Load Standard Layers
r_sst        <- load_raw("thetao", "thetao") 
master_grid  <- r_sst 

r_ssh        <- load_raw("ssh", "zos")
r_chl        <- load_raw("l.chl", "chl")
r_uo         <- load_raw("uo", "uo")     # Total Eastward Current
r_vo         <- load_raw("vo", "vo")     # Total Northward Current

# 2. Load Geostrophic Anomalies (Required for EKE)
r_ugosa      <- load_raw("ugosa", "ugosa") 
r_vgosa      <- load_raw("vgosa", "vgosa")

# 3. Load Subsurface & Others
r_bottom_t   <- load_raw("bottom_t", "tob")
r_thetao_150 <- load_raw("thetao_150m", "thetao")
r_mld        <- load_raw("mld", "mlotst")
r_so         <- load_raw("so", "so")

# ---------------------------------------------------------
# 4. Calculate Derived Variables
# ---------------------------------------------------------
message("Calculating Derived Variables (EKE, TKE, Fronts)...")

# A. TKE (Total Kinetic Energy) 
r_tke <- 0.5 * (r_uo^2 + r_vo^2)

# B. EKE (Eddy Kinetic Energy) 
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
full_stack <- c(env_stack_dynamic, r_depth, r_shore, r_month, r_doy, r_moon, r_sst_anomaly, r_ssh_anomaly, r_hooks_rule)

# ----------------------------------------------------------------
# 5b. Create Prediction Dataframe
# ----------------------------------------------------------------
message("Preparing Prediction Dataframe...")

# Optional safety loop if any layer is fully NA, preventing crash
for (i in 1:nlyr(full_stack)) {
  if (all(is.na(values(full_stack[[i]])))) {
    full_stack[[i]] <- master_grid * 0
    names(full_stack[[i]]) <- names(full_stack)[i]
  }
}

pred_df <- as.data.frame(full_stack, xy = TRUE, na.rm = TRUE) %>%
  mutate(geometry = NA) # <--- Add this line so the column exists!

if (nrow(pred_df) > 0) {
  # Type Casting
  if("hooks_rule" %in% names(pred_df)) pred_df$hooks_rule <- as.integer(pred_df$hooks_rule)
  if("doy" %in% names(pred_df))        pred_df$doy        <- as.integer(pred_df$doy)
  if("month" %in% names(pred_df))      pred_df$month      <- as.integer(pred_df$month)
  
  # Aliases
  if("chl" %in% names(pred_df))          pred_df$ChlA       <- pred_df$chl
  if("thetao" %in% names(pred_df))       pred_df$SST        <- pred_df$thetao
  if("zos" %in% names(pred_df))          pred_df$SSH        <- pred_df$zos
  if("front_z" %in% names(pred_df))      pred_df$Front_Z    <- pred_df$front_z
  if("depth" %in% names(pred_df))        pred_df$Depth      <- pred_df$depth
  if("dfrom_shore" %in% names(pred_df))  pred_df$DfromShore <- pred_df$dfrom_shore
}

message(glue("Prediction Points: {nrow(pred_df)}"))

if(nrow(pred_df) == 0) stop("Terminating due to empty prediction frame.")

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 6. PREDICTION LOOP
# ----------------------------------------------------------------
fishery_predictors <- c(
  "soak_duration", "doy", "mlotst", "so", "thetao", "uo", "vo", "zos", 
  "sst_anomaly", "ssh_anomaly", "moon_angle", "chl", "front_z", "eke", 
  "tke", "thetao_150m", "day_hours", "night_hours", 
  "hooks_rule", "number_light_sticks", "number_of_floats", "depth", "dfrom_shore",
  "geometry" # <--- ADDED: Prevents CPUE tidymodels from crashing
)

inputs_swordfish <- list(number_light_sticks = 352, number_of_floats = 193, soak_duration = 8, day_hours = 2, night_hours = 2)
inputs_yellowfin <- list(number_light_sticks = 22, number_of_floats = 167, soak_duration = 7, day_hours = 4, night_hours = .2)

model_files <- list.files(models_dir, pattern = "\\.rds$", full.names = TRUE)
message(glue("Found {length(model_files)} models. Starting predictions..."))

for (m_file in model_files) {
  model_name <- basename(m_file)
  
  # --- Model Identification & Guard ---
  if (grepl("MANTA", model_name, ignore.case = TRUE)) {
    message(glue("Skipping {model_name} (Handled by standalone Manta script)"))
    next
  }
  
  is_swordfish_target <- grepl("Swordfish_Target", model_name)
  is_yellowfin_target <- grepl("Yellowfin_Target", model_name)
  is_depredation      <- grepl("Depredation", model_name)
  
  # Skip Utility Models (Prevents dplyr::all_of crashes)
  if (!is_swordfish_target && !is_yellowfin_target) {
    message(glue("Skipping {model_name}: Not a standard Swordfish or Yellowfin target model."))
    next
  }
  
  model_type_log <- if(is_depredation) "Shark Depredation" else "Fishery Species"
  message(glue("Processing [{model_type_log}]: {model_name}"))
  
  current_df <- pred_df
  
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
    
    # 1. ADDED: Add dummy geometry column to the current dataframe
    current_df_geo <- current_df %>% mutate(geometry = NA)
    
    clean_df <- current_df_geo %>% 
      dplyr::select(dplyr::all_of(fishery_predictors)) %>%
      as_tibble()
    
    # --- DYNAMIC CPUE vs PA LOGIC ---
    # 2. FIXED: Added "^" to only match if the filename starts with the species name
    is_cpue_model <- grepl("^swordfish_", model_name, ignore.case = TRUE) || 
                     grepl("^tuna_yellowfin_", model_name, ignore.case = TRUE)
    
    if (is_cpue_model) {
      # 1. Predict Raw CPUE
      raw_preds <- predict(model_obj, new_data = clean_df, type = "numeric")$.pred
      
      # 2. Scale to 0-1 (99th percentile) to match PA formats for GulfCast math
      cpue_99 <- quantile(raw_preds, probs = 0.99, na.rm = TRUE)
      if (cpue_99 > 0) {
        preds <- raw_preds / cpue_99
        preds[preds > 1] <- 1  
        preds[preds < 0] <- 0  
      } else {
        preds <- raw_preds * 0
      }
    } else {
      # Bycatch models remain standard PA probability
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
  "uo"          = "uo",            
  "vo"          = "vo"             
)

for (layer_name in names(env_map)) {
  if (layer_name %in% names(full_stack)) {
    r_out <- full_stack[[layer_name]]
    out_suffix <- env_map[[layer_name]]
    
    # --- CLIP TO GULF BATHYMETRY ---
    if (layer_name != "depth") {
      r_out <- terra::mask(r_out, full_stack[["depth"]])
    }
    
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
  
  # --- CLIP TO GULF BATHYMETRY ---
  r_curr <- terra::mask(r_curr, full_stack[["depth"]])
  
  save_name <- glue("PRED_{date_forecast}_current_speed.tif")
  save_path <- file.path(preds_dir, save_name)
  
  writeRaster(r_curr, save_path, overwrite = TRUE)
  message(glue("  -> Exported Env: {save_name}"))
}

message("All predictions and environmental layers completed.")
