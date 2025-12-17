# ==============================================================================
# Script: predict_gulf.R
# Description: Generates predictions for Gulf of Mexico species.
#              Reads Raw NetCDFs (downloaded in Job 1), processes them,
#              and runs tidysdm models.
# ==============================================================================

# --- 1. Load Libraries ---
library(tidysdm)
library(tidyverse)
library(sf)
library(terra)
library(lubridate)
library(oce)
library(grec)
library(bundle)
library(xgboost)
library(ranger)
library(mgcv)
library(glue)

# --- 2. Configuration & Paths ---

# Directories matching the GitHub Workflow structure
# "Raw" inputs passed from Job 1
raw_ncdir     <- "data_acquisition/netcdfs/cmems_ncdfs"
# Where models live
models_dir    <- "gulf/results"
# Where static files (climatology/bathy) live
static_dir    <- "gulf/data"
# Where to save final predictions
output_dir    <- "gulf/data"

# Create output dir if missing
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Target Date (Tomorrow) - Must match the download date logic!
target_date <- Sys.Date() + 1
message(glue("Starting Gulf Prediction for date: {target_date}"))


# --- 3. Construct Environmental Stack ---
message("Building Environmental Stack from Raw NetCDFs...")

# Helper to find downloaded files
# Note: Matches format "{product}_{variable}_{date}.nc" or similar
find_file <- function(var_pattern) {
  # Regex: Look for the variable name in the middle of the filename
  pattern <- glue(".*{var_pattern}.*{target_date}.*\\.nc$")
  f <- list.files(raw_ncdir, pattern = pattern, full.names = TRUE)
  
  if (length(f) == 0) {
    stop(glue("CRITICAL ERROR: Could not find NetCDF for '{var_pattern}' for date {target_date}. 
               Check if Job 1 (Download) actually downloaded this variable."))
  }
  # If multiple files match (e.g. thetao vs thetao_150m), try to be specific
  return(f[1])
}

# --- Load Raw NetCDFs ---
# We use +0 to force them into memory as SpatRasters immediately

# Physics
r_bottom_t   <- rast(find_file("bottom_t")) + 0; names(r_bottom_t)   <- "bottom_t"
r_so         <- rast(find_file("so")) + 0;       names(r_so)         <- "so"
r_uo         <- rast(find_file("uo")) + 0
r_vo         <- rast(find_file("vo")) + 0
r_cur        <- c(r_uo, r_vo); names(r_cur) <- c("uo", "vo")

# Temperature at Depths
# NOTE: Ensure your 'acquire_cmems.R' saved these with distinct names!
r_thetao_surf <- rast(find_file("thetao")) + 0;      names(r_thetao_surf) <- "thetao"
r_thetao_150  <- rast(find_file("thetao_150m")) + 0; names(r_thetao_150)  <- "thetao_150m"
r_thetao_500  <- rast(find_file("thetao_500m")) + 0; names(r_thetao_500)  <- "thetao_500m"
r_mld         <- rast(find_file("mlotst|mld")) + 0;  names(r_mld)         <- "mlotst"

# Surface Bio/Phys
r_chla       <- rast(find_file("chl|CHL")) + 0;      names(r_chla)        <- "chl"
r_ugosa      <- rast(find_file("ugosa")) + 0
r_vgosa      <- rast(find_file("vgosa")) + 0
r_ssh        <- rast(find_file("zos|ssh|sla")) + 0;  names(r_ssh)         <- "zos"

# Define Master Grid (Surface Temp)
master_grid <- r_thetao_surf

# Resample everything to Master Grid
r_chla  <- resample(r_chla, master_grid, method = "bilinear")
r_ugosa <- resample(r_ugosa, master_grid, method = "bilinear")
r_vgosa <- resample(r_vgosa, master_grid, method = "bilinear")
r_ssh   <- resample(r_ssh, master_grid, method = "bilinear")

# Build Base Stack
predictor_stack_base <- c(r_bottom_t, r_so, r_cur, r_thetao_surf, r_thetao_150, r_thetao_500, r_mld, r_chla, r_ugosa, r_vgosa, r_ssh)

# --- Derived Variables ---
target_doy <- yday(target_date)

# 1. Climatologies
sst_clim <- rast(file.path(static_dir, "sst_daily_climatology_1993-2024.tif"))
ssh_clim <- rast(file.path(static_dir, "ssh_daily_climatology_1993-2024.tif"))

# 2. Anomalies
sst_clim_res <- resample(project(sst_clim[[target_doy]], crs(master_grid)), master_grid)
sst_anomaly  <- r_thetao_surf - sst_clim_res; names(sst_anomaly) <- "sst_anomaly"

ssh_clim_res <- resample(project(ssh_clim[[target_doy]], crs(master_grid)), master_grid)
ssh_anomaly  <- r_ssh - ssh_clim_res; names(ssh_anomaly) <- "ssh_anomaly"

# 3. Kinetic Energy
tke <- 0.5 * (r_uo^2 + r_vo^2); names(tke) <- "tke"
eke <- 0.5 * (r_ugosa^2 + r_vgosa^2); names(eke) <- "eke"

# 4. Static Variables
depth       <- resample(rast(file.path(static_dir, "bathymetry.tif")), master_grid); names(depth) <- "depth"
dist_shore  <- resample(rast(file.path(static_dir, "DfromShore.tif")), master_grid); names(dist_shore) <- "dfrom_shore"
strip_area  <- resample(rast(file.path(static_dir, "striparea.tif")), master_grid);  names(strip_area) <- "striparea"

# 5. Fronts
fronts <- grec::getGradients(r_thetao_surf, method = "BelkinOReilly2009")
max_front <- global(fronts, "max", na.rm=TRUE)$max
front_z <- if(is.na(max_front) || max_front == 0) fronts * 0 else fronts / max_front
names(front_z) <- "front_z"

# 6. Time Variables
coords <- as.data.frame(master_grid, xy=TRUE)[, c("x", "y")]
moon <- rast(master_grid, vals = oce::moonAngle(t = target_date, longitude = coords$x, latitude = coords$y)$illuminatedFraction); names(moon) <- "moon_angle"
month_r <- rast(master_grid, vals = as.integer(format(target_date, "%m"))); names(month_r) <- "month"
doy_r <- rast(master_grid, vals = as.integer(target_doy)); names(doy_r) <- "doy"
hooks_r <- rast(master_grid, vals = 1L); names(hooks_r) <- "hooks_rule"

# --- 4. Final Stack & Masking ---
predictor_stack_final <- c(predictor_stack_base, sst_anomaly, ssh_anomaly, depth, strip_area, dist_shore, front_z, tke, eke, moon, month_r, doy_r, hooks_r)

# Mask
# Ensure these names match EXACTLY with layers in predictor_stack_final
mask_layers <- intersect(names(predictor_stack_final), c("mlotst", "thetao_150m", "bottom_t"))
if (length(mask_layers) > 0) {
  master_mask <- app(predictor_stack_final[[mask_layers]], fun = "mean", na.rm = FALSE)
  predictor_stack_final <- terra::mask(predictor_stack_final, master_mask)
}

# Convert to DF
raster_df_env <- as.data.frame(predictor_stack_final, na.rm = FALSE, cells = TRUE) %>%
  mutate(hooks_rule = as.integer(hooks_rule), doy = as.integer(doy), month = as.integer(month))


# --- 5. Prediction Logic ---

# Helper: Fishery Inputs
get_fishery_inputs <- function(type) {
  obs_file <- file.path(static_dir, "observer_GOM_cleaned_projected.csv")
  if (!file.exists(obs_file)) stop("Observer CSV missing.")
  
  obs_data <- read.csv(obs_file)
  dat <- if (type == "Swordfish_Target") filter(obs_data, begin_set_time > 1500) else filter(obs_data, begin_set_time < 1200)
  
  data.frame(
    soak_duration = mean(dat$soak_duration, na.rm=TRUE),
    day_hours = mean(dat$day_hours, na.rm=TRUE),
    night_hours = mean(dat$night_hours, na.rm=TRUE)
  )
}

daily_results <- list(Tomorrow = list(Fishery = list(), Protected_Species = list()))

species_list <- c("tuna_yellowfin", "swordfish", "wahoo", "dolphin_fish_mahi_mahi",
                  "tuna_bluefin", "marlin_blue", "skipjack", "tuna_bigeye", 
                  "shark_silky", "shark_mako_shortfin", "sailfish", "marlin_white")

# A. Fishery Models
for (obj in c("Swordfish_Target", "Yellowfin_Target")) {
  message(glue("Predicting {obj}..."))
  inputs <- get_fishery_inputs(obj)
  
  df_fish <- raster_df_env %>% 
    mutate(soak_duration = inputs$soak_duration, 
           day_hours = inputs$day_hours, 
           night_hours = inputs$night_hours)
  
  preds_list <- list()
  for (s in species_list) {
    mod_path <- file.path(models_dir, paste0(s, "_", obj, "_final_ensemble.rds"))
    if (file.exists(mod_path)) {
      mod <- bundle::unbundle(readRDS(mod_path))
      df_clean <- na.omit(df_fish)
      if (nrow(df_clean) > 0) {
        p <- predict(mod, new_data = select(df_clean, -cell), type = "prob")
        r_out <- rast(master_grid, nlyr=1); r_out[df_clean$cell] <- as.numeric(p$.pred_presence); names(r_out) <- s
        preds_list[[s]] <- r_out
      }
    }
  }
  if (length(preds_list) > 0) daily_results$Tomorrow$Fishery[[obj]] <- rast(preds_list)
}

# B. Protected Species
manta_path <- file.path(models_dir, "MANTA_RAY_final_ensemble.rds")
if (file.exists(manta_path)) {
  message("Predicting Manta Ray...")
  mod <- bundle::unbundle(readRDS(manta_path))
  df_clean <- na.omit(raster_df_env)
  p <- predict(mod, new_data = select(df_clean, -cell), type = "prob")
  r_out <- rast(master_grid, nlyr=1); r_out[df_clean$cell] <- as.numeric(p$.pred_presence); names(r_out) <- "manta_ray"
  daily_results$Tomorrow$Protected_Species[["manta_ray"]] <- r_out
}

# --- 6. Save Output ---
wrap_list <- function(l) {
  if (inherits(l, "SpatRaster")) return(terra::wrap(l))
  if (is.list(l)) return(lapply(l, wrap_list))
  return(l)
}

saveRDS(wrap_list(daily_results), file = file.path(output_dir, "latest_predictions.rds"))
message("Prediction Complete. Saved to latest_predictions.rds")
