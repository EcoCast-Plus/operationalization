# ==============================================================================
# MASTER REFERENCE: LEATHERBACK TURTLE PREDICTION WORKFLOW
# ==============================================================================
# This script documents the full process for the "Manual Prediction Engine."
# PART A: Extracts coefficients/mesh from the raw INLA model (Run ONCE).
# PART B: Runs the daily prediction using the extracted data (Run DAILY).
# ==============================================================================

library(terra)
library(sf)
library(dplyr)
library(stringr)

# --- CONFIGURATION ---
RAW_INLA_MODEL  <- "inla.st.etag.2to1.rds"       # The 3GB+ raw model file
LEAN_MODEL_OUT  <- "lean_leatherback_model.rds"  # The <1MB extracted output
SCALING_RDS     <- "scaling_params_2026.02.05.rds"
GRID_DIR        <- "model_prediction/gulf/predictions"
OUTPUT_DIR      <- "model_prediction/gulf/predictions"


# ==============================================================================
# PART A: THE EXTRACTOR (RUN ONCE)
# ==============================================================================
# Purpose: Pulls the "brains" (coefficients, mesh, spatial field) out of the 
# massive INLA object so we don't need the 'INLA' package installed to predict.
# ==============================================================================

if (file.exists(RAW_INLA_MODEL)) {
  message("PART A: Loading raw INLA model to extract parameters...")
  
  # 1. Load the massive model
  model <- readRDS(RAW_INLA_MODEL)
  
  # 2. Extract The Mesh (Geometry)
  # Logic handles standard INLA vs. inlabru structures
  if (!is.null(model$bru_info$model$effects$etag_field$main$mapper$mesh)) {
    mesh <- model$bru_info$model$effects$etag_field$main$mapper$mesh
  } else {
    # Fallback for older structures
    mesh <- model$bru_info$model$effects$etag_field$main$mesh
  }
  
  # 3. Create the Lean List
  lean_model <- list(
    # A. Fixed Effects (Intercept)
    # Note: Check rownames(model$summary.fixed) if "intercept_etag" fails
    intercept = model$summary.fixed["intercept_etag", "mean"],
    
    # B. Random Effects (Covariate Smoothers)
    # We save the Lookup Table (ID vs Mean) for each variable
    smooths = list(
      thetao     = model$summary.random$thetao[, c("ID", "mean")],
      zos        = model$summary.random$zos[, c("ID", "mean")],
      z          = model$summary.random$z[, c("ID", "mean")],
      l.chl      = model$summary.random$l.chl[, c("ID", "mean")],
      l.tke_mean = model$summary.random$l.tke_mean[, c("ID", "mean")]
    ),
    
    # C. Spatial Field (The Map)
    spatial = list(
      # The XY coordinates of the mesh nodes
      mesh_locs = as.data.frame(mesh$loc[, 1:2]),
      # The value of the spatial field at each node
      values    = model$summary.random$etag_field$mean
    )
  )
  
  # Clean up column names for the spatial engine
  colnames(lean_model$spatial$mesh_locs) <- c("X", "Y")
  
  # 4. Save the Lightweight File
  saveRDS(lean_model, LEAN_MODEL_OUT)
  message(paste("SUCCESS: Lean model saved to", LEAN_MODEL_OUT))
  
  # Clean up memory
  rm(model, mesh)
  gc()
  
} else {
  message("Skipping Part A: Raw INLA model file not found in directory.")
}


# ==============================================================================
# PART B: THE PREDICTOR (RUN DAILY)
# ==============================================================================
# Purpose: Uses the lean model + daily environmental rasters to generate maps.
# Dependencies: terra, sf, dplyr (NO INLA REQUIRED)
# ==============================================================================

message("PART B: Starting Daily Prediction...")

# 1. Load Static Assets
if (!file.exists(LEAN_MODEL_OUT)) stop("Lean Model RDS missing! Run Part A.")
m <- readRDS(LEAN_MODEL_OUT)
scaling_params <- readRDS(SCALING_RDS)

# 2. Date Setup
date_now  <- Sys.Date()
date_str  <- as.character(date_now)

# 3. Helper: Smart File Finder
# Looks for today's file; falls back to most recent if missing (e.g. cloud cover)
find_best_file <- function(all_files, var_suffix, target_date) {
  # Try exact match (PRED_2026-02-06_sst.tif)
  pattern <- paste0("PRED_", target_date, "_", var_suffix, ".tif$")
  match <- all_files[str_detect(all_files, pattern)]
  if (length(match) > 0) return(match[1])
  
  # Fallback: Find most recent
  message(paste("! Missing", var_suffix, "for", target_date, "- using most recent."))
  all_var_files <- all_files[str_detect(all_files, paste0("_", var_suffix, ".tif$"))]
  if (length(all_var_files) == 0) return(NULL)
  
  return(tail(sort(all_var_files), 1))
}

all_files <- list.files(GRID_DIR, full.names = TRUE)

# 4. Load & Scale Environment
env_vars <- c("thetao", "zos", "z", "l.chl", "l.tke_mean")
name_map <- list(
  "thetao" = "sst", "zos" = "ssh", "z" = "bathymetry", 
  "l.chl" = "chlorophyll", "l.tke_mean" = "tke"
)

layers <- list()
for (v in env_vars) {
  suffix <- name_map[[v]]
  fpath  <- find_best_file(all_files, suffix, date_str)
  
  if (is.null(fpath)) stop(paste("CRITICAL: No files found for", suffix))
  
  r <- terra::rast(fpath)
  
  # Apply Log Transforms for Biological Vars
  if (suffix %in% c("chlorophyll", "tke")) r <- log(r + 0.001)
  
  names(r) <- v
  layers[[v]] <- r
}

# Stack and Align
predictor_stack <- terra::rast(layers)
# Handle slight grid mismatches (e.g., if using older fallback data)
if (length(unique(as.character(terra::ext(predictor_stack)))) > 1) {
  predictor_stack <- terra::resample(predictor_stack, layers[[1]])
}

# Create Data Frame
pred_df <- as.data.frame(predictor_stack, xy = TRUE, na.rm = TRUE)
colnames(pred_df)[1:2] <- c("lon", "lat")

# Apply Scaling (Z-Score)
for (v in env_vars) {
  # Handle naming quirk in RDS (l.tke_mean vs l.tke_mean_mean)
  mu_name <- if(v == "l.tke_mean") "l.tke_mean_mean" else paste0(v, "_mean")
  sd_name <- if(v == "l.tke_mean") "l.tke_mean_sd"   else paste0(v, "_sd")
  
  pred_df[[v]] <- (pred_df[[v]] - scaling_params[[mu_name]]) / scaling_params[[sd_name]]
}

# 5. Manual Prediction Engine
message("Running Manual Calculation...")

# A. Covariate Effects (Lookup Table)
get_val <- function(var_name, grid_vals) {
  tab <- m$smooths[[var_name]]
  idx <- findInterval(grid_vals, tab$ID)
  idx[idx == 0] <- 1
  return(as.numeric(tab$mean[idx]))
}

lp_covs <- m$intercept + 
  get_val("thetao", pred_df$thetao) + 
  get_val("zos", pred_df$zos) +
  get_val("z", pred_df$z) + 
  get_val("l.chl", pred_df$l.chl) +
  get_val("l.tke_mean", pred_df$l.tke_mean)

# B. Spatial Field (Nearest Neighbor Bypass)
# Project grid to model CRS (Zone 16N) to find nearest mesh nodes

grid_sf <- st_as_sf(pred_df, coords = c("lon", "lat"), crs = 4326) %>% st_transform(32616)
mesh_sf <- st_as_sf(m$spatial$mesh_locs, coords = c("X", "Y"), crs = 32616)
nn_idx  <- st_nearest_feature(grid_sf, mesh_sf)
lp_spatial <- m$spatial$values[nn_idx]

# C. Probability Calculation
final_prob <- 1 / (1 + exp(-(lp_covs + lp_spatial)))

# 6. Export Results
# Reconstruct Raster
r_out <- predictor_stack[[1]]
values(r_out) <- NA
cells <- cellFromXY(r_out, as.matrix(pred_df[,c("lon", "lat")]))
r_out[cells] <- final_prob

# Save Continuous Probability
prob_file <- file.path(OUTPUT_DIR, paste0("PRED_", date_str, "_leatherback.tif"))
writeRaster(r_out, prob_file, overwrite=TRUE)

# Save Binary Core Habitat Mask (>0.71)

core_file <- file.path(OUTPUT_DIR, paste0("CORE_", date_str, "_leatherback.tif"))
writeRaster(r_out > 0.71, core_file, overwrite=TRUE)

message(paste("DONE! Saved:", basename(prob_file), "and", basename(core_file)))