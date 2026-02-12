# --- GHA ENVIRONMENT CHECK ---
# If running on GitHub, we need the Linux INLA build, not the Mac one.
if (identical(Sys.getenv("GITHUB_ACTIONS"), "true")) {
  message("Running on GitHub Actions - Installing Linux INLA...")
  
  if (!require("INLA", quietly = TRUE)) {
    # Install Bioconductor dependencies first
    if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
    BiocManager::install(c("graph", "Rgraphviz"), update = FALSE, ask = FALSE)
    
    # Install Linux-specific INLA (Stable)
    install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)
  }
  
  if (!require("inlabru", quietly = TRUE)) {
    remotes::install_version("inlabru", version = "2.12.0", repos = "https://cloud.r-project.org")
  }
}

# Fix versioning for INLA/inlabru consistency
assignInNamespace("inla.version", function(...) "24.12.11", ns="INLA")
options(inlabru_upgrade_check = FALSE)

# --- GOOGLE DRIVE DOWNLOAD ---
library(googledrive)
local_dir <- "model_prediction/gulf/results"
if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

drive_deauth()
folder <- drive_get(as_id("1a7yxmaZUm7RLolJ-svD5QYFXQZxJW5If"))
targets <- c("inla.st.etag.2to1.rds", "scaling_params_2026.02.05.rds")

for (target in targets) {
  drive_download(
    file = drive_ls(folder, pattern = target),
    path = file.path(local_dir, target),
    overwrite = TRUE
  )
}

# --- LIBRARIES ---
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyterra)
library(INLA)
library(inlabru)
library(fmesher)

# --- SETTINGS ---
MODEL_RDS   <- file.path(local_dir, "inla.st.etag.2to1.rds")
SCALING_RDS <- file.path(local_dir, "scaling_params_2026.02.05.rds")
GRID_DIR    <- "model_prediction/gulf/predictions"
OUTPUT_DIR  <- "model_prediction/gulf/predictions"
GOM_CRS     <- 32616 

# Date Logic
date_forecast <- Sys.Date() + 1
date_str      <- as.character(date_forecast)
target_month  <- as.numeric(format(date_forecast, "%m"))

# Load and "Fix" Model Metadata
model <- readRDS(MODEL_RDS)
scaling_params <- readRDS(SCALING_RDS)

class(model) <- c("bru", "iinla", "inla", "list")
model$bru_info$inlabru_version <- package_version("2.12.0")
attr(model$bru_info, "upgraded") <- TRUE

# --- DATA PREP & PREDICTION ---
# [Your existing environmental layer loading logic here...]

# Run Prediction
message("Running INLA prediction engine...")
pred_formula <- as.formula(paste0("~ plogis(intercept_etag + ", 
                                  paste(env_vars, collapse = " + "), 
                                  " + etag_field)"))

inla_pred <- predict(model, newdata = pred_sf, formula = pred_formula, 
                     n.samples = 50, num.threads = 2)

# --- EXPORT & MAPPING ---
# [Your existing grid_template assignment and writeRaster logic here...]

message("Success! Predictions mapped and saved.")

# Cleanup Downloaded Model to keep Repo clean
file.remove(MODEL_RDS)
file.remove(SCALING_RDS)
