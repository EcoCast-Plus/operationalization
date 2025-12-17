# Script for downloading CMEMS data using GitHub Actions workflow

library(dplyr)
library(purrr)
library(readr)
library(glue)
library(lubridate)

source("data_acquisition/R/acquire_utils.R")

# Load metadata
meta <- read_csv("metadata/model_metadata.csv")

# Define output directories
ncdir_cmems = "data_acquisition/netcdfs/cmems_ncdfs"

# ----------------------------------------------------------------
# DATE LOGIC FIX
# ----------------------------------------------------------------
# We need two different dates:
# 1. Forecast Date: For Models (Physics) -> Tomorrow
# 2. Observation Date: For Satellites (Chl, SLA) -> Yesterday (latest available)
# ----------------------------------------------------------------
date_forecast <- Sys.Date() + 1
date_obs      <- Sys.Date() - 1

message(glue("Target Dates -> Forecasts: {date_forecast} | Observations: {date_obs}"))


###############
#### cmems ####
###############

# Define CMEMS metadata object
meta_cmems <- meta |>
  filter(data_type == 'CMEMS',
         category != 'derived' | is.na(category)) 

# Create the row-based list for iteration
cmems_rows <- meta_cmems |>
  split(1:nrow(meta_cmems))

tryCatch(
  expr ={
    
    # Download netCDF files if available
    purrr::walk(cmems_rows, function(x) {
      
      # [LOGIC] Determine which date to use based on product type
      # If the product name contains "obs", it's satellite data (use past date).
      # Otherwise, assume it's a model (use forecast date).
      target_date <- if (grepl("obs", x$product, ignore.case = TRUE)) date_obs else date_forecast
      
      # Handle depths
      d_min <- if(!is.na(x$depth_min)) x$depth_min else NULL
      d_max <- if(!is.na(x$depth_max)) x$depth_max else NULL
      
      # Define filename with the CORRECT date for that file
      unique_savename <- glue("{x$product}_{x$model_var_name}_{target_date}")
      
      message(glue("Downloading {unique_savename} (Date: {target_date})..."))
      
      # [CODE FIX] Removed 'cmems_user_env =' name. Passed path as first argument.
      download_cmems(
        Sys.getenv("CM_TOOL_PATH", "/usr/share/miniconda/envs/test/bin/copernicusmarine"), 
        ncdir_cmems,
        x$product,
        x$variable,
        unique_savename,
        target_date,
        d_min,
        d_max
      )
    })
    
  },
  error = function(e){
    message("Critical error in CMEMS download loop")
    print(e)
    stop(e)
  }
)
