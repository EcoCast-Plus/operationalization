# Script for downloading CMEMS data using GitHub Actions workflow

library(dplyr)
library(purrr)
library(readr)
library(glue)
library(lubridate)

# Load metadata
meta <- read_csv("metadata/model_metadata.csv")

# Define output directories
ncdir_cmems = "data_acquisition/netcdfs/cmems_ncdfs"
if (!dir.exists(ncdir_cmems)) dir.create(ncdir_cmems, recursive = TRUE)

# ----------------------------------------------------------------
# DATE LOGIC
# ----------------------------------------------------------------
date_forecast <- Sys.Date() + 1
date_obs      <- Sys.Date() - 1

message(glue("Target Dates -> Forecasts: {date_forecast} | Observations: {date_obs}"))

# ----------------------------------------------------------------
# TOOL PATH CHECK
# ----------------------------------------------------------------
tool_path <- Sys.getenv("CM_TOOL_PATH")
if (tool_path == "") tool_path <- "/usr/share/miniconda/envs/test/bin/copernicusmarine"
message(glue("Using Copernicus Tool at: {tool_path}"))

# ----------------------------------------------------------------
# REGIONAL BOUNDING BOXES
# ----------------------------------------------------------------
# 1. Gulf of Mexico
bbox_gulf <- "--minimum-longitude -98 --maximum-longitude -81 --minimum-latitude 18 --maximum-latitude 31"

# 2. Top Predator / West Coast (Default)
bbox_toppred <- "--minimum-longitude -160 --maximum-longitude -110 --minimum-latitude 10 --maximum-latitude 60"


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
    
    purrr::walk(cmems_rows, function(x) {
      
      # 1. Determine Date
      target_date <- if (grepl("obs", x$product, ignore.case = TRUE)) date_obs else date_forecast
      
      # 2. Determine Filename (Handle NAs)
      # If model_var_name is missing (NA), use the raw variable name
      name_tag <- if(is.na(x$model_var_name)) x$variable else x$model_var_name
      unique_savename <- glue("{x$product}_{name_tag}_{target_date}")
      
      # Full path for checking existence
      outfile_path <- file.path(ncdir_cmems, paste0(unique_savename, ".nc"))
      
      # [CRITICAL FIX] Delete file if it exists to prevent _(1) duplicates
      if (file.exists(outfile_path)) {
        message(glue("Deleting old file: {unique_savename}.nc"))
        file.remove(outfile_path)
      }
      
      message(glue("Downloading {unique_savename} (Date: {target_date})..."))
      
      # 3. Determine Region
      subset_cmd <- if (x$model == "Gulf_Model") bbox_gulf else bbox_toppred
      
      # 4. Construct Command (Removed --force-download as it's deprecated/ignored)
      cmd <- glue("{tool_path} subset -i {x$product} -v {x$variable} -t {target_date} -T {target_date} -o {ncdir_cmems} -f {unique_savename} {subset_cmd}")
      
      # Conditionally add depth flags
      if (!is.na(x$depth_min)) {
        cmd <- paste(cmd, glue("--minimum-depth {x$depth_min}"))
      }
      if (!is.na(x$depth_max)) {
        cmd <- paste(cmd, glue("--maximum-depth {x$depth_max}"))
      }
      
      # 5. Execute
      exit_code <- system(cmd)
      
      if (exit_code != 0) {
        warning(glue("Download warning for {unique_savename} (Code {exit_code})"))
      }
    })
    
  },
  error = function(e){
    message("Critical error in CMEMS download loop")
    print(e)
    stop(e)
  }
)
