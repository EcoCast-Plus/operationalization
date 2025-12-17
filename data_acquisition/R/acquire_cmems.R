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
      
      # 2. Determine Filename
      unique_savename <- glue("{x$product}_{x$model_var_name}_{target_date}")
      
      message(glue("Downloading {unique_savename} (Date: {target_date})..."))
      
      # 3. Construct Command
      # [FIXED FLAGS]
      # -v : Variable name (was -x)
      # --minimum-depth : (was --min-depth)
      # --maximum-depth : (was --max-depth)
      
      cmd <- glue("{tool_path} subset -i {x$product} -v {x$variable} -t {target_date} -T {target_date} -o {ncdir_cmems} -f {unique_savename} --force-download")
      
      # Conditionally add depth flags ONLY if they are not NA
      if (!is.na(x$depth_min)) {
        cmd <- paste(cmd, glue("--minimum-depth {x$depth_min}"))
      }
      if (!is.na(x$depth_max)) {
        cmd <- paste(cmd, glue("--maximum-depth {x$depth_max}"))
      }
      
      # 4. Execute
      exit_code <- system(cmd)
      
      # Note: Copernicus tool might return non-zero for warnings, so we just warn instead of hard stopping
      if (exit_code != 0) {
        warning(glue("Download process returned exit code {exit_code} for {unique_savename}. Check if file exists."))
      }
    })
    
  },
  error = function(e){
    message("Critical error in CMEMS download loop")
    print(e)
    stop(e)
  }
)
