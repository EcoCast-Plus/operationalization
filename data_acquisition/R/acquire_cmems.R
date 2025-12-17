# Script for downloading CMEMS data using GitHub Actions workflow

library(dplyr)
library(purrr)
library(readr)
library(glue)

source("data_acquisition/R/acquire_utils.R")

# Load metadata
meta <- read_csv("metadata/model_metadata.csv")

# Define output directories
ncdir_cmems = "data_acquisition/netcdfs/cmems_ncdfs"

# Define date of interest (Tomorrow, to match process/predict scripts)
get_date <- Sys.Date() + 1
message(glue("Acquiring CMEMS data for target date: {get_date}"))


###############
#### cmems ####
###############

# Define CMEMS metadata object
meta_cmems <- meta |>
  filter(data_type == 'CMEMS',
         category != 'derived' | is.na(category)) 

# Transform to list and add exported file names
cmems_product_list <- meta_cmems |>
  mutate(
    # Create unique filename. 
    # Important: Other scripts look for pattern: "{product}_{variable}_{date}.nc"
    # or rely on regex that matches the variable name.
    savename = glue("{product}_{variable}_{get_date}")
  ) |>
  split(~variable) # Be careful: this groups multiple depths of 'thetao' together!


# We need to iterate carefully to handle rows with same variable but different depths (e.g. thetao 0m, 150m, 500m)
# Splitting by row number is safer than splitting by variable name here.
cmems_rows <- meta_cmems |>
  mutate(savename = glue("{product}_{variable}_{get_date}")) |>
  split(1:nrow(meta_cmems))


tryCatch(
  expr ={
    
    # Download netCDF files if available
    # Iterate over every ROW in the CSV, not just unique variable names
    purrr::walk(cmems_rows, function(x) {
      
      # Use depth from CSV if available, otherwise default to surface/null
      d_min <- if(!is.na(x$depth_min)) x$depth_min else NULL
      d_max <- if(!is.na(x$depth_max)) x$depth_max else NULL
      
      # For files with distinct depths (like thetao 150m), we need distinct filenames
      # otherwise they overwrite each other!
      # We append the model_var_name to the filename to make it unique (e.g. thetao_150m)
      unique_savename <- glue("{x$product}_{x$model_var_name}_{get_date}")
      
      message(glue("Downloading {unique_savename} (Depth: {d_min}-{d_max})..."))
      
      download_cmems(
        # Use the environment variable we set in the YAML for the tool path
        cmems_user_env = Sys.getenv("CM_TOOL_PATH", "/usr/share/miniconda/envs/test/bin/copernicusmarine"),
        ncdir_cmems = ncdir_cmems,
        product_cmems = x$product,
        variable_cmems = x$variable,
        savename_cmems = unique_savename,
        get_date = get_date,
        var_depth_min = d_min,
        var_depth_max = d_max
      )
    })
    
  },
  error = function(e){
    message(glue("Error downloading CMEMS data for {get_date}"))
    print(e)
    # Stop so the workflow knows it failed
    stop(e)
  }
)
