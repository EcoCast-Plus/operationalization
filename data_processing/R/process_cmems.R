# Script for processing CMEMS data using GitHub Actions workflow

library(glue)
library(terra)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)

source("data_processing/R/process_utils.R")

# Load metadata
meta <- read_csv("metadata/model_metadata.csv")

# Define paths
ncdir_TopPred <- "data_acquisition/netcdfs/cmems_ncdfs"
outdir_TopPred <- "data_processing/TopPredatorWatch/rasters"

# Ensure output directory exists
if (!dir.exists(outdir_TopPred)) dir.create(outdir_TopPred, recursive = TRUE)

# ----------------------------------------------------------------
# DATE LOGIC (Must match acquire_cmems.R)
# ----------------------------------------------------------------
date_forecast <- Sys.Date() + 1
date_obs      <- Sys.Date() - 1

message(glue("Processing Target Dates -> Forecasts: {date_forecast} | Observations: {date_obs}"))

# Define raster template
template_TopPred <- rast("data_processing/TopPredatorWatch/static/template.tiff")


########################################################
### Process and resample data for Top Predator Watch ###
########################################################

# Prepare metadata info for I/O
meta_TopPred <- meta |> 
  filter(data_type == 'CMEMS',
         (category != 'derived' | is.na(category)),
         
         # Exclude deep/3D variables (handled by predict_gulf.R)
         !grepl("150m|500m|bottom|so|uo|vo|thetao", model_var_name)
  ) |> 
  mutate(
    # [LOGIC FIX] Determine the correct date for the filename
    file_date = ifelse(grepl("obs", product, ignore.case = TRUE), 
                       as.character(date_obs), 
                       as.character(date_forecast)),
    
    savename = case_when(!variable %in% c('ugosa','vgosa') ~ glue('{model_var_name}'),
                              TRUE ~ glue('{variable}')),
    
    # Construct filename using the correct date
    filename = glue("{product}_{variable}_{file_date}.nc")
  ) |> 
  split(~variable)


# Process raster layers
walk(meta_TopPred,
     ~process_vars(infile = .x$filename,
                   indir = ncdir_TopPred,
                   variable = .x$variable,
                   outdir = outdir_TopPred,
                   savename = .x$savename,
                   get_date = as.Date(.x$file_date), # Pass specific date to function
                   template = template_TopPred,
                   tool = "TopPredatorWatch"),
     .progress = TRUE
)


######################################################
### Generate derived covars for Top Predator Watch ###
######################################################

# Prepare metadata info for I/O
TopPred_meta_derived <- meta |> 
  filter(data_type == 'CMEMS',
         category == 'derived') |> 
  mutate(savename = glue('{model_var_name}')) |> 
  split(~variable)


# Calculate derived variables
# Note: For derived vars, we usually use the forecast date (or the date of the primary input).
# If derived vars depend on OBS data (like EKE from SLA), this might need adjustment.
# For now, we assume derived vars are based on the main 'get_date' logic or exist for both.
# We will default to date_forecast for consistency with the main model run, 
# but if EKE fails, we might need to switch this to date_obs.
walk(TopPred_meta_derived,
     ~calc_derived_vars(dir = outdir_TopPred,
                        variable = .x$variable,
                        savename = .x$savename,
                        get_date = date_forecast, # Defaulting to forecast date
                        tool = "TopPredatorWatch"),
     .progress = TRUE
)
