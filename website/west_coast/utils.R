
#' Function to check status of environmental data from ROMS THREDDS server
#'
#' Detect whether data is available from the ROMS THREDDS server (by testing w/ SST).
#'
#' @param get_date Date of interest in YYYY-MM-DD format.
#'
#' @return A Boolean value (TRUE/FALSE) as to whether data is available w/in the past 5 days (due to 4-day data release cycle)
#'
#' @export
check_roms = function(get_date) {
  
  # Define number of days since ref date (2011-01-02) for url index
  ref_date <- lubridate::dmy('02-01-2011')
  new_date <- as.Date(get_date)
  days <- as.numeric(difftime(new_date, ref_date))
  
  # Define url for data download
  my_url <- glue::glue("https://oceanmodeling.ucsc.edu/thredds/dodsC/ccsra_2016a_phys_agg_derived_vars/fmrc/CCSRA_2016a_Phys_ROMS_Derived_Variables_Aggregation_best.ncd?sst%5B{days}:1:{days}%5D%5B0:1:180%5D%5B0:1:185%5D,lat_rho%5B0:1:180%5D%5B0:1:185%5D,lon_rho%5B0:1:180%5D%5B0:1:185%5D,time%5B0:1:1%5D")
  
  # Download data and open as R object
  nc.data <- ncdf4::nc_open(my_url, return_on_error = TRUE, verbose = FALSE)
  
  # Store result of server check (that server is running)
  res <- nc.data$error == FALSE
  
  
  return(res)
  
}




#' Function to check status of environmental data from CMEMS server
#'
#' Detect whether data is available from the CMEMS server (by testing each product ID).
#'
#' @param path_copernicus_marine_toolbox File path to locally stored version of Copernicus Marine Toolbox. In copernicusmarine v2.0.0 on personal machine, only need to call `copernicusmarine` instead of full path to binary file.
#' @param productID Product name of interest from CMEMS.
#' @param get_date Date of interest in YYYY-MM-DD format.
#'
#' @return A Boolean value (TRUE/FALSE) as to whether data is available w/in the past 5 days (due to 4-day data release cycle)
#'
#' @export
check_cmems = function(path_copernicus_marine_toolbox = "copernicusmarine",
                       productID, get_date) {
  
  # Define command for querying metadata
  command <- glue::glue("{path_copernicus_marine_toolbox} describe -i {productID} -r coordinate_id,coordinate_unit,minimum_value,maximum_value")
  meta <- system(command, intern = TRUE)
  meta.json <- jsonlite::parse_json(meta)
  
  #Access time data
  dims <- meta.json$products[[1]]$datasets[[1]]$versions[[1]]$parts[[1]]$services[[3]]$variables[[1]]$coordinates
  
  # Check to see where time is stored; varies by product
  if (dims[[1]]$coordinate_id == "time" & grepl("^milliseconds", dims[[1]]$coordinate_unit)) {
    
    max_date <- lubridate::as_datetime(dims[[1]]$maximum_value / 1000)
    
  } else if (dims[[4]]$coordinate_id == "time" & grepl("^milliseconds", dims[[4]]$coordinate_unit)) {
    
    max_date <- lubridate::as_datetime(dims[[4]]$maximum_value / 1000)
    
  } else {
    stop("Need to fix bug in parsing of time")
  }
  
  
  # Store result of server check (that server is running)
  res <- max_date >= get_date - 7  #check if data available w/in 7 days of current date
  
  
  return(res)
  
}
