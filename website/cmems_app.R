
# Shiny app to explore CMEMS predictions and covariates by date

library(shiny)
library(bslib)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(glue)
library(leaflet)
library(leafem)
library(terra)
library(cmocean)


#############################
### Check data on GH repo ###
#############################

# GitHub API URLs
base_api <- "https://api.github.com/repos/joshcullen/CEG_operationalization/contents"

# Folders
pred_folder <- "model_prediction/TopPredatorWatch/rasters"
env_folder <- "data_processing/TopPredatorWatch/rasters"

# Function to get file listing from GitHub folder
get_file_dates <- function(folder, pattern, prefix = "", suffix = "", strip = TRUE) {
  res <- GET(glue("{base_api}/{folder}"))
  stop_for_status(res)
  files <- content(res, as = "parsed")
  
  tibble(
    name = sapply(files, `[[`, "name"),
    url = sapply(files, `[[`, "download_url")
  ) |>
    filter(str_detect(name, pattern)) |>
    mutate(date = str_extract(name, "\\d{4}-\\d{2}-\\d{2}")) |>
    filter(!is.na(date)) |>
    mutate(
      layer = if (strip) str_remove(name, paste0(prefix, "|_", date, suffix, "\\.tiff$")) else name,
      layer = str_replace(layer, "\\.tiff$", "")
    )
}

# Get prediction rasters
pred_rasters <- get_file_dates(pred_folder,
                               "^pred_\\d{4}-\\d{2}-\\d{2}_leatherbackTurtle\\.tiff$",
                               "pred_",
                               "_leatherbackTurtle",
                               FALSE)

# Get environmental rasters
env_rasters <- get_file_dates(env_folder, "\\.tiff$")

# Combine both
all_rasters <- bind_rows(pred_rasters, env_rasters)

# Create a dropdown of available dates (those that appear in all layers)
valid_dates <- intersect(pred_rasters$date, env_rasters$date) |> 
  sort(decreasing = TRUE) |> 
  as.character()





#################
### Define UI ###
#################

ui <- page_fluid(
  # title = "My app",
  # sidebar = sidebar(
  #   title = "Inputs"
  # ),
  tags$head(
    tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
  ),
  selectInput(inputId = "date",
              "Select date: ",
              choices = valid_dates,
              selected = valid_dates[1],
              multiple = FALSE,
              width = "50px"),
  
  # Add navset tab
  navset_card_tab(
    nav_panel(title = "Model Prediction",
              card(full_screen = TRUE,
                   leafletOutput("pred_map")
              )
    ),
    
    nav_panel(title = "Environ. Predictors",
              card(full_screen = TRUE,
                   leafletOutput("covar_map")
              )
    )
  )
)


#####################
### Define server ###
#####################

server <- function(input, output, session) {
  
  ## Filter data based on select input
  pred_rast <- reactive({
    pred_rasters |> 
      filter(date == input$date) |> 
      pull(url) |> 
      rast() |> 
      project('EPSG:3857')  # Reproject so properly mapped by leaflet
  })
  
  covar_rast <- reactive({
    env_rasters |> 
      filter(date == input$date,
             !str_detect(name, "day|ugosa|vgosa")) |> 
      pull(url) |> 
      rast() |> 
      project('EPSG:3857') |>  # Reproject so properly mapped by leaflet
      crop(pred_rast())  #crop to match same extent as prediction
  })
  
   
  
  ## Map prediction layer
  
  # Create basemap for prediction
  output$pred_map <- renderLeaflet({
    leaflet() |> 
      setView(lng = -130, lat = 30, zoom = 3) |> 
      addProviderTiles(provider = providers$CartoDB.DarkMatter, group = "CartoDB",
                       options = providerTileOptions(zIndex = -10)) |> 
      addProviderTiles(provider = providers$Esri.WorldImagery, group = "Satellite",
                       options = providerTileOptions(zIndex = -10)) |> 
      addProviderTiles(provider = providers$Esri.OceanBasemap, group = "Bathymetry",
                       options = providerTileOptions(zIndex = -10)) |> 
      addLayersControl(position = "topright",
                       baseGroups = c("CartoDB","Satellite","Bathymetry"),
                       overlayGroups = "HSI",
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE))
    })
  
  # Add reactive layer to map
  observe({
    # Define color palette for raster layers
    pal <- colorNumeric("inferno", domain = values(pred_rast()), na.color = "transparent")
    
    leafletProxy("pred_map") |> 
      clearControls() |> 
      clearImages() |>
      addRasterImage(pred_rast(), colors = pal, opacity = 0.8, group = "HSI", layerId = "HSI") |> 
      addImageQuery(pred_rast(), layerId = "HSI") |>  #add raster  query
      addLegend(title = "Habitat Suitability", position = "bottomright", pal = pal, values = values(pred_rast()))
  })
  
  
  ## Map covar layers
  
  # Create basemap for covar layers
  output$covar_map <- renderLeaflet({
    leaflet() |>
      setView(lng = -130, lat = 30, zoom = 3) |>
      addProviderTiles(provider = providers$CartoDB.DarkMatter, group = "CartoDB",
                       options = providerTileOptions(zIndex = -10)) |>
      addProviderTiles(provider = providers$Esri.WorldImagery, group = "Satellite",
                       options = providerTileOptions(zIndex = -10)) |>
      addProviderTiles(provider = providers$Esri.OceanBasemap, group = "Bathymetry",
                       options = providerTileOptions(zIndex = -10)) |>
      addLayersControl(position = "topright",
                       baseGroups = c("CartoDB","Satellite","Bathymetry"),
                       overlayGroups = c("PPupper200m","log(EKE)","log(Chla)","MLD","Oxy200m","SLA","SST","SST_SD"),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE))
  })

  
  # Add reactive layer to map
  outputOptions(output, "covar_map", suspendWhenHidden = FALSE)  #need for displaying points for map on load
  
  observe({
    # req(covar_rast())
    
    # Define color palette for raster layers
    npp.pal <- colorNumeric(cmocean("algae")(256), domain = values(covar_rast()[[1]]), na.color = "transparent")
    eke.pal <- colorNumeric(cmocean("tempo")(256), domain = values(covar_rast()[[2]]), na.color = "transparent")
    chl.pal <- colorNumeric(cmocean("algae")(256), domain = values(covar_rast()[[3]]), na.color = "transparent")
    mld.pal <- colorNumeric(cmocean("dense")(256), domain = values(covar_rast()[[4]]), na.color = "transparent")
    o2.pal <- colorNumeric(cmocean("matter")(256), domain = values(covar_rast()[[5]]), na.color = "transparent")
    sla.pal <- colorNumeric(cmocean("delta")(256), domain = values(covar_rast()[[6]]), na.color = "transparent")
    sst.pal <- colorNumeric(cmocean("thermal")(256), domain = values(covar_rast()[[7]]), na.color = "transparent")
    sst_sd.pal <- colorNumeric(cmocean("amp")(256), domain = values(covar_rast()[[8]]), na.color = "transparent")

    leafletProxy("covar_map") |> 
      clearControls() |> 
      clearImages() |>
      addRasterImage(x = covar_rast()[[1]], colors = npp.pal, opacity = 1, group = "PPupper200m",
                   layerId = "PPupper200m") |>
      addImageQuery(covar_rast()[[1]], layerId = "PPupper200m") |>  #add raster  query
      addLegend(title = "PPupper200m", position = "bottomright", pal = npp.pal,
                values = values(covar_rast()[[1]]), group = "PPupper200m") |>

      addRasterImage(covar_rast()[[2]], colors = eke.pal, opacity = 1, group = "log(EKE)", layerId = "log(EKE)") |>
      addImageQuery(covar_rast()[[2]], layerId = "log(EKE)") |>  #add raster  query
      addLegend(title = "log(EKE)", position = "bottomright", pal = eke.pal, values = values(covar_rast()[[2]]),
                group = "log(EKE)") |>

      addRasterImage(covar_rast()[[3]], colors = chl.pal, opacity = 1, group = "log(Chla)",
                     layerId = "log(Chla)") |>
      addImageQuery(covar_rast()[[3]], layerId = "log(Chla)") |>  #add raster  query
      addLegend(title = "log(Chla)", position = "bottomright", pal = chl.pal,
                values = values(covar_rast()[[3]]), group = "log(Chla)") |>

      addRasterImage(covar_rast()[[4]], colors = mld.pal, opacity = 1, group = "MLD", layerId = "MLD") |>
      addImageQuery(covar_rast()[[4]], layerId = "MLD") |>  #add raster  query
      addLegend(title = "MLD", position = "bottomright", pal = mld.pal, values = values(covar_rast()[[4]]),
                group = "MLD") |>

      addRasterImage(covar_rast()[[5]], colors = o2.pal, opacity = 1, group = "Oxy200m",
                     layerId = "Oxy200m") |>
      addImageQuery(covar_rast()[[5]], layerId = "Oxy200m") |>  #add raster  query
      addLegend(title = "Oxy200m", position = "bottomright", pal = o2.pal,
                values = values(covar_rast()[[5]]), group = "Oxy200m") |>

      addRasterImage(covar_rast()[[6]], colors = sla.pal, opacity = 1, group = "SLA", layerId = "SLA") |>
      addImageQuery(covar_rast()[[6]], layerId = "SLA") |>  #add raster  query
      addLegend(title = "SLA", position = "bottomright", pal = sla.pal, values = values(covar_rast()[[6]]),
                group = "SLA") |>

      addRasterImage(covar_rast()[[7]], colors = sst.pal, opacity = 1, group = "SST", layerId = "SST") |>
      addImageQuery(covar_rast()[[7]], layerId = "SST") |>  #add raster  query
      addLegend(title = "SST", position = "bottomright", pal = sst.pal, values = values(covar_rast()[[7]]),
                group = "SST") |>

      addRasterImage(covar_rast()[[8]], colors = sst_sd.pal, opacity = 1, group = "SST_SD",
                     layerId = "SST_SD") |>
      addImageQuery(covar_rast()[[8]], layerId = "SST_SD") |>  #add raster  query
      addLegend(title = "SST_SD", position = "bottomright", pal = sst_sd.pal,
                values = values(covar_rast()[[8]]), group = "SST_SD") |>

      hideGroup(c("log(EKE)","log(Chla)","MLD","Oxy200m","SLA","SST","SST_SD"))
  })
  
}


###############
### Run app ###
###############

shinyApp(ui, server)
