
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
get_file_dates <- function(folder, pattern, strip_pattern, strip = TRUE) {
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
      layer = if (strip) str_remove(name, strip_pattern) else name,
      layer = str_replace(layer, "\\.tiff$", "")
    )
}

# Get prediction rasters
pred_rasters <- get_file_dates(folder = pred_folder,
                               pattern = "^pred_\\d{4}-\\d{2}-\\d{2}_leatherbackTurtle\\.tiff$",
                               strip_pattern = "_\\d{4}-\\d{2}-\\d{2}_leatherbackTurtle\\.tiff$",
                               strip = TRUE)

# Get environmental rasters
env_rasters <- get_file_dates(folder = env_folder,
                              pattern = "\\.tiff$",
                              strip_pattern = "_\\d{4}-\\d{2}-\\d{2}",
                              strip = TRUE) |> 
  filter(!str_detect(name, "day|ugosa|vgosa"))  #remove unneeded variables

# Combine both
all_rasters <- bind_rows(pred_rasters, env_rasters)

# Create a dropdown of available dates (those that appear in all layers)
valid_dates <- intersect(pred_rasters$date, env_rasters$date) |> 
  sort(decreasing = TRUE) |> 
  as.character()


# Define list of palette colors from {cmocean} to viz covars
covar_pal_df <- data.frame(covar = unique(env_rasters$layer),
                           pals = c("algae","tempo","algae","dense","matter","delta","thermal","amp"))




#################
### Define UI ###
#################

ui <- page_fluid(
  # title = "My app",
  # sidebar = sidebar(
  #   title = "Inputs"
  # ),
  # tags$head(
  #   tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
  # ),
  layout_column_wrap(
    width = "200px",
    fixed_width = TRUE,
    heights_equal = "row",
    selectInput(inputId = "date",
                "Select date: ",
                choices = valid_dates,
                selected = valid_dates[1],
                multiple = FALSE,
                width = "150px"),
    
    selectInput(inputId = "covar",
                "Select variable: ",
                choices = unique(env_rasters$layer),
                selected = unique(env_rasters$layer)[1],
                multiple = FALSE,
                width = "150px")
  ),
  
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
      rast() #|> 
      # project('EPSG:3857')  # Reproject so properly mapped by leaflet
  })
  
  covar_rast <- reactive({
    env_rasters |> 
      filter(date == input$date,
             layer == input$covar) |> 
      pull(url) |> 
      rast() |> 
      # project('EPSG:3857') |>  # Reproject so properly mapped by leaflet
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
      addRasterImage(project(pred_rast(), 'EPSG:3857'), colors = pal, opacity = 0.8, group = "HSI", layerId = "HSI") |> 
      addImageQuery(project(pred_rast(), 'EPSG:3857'), layerId = "HSI") |>  #add raster  query
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
                       overlayGroups = c("covar"),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE))
  })

  
  # Add reactive layer to map
  outputOptions(output, "covar_map", suspendWhenHidden = FALSE)  #need for displaying points for map on load
  
  observe({
    req(covar_rast())
    
    # Select palette to use
    covar_pal_name <- covar_pal_df |> 
      filter(covar == input$covar) |> 
      pull(pals)
    
    # Define palette
    covar_pal <- colorNumeric(cmocean(covar_pal_name)(256), domain = values(covar_rast()), na.color = "transparent")
    
    leafletProxy("covar_map") |> 
      clearControls() |> 
      clearImages() |>
      addRasterImage(x = project(covar_rast(), 'EPSG:3857'), colors = covar_pal, opacity = 1, group = "covar") |>
      addImageQuery(project(covar_rast(), 'EPSG:3857'), group = "covar", layerId = "covar") |>  #add raster  query
      addLegend(title = paste(input$covar), position = "bottomright", pal = covar_pal,
                values = values(covar_rast()), layerId = "covar", group = "covar") 
  })
  
}


###############
### Run app ###
###############

shinyApp(ui, server)
