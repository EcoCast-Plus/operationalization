library(shiny)
library(tidyverse)
library(sf)
library(terra)
library(leaflet)        
library(leaflet.extras)
library(bslib)          
library(httr)
library(jsonlite)
library(stringr)
library(rmarkdown)
library(knitr)

# --- 1. CONFIGURATION ---
GITHUB_REPO_API <- "https://api.github.com/repos/EcoCast-Plus/operationalization/contents"
PRED_FOLDER_PATH <- "model_prediction/gulf/predictions"
GITHUB_ISSUES_URL <- "https://github.com/EcoCast-Plus/operationalization/issues"

# --- HELPER FUNCTION FOR CHECKBOXES (UPDATED) ---
species_checkbox <- function(id, label, value = TRUE, type = "target", weight = 1.0) {
  # Format weight to 2 decimal places
  weight_text <- sprintf("%.2f", weight)
  
  div(style = "margin-bottom: 12px;",
      # Use flexbox to push the species name to the left and the weight to the right
      div(class="species-label", style="display: flex; justify-content: space-between; align-items: center;", 
          span(label),
          span(style="color: #666; font-size: 0.8rem; font-weight: normal;", paste0("Weight: ", weight_text))
      ),
      if(type == "target") {
        checkboxInput(id, "Include in calculation", value = value)
      }
      # Removed the "Required" text from here
  )
}

# --- 2. UI ---
ui <- page_sidebar(
  title = "GulfCast",
  theme = bs_theme(version = 5, bootswatch = "zephyr"),
  
  withMathJax(),
  
  tags$head(
    tags$style(HTML("
      .selectize-dropdown { z-index: 10000 !important; }
      .species-label { font-weight: 600; font-size: 0.9rem; margin-bottom: 2px; color: #333; }
      .column-header { font-weight: bold; font-size: 1.0rem; border-bottom: 2px solid #eee; margin-bottom: 15px; padding-bottom: 5px; }
      .date-label { font-size: 0.85rem; color: #666; margin-top: 5px; font-style: italic; }
      .methods-header { color: #2c3e50; border-bottom: 2px solid #ecf0f1; padding-bottom: 10px; margin-top: 20px;}
      .faq-q { font-weight: bold; color: #005da2; margin-top: 15px; }
      .faq-a { margin-bottom: 15px; margin-left: 10px; border-left: 3px solid #eee; padding-left: 10px; }
    "))
  ),
  
  sidebar = sidebar(
    width = 300,
    h5("1. Select Date"),
    uiOutput("date_selector_ui"),
    uiOutput("download_status"),
    hr(),
    
    h5("2. Fishing Target"),
    selectInput("display_objective", "Select Fishery:", 
                choices = c("Swordfish Target (night)" = "Swordfish_Target", 
                            "Yellowfin Target (day)" = "Yellowfin_Target")),
    
    hr(),
    h5("Map Layers"),
    checkboxInput("show_fishing_area", "Hist. Fishing Area", value = FALSE),
    checkboxInput("show_desoto_canyon", "DeSoto Canyon", value = FALSE),
    checkboxInput("show_leatherback_core", "Leatherback Core Habitat", value = TRUE),
    uiOutput("leatherback_warning"),
    
    # --- REPORT DOWNLOAD SECTION ---
    hr(),
    h5("3. Download Report"),
    radioButtons("report_format", "Format:", choices = c("HTML", "PDF"), inline = TRUE),
    downloadButton("downloadReport", "Generate Report", class = "btn-primary w-100")
  ),
  
  navset_card_underline(
    id = "main_tabs", 
    title = NULL, 
    
    # --- TAB 1: INTEGRATED MAP ---
    nav_panel("Integrated Multispecies Map",
              layout_columns(
                col_widths = breakpoints(
                  sm = 12, 
                  lg = c(8, 4)
                ),
                
                # --- LEFT/TOP: THE MAP ---
                card(
                  full_screen = TRUE,
                  height = "750px",
                  card_header(
                    class = "d-flex justify-content-between align-items-center",
                    span("Integrated Suitability"),
                    # --- ADDED: FORMAT DROPDOWN FOR KMZ ---
                    div(class = "d-flex align-items-center", style = "gap: 10px;",
                        selectInput("format_int", NULL, choices = c("GeoTIFF" = ".tif", "KMZ" = ".kmz"), width = "100px", selectize = FALSE),
                        downloadButton("downloadIntegrated", "Download Map", class = "btn-sm btn-light")
                    )
                  ),
                  leafletOutput("integratedLeafletMap", height = "100%", width = "100%")
                ),
                
                # --- RIGHT/BOTTOM: THE CONTROLS ---
                card(
                  height = "750px",
                  style = "overflow-y: auto;", 
                  card_header("Species Selection"),
                  
                  div(
                    selectInput("calc_objective", "Fishery Objective:", 
                                choices = c("Swordfish Target (night)" = "Swordfish_Target", "Yellowfin Target (day)" = "Yellowfin_Target"), 
                                width = "100%")
                  ),
                  
                  hr(),
                  
                  # Dynamic UI for checkboxes
                  uiOutput("species_selector_ui")
                )
              )
    ),
    
    # --- TAB 2: SINGLE LAYER ---
    nav_panel("Single Layer Map",
              layout_columns(
                col_widths = breakpoints(sm = 12, lg = 6),
                selectInput("fisheries_layer", "Select Prediction", choices = c("Loading..." = "")),
                selectInput("env_layer", "Select Environmental Layer", choices = c("None" = ""))
              ),
              card(
                full_screen = TRUE, 
                height = "600px", 
                leafletOutput("leafletMap", height = "100%")
              ),
              
              # --- ADDED: FORMAT DROPDOWN FOR KMZ ---
              div(style = "margin-top: 10px; display: flex; align-items: center; gap: 10px;", 
                  selectInput("format_single", NULL, choices = c("GeoTIFF" = ".tif", "KMZ" = ".kmz"), width = "100px", selectize = FALSE),
                  downloadButton("downloadSingleMap", "Download Map", class = "btn-sm"),
                  uiOutput("layer_date_info", inline = TRUE, style = "margin-left: 10px;")
              )
    ),
    
    # --- TAB 3: USER GUIDE & FAQs (UPDATED TEXT) ---
    nav_panel("User Guide & FAQs",
              card(
                height = "750px",
                style = "overflow-y: auto; padding: 20px;",
                h2("User Guide & FAQ"),
                
                h3("Part 1: User Instructions"),
                
                h4("1. Getting Started"),
                p("GulfCast is a decision-support tool designed to help fishers in the Gulf of America identify optimal fishing grounds. It balances the probability of catching target species (swordfish and yellowfin tuna) against the risk of interacting with protected bycatch species (bluefin tuna, billfish, sharks and sea turtles)."),
                
                h4("2. Dashboard Navigation"),
                
                h5("Step 1: Select Date"),
                tags$ul(
                  tags$li("Navigate to the Sidebar on the left."),
                  tags$li("Use the dropdown menu under '1. Select Date' to choose the forecast date."),
                  tags$li(strong("Note:"), " The app requires an internet connection to download the latest environmental and prediction data from the server. Wait for the 'Data loaded' message in green before proceeding.")
                ),
                
                h5("Step 2: Choose Fishery Objective"),
                p("Under '2. Fishing Target', select your primary goal:"),
                tags$ul(
                  tags$li(strong("Swordfish Target (night):"), " Optimizes for Swordfish gear configurations (light sticks, night fishing)."),
                  tags$li(strong("Yellowfin Target (day):"), " Optimizes for Yellowfin Tuna gear configurations (no light sticks, day fishing)."),
                  tags$li(strong("Note:"), " Changing this selection will automatically reset target species preferences to default values for that fishery.")
                ),
                
                h5("Step 3: Configure Map Layers"),
                p("Toggle the checkboxes to overlay reference boundaries on the map:"),
                tags$ul(
                  tags$li(strong("Hist. Fishing Area:"), " Shows the general extent of historical fishing efforts."),
                  tags$li(strong("DeSoto Canyon:"), " Outlines the restricted gear zones."),
                  tags$li(strong("Leatherback Core Habitat:"), " Displays active core habitat zones (dark green) for Leatherback Turtles.")
                ),
                
                h4("3. Using the Integrated Map"),
                p("The Integrated Multispecies Map tab is the main decision tool."),
                tags$ul(
                  tags$li(strong("Select Targets:"), " On the right-hand panel, you can check which target species you are actively pursuing. All bycatch species are mandatory and cannot be disabled."),
                  tags$li(strong("Reliability Weighting:"), " You can select any target species. Behind the scenes, the model scales each species' influence based on its seasonal reliability score. Poorly performing models are mathematically down-weighted so they don't skew your map."),
                  tags$li(strong("Interpreting the Map:"), 
                          tags$ul(
                            tags$li("Blue Areas: High suitability (High probability of target catch + Low probability of bycatch)."),
                            tags$li("Red Areas: Low suitability (High bycatch risk)."),
                            tags$li("White/Grey Areas: Neutral suitability.")
                          )
                  ),
                  tags$li(strong("Download Map:"), " Click the 'Download Map' button at the top right of the map to save the current suitability surface for use in other GIS software.")
                ),
                
                h4("4. Inspecting Single Layers"),
                p("Navigate to the 'Single Layer Map' tab to view the modeled surfaces and environmental data driving the integrated model."),
                tags$ul(
                  tags$li(strong("Select Prediction:"), " Choose a specific species (e.g., 'Yellowfin Tuna') to see its individual probability map (0 to 1)."),
                  tags$li(strong("Select Environmental Layer:"), " Choose a variable (e.g., 'Sea Surface Temperature,' 'Chlorophyll-a') to view oceanographic conditions."),
                  tags$li(strong("Download:"), " Use the 'Download Map' button to save the specific layer you are viewing.")
                ),
                
                h4("5. Generating Reports"),
                p("To save a summary of the day's conditions:"),
                tags$ul(
                  tags$li("Go to the Sidebar under '3. Download Report'."),
                  tags$li("Select your preferred format (HTML for web viewing or PDF for printing)."),
                  tags$li("Click 'Generate Report'. This will compile the Integrated Map, all active Species Maps, and Environmental Maps into a single document.")
                ),
                
                hr(),
                
                h3("Part 2: Frequently Asked Questions (FAQs)"),
                
                div(class="faq-q", "Q: What happens if a species model is unreliable for the current season?"),
                div(class="faq-a", "A: We assess the accuracy of our models for every species and season (Quarter) using historical data. Instead of removing them entirely, we use a technique called Reliability Weighting. Each species' probability surface is multiplied by its reliability score. If a model performs poorly (e.g., TSS near 0), its mathematical weight drops to near zero, effectively muting its impact on the final map without requiring you to manually exclude it."),
                
                div(class="faq-q", "Q: What do the colors on the Integrated Map mean?"),
                div(class="faq-a", "A: The map uses a 'Traffic Light' scale:",
                    tags$ul(
                      tags$li(strong("Blue:"), " Ideal fishing grounds. These areas have high predicted target abundance and low bycatch risk."),
                      tags$li(strong("White:"), " Neutral. Either low activity for both targets and bycatch, or the target reward is canceled out by the bycatch risk."),
                      tags$li(strong("Red:"), " Avoidance zones. These areas have a high probability of bycatch interaction.")
                    )
                ),
                
                div(class="faq-q", "Q: How is the 'Integrated Score' calculated?"),
                div(class="faq-a", "A: We take the probability of finding your selected target species and subtract the probability of interacting with bycatch species. Crucially, each species layer is multiplied by its model's reliability score (meaning highly accurate models have more 'voting power' than less accurate ones). We also mathematically balance the total weight of the targets against the total weight of the bycatch so the map always remains perfectly neutral when the risks and rewards are equal."),
                
                div(class="faq-q", "Q: Can I use this app offline?"),
                div(class="faq-a", "A: No. The app needs an active internet connection to fetch the latest daily prediction files and environmental data from our remote repository. This requires 1.8 MB of data. You can download the report to access all static maps offline."),
                
                div(class="faq-q", "Q: I found a bug or have a suggestion. Who do I contact?"),
                div(class="faq-a", "A: Please navigate to the 'Report Issues' tab in the application. Click the 'Open an Issue on GitHub' button to submit a report directly to our development team. Please include the date and layer you were viewing when the error occurred.")
              )
    ),
    
    # --- TAB 4: METHODS ---
    nav_panel("Methods",
              card(
                height = "750px",
                style = "overflow-y: auto; padding: 20px;",
                h2("Methodology & Data Sources"),
                p("This application provides daily forecasts of fishing suitability in the Gulf of America, balancing the catch of target species against the risk of interacting with protected bycatch species."),
                
                h4("1. Data Sources", class = "methods-header"),
                p("Environmental data is sourced daily from the Copernicus Marine Service (CMEMS). Key dynamic and static variables include:"),
                tags$ul(
                  tags$li(strong("Physical Physics:"), " Sea Water Temperature (surface, 150m, 500m), Bottom Water Temperature, Sea Surface Height (SSH), Mixed Layer Depth, Salinity, and Current Velocities (U/V)."),
                  tags$li(strong("Biogeochemistry:"), " Chlorophyll-a concentration."),
                  tags$li(strong("Derived Features:"), " SST/SSH Anomalies, Eddy Kinetic Energy (EKE), Total Kinetic Energy (TKE), Thermal Fronts."),
                  tags$li(strong("Static Features:"), " Bathymetry (leatherback models).")
                ),
                p("Fishery-dependent data (catch presence and effort) were obtained from the NOAA Pelagic Observer Program (1993–2024)."),
                
                h4("2. Modeling Approach", class = "methods-header"),
                p("The species distribution models in this tool were developed by specific researchers targeting different functional groups:"),
                tags$ul(
                  
                  # --- TO RESTORE SHARK DEPREDATION: Change the text below back to "...bycatch (e.g., Sharks, Billfish), and Shark Depredation risk were developed by..." ---
                  tags$li(strong("Pelagic Longline Species:"), " Models for fishery targets (e.g., Yellowfin, Swordfish) and bycatch (Bluefin, Sharks, Billfish) were developed by ", strong("Sarah Roberts"), " (manuscript in preparation) using observer data."),
                  
                  # --- TO RESTORE MANTA RAY: UNCOMMENT THE LINE BELOW ---
                  # tags$li(strong("Manta Ray:"), " Spatial models were developed by ", strong("Nick Farmer"), ". For details, see ", a("Farmer et al. (2022)", href="https://www.nature.com/articles/s41598-022-10482-8", target="_blank"), "."),
                  
                  tags$li(strong("Leatherback Turtle:"), " Spatial models were developed by ", strong("Mitchell Rider"), ". Manuscript is in prep, but for similar methods, see ", a("Rider et al. (2024)", href="https://onlinelibrary.wiley.com/doi/pdf/10.1111/ddi.70131", target="_blank"), ".")
                ),
                p("Pelagic Longline species distribution models were developed using a stacked ensemble approach combining four algorithms: Generalized Additive Models (GAM), Random Forests (RF), Boosted Regression Trees (XGBoost), and Generalized Linear Models (GLM)."),
                p("Models were trained on presence-absence data and tuned using spatial cross-validation to account for autocorrelation. The final predictions represent the probability of longline gear interacting with the species (0 to 1)."),
                
                h4("3. Species Inclusion & Fishery Constraints", class = "methods-header"),
                p("Not all species are included in the mathematical calculation. The tool applies specific ecological and operational constraints:"),
                tags$ul(
                  # --- TO RESTORE MANTA RAY: ADD 'Manta Ray probability layers and' BACK INTO THE SENTENCE BELOW ---
                  tags$li(strong("Overlay-Only Species:"), " Leatherback Turtle Core Habitat boundaries are provided as visual reference overlays. They are not mathematically penalized in the Integrated Suitability calculation.")
                ),
                
                h4("4. GulfCast Calculation & Standardization", class = "methods-header"),
                p("To ensure that common target species (with high probabilities of occurrence) do not mathematically overwhelm rare bycatch species (with naturally low probabilities), we applied a",  strong("global standardization"), "step. Before integration, the daily predicted probability for each species (P) is divided by its historical maximum (Pmax), defined as the 99.9th percentile of probabilities observed during the model training period:"),
                p("$$P_{standardized} = \\frac{P_{daily}}{P_{max}}$$"),
                p("To prevent poorly performing models from degrading the map, each species layer is weighted (\\(w\\)) by its historical model reliability (bounded TSS for targets, Sensitivity for bycatch). The integrated suitability map is calculated as the difference between the balanced target score and the cumulative bycatch penalty:"),
                
                # Equation updated to match the slide
                p("$$S_{raw} = \\left( \\alpha \\sum_{i=1}^{N_T} w_i P'_i \\right) - \\left( \\sum_{j=1}^{N_B} w_j P_j \\right)$$"),
                p("Where:"),
                tags$ul(
                  # Definitions updated to include NT, NB, i, j, and alpha
                  tags$li(strong("\\(N_T\\):"), " The number of target species (where \\(i\\) is the index for target species)."),
                  tags$li(strong("\\(N_B\\):"), " The number of bycatch species (where \\(j\\) is the index for avoidance species)."),
                  tags$li(strong("\\(P_j\\):"), " The standardized predicted probability for bycatch species."),
                  tags$li(strong("Thresholded Target (\\(P'_i\\)):"), " Standardized target probabilities thresholded to reduce noise. Cells below \\(0.25 \\times w_i\\) are zeroed out."),
                  tags$li(strong("Balancing Factor (\\(\\alpha\\)):"), " A scaler calculated as the ratio of the sum of active bycatch weights to the sum of active target weights. This ensures the target suitability signal is mathematically balanced against the avoidance signal, regardless of species count or model quality."),
                  tags$li(strong("Normalization:"), " The raw score is normalized using min-max scaling to range from -1 (High Bycatch Risk, Low Target Catch) to +1 (Ideal Fishing Ground).")
                ),
                
                h4("5. Reliability Weighting", class = "methods-header"),
                p("Reliability scores (TSS for targets, Sensitivity for bycatch) are evaluated for each species within the current season and fishery. Scores are graded using dynamic quantiles for reference on the single-layer map, but the raw score is used directly as a multiplier in the integration math:"),
                tags$ul(
                  tags$li(strong("Low (Down-weighted to zero):"), " Score falls in the bottom 25% for the season. If the base metric score is \u2264 0 (indicating performance worse than random), its weight becomes exactly 0."),
                  tags$li(strong("Medium:"), " Score falls between the 25th and 75th percentiles."),
                  tags$li(strong("High:"), " Score falls in the top 25% for the season.")
                ),
                
                h4("6. Limitations", class = "methods-header"),
                p("Predictions depend on daily satellite data availability. Models assume standard gear configurations typical for the selected fishery objective. Users should interpret results as relative catch suitability rather than habitat or absolute abundance.")
              )
    ),
    
    # --- TAB 5: ISSUES ---
    nav_panel("Report Issues",
              card(
                height = "400px", style = "padding: 40px; text-align: center;",
                h2("Found a Bug or Have a Suggestion?"),
                p("We track all issues and feature requests on our GitHub repository."),
                br(),
                a(href = GITHUB_ISSUES_URL, target = "_blank", class = "btn btn-primary btn-lg", "Open an Issue on GitHub")
              )
    )
  )
)

server <- function(input, output, session) {
  
  # --- LOAD SCALING FACTORS ---
  scaling_factors <- tryCatch({
    readRDS("data/scaling_factors.rds")
  }, error = function(e) {
    warning("Scaling factors file not found. Normalization will default to 1.0.")
    list()
  })
  
  # --- 1. UNCOMMENT BELOW TO RESTORE SHARK DEPREDATION SCALING FACTORS ---
  # shark_scaling_factors <- tryCatch({
  #   readRDS("data/shark_scaling_factors.rds")
  # }, error = function(e) {
  #   warning("Shark scaling factors file not found. Normalization will default to 1.0.")
  #   list()
  # })
  # 
  # for (obj in names(shark_scaling_factors)) {
  #   if (is.null(scaling_factors[[obj]])) {
  #     scaling_factors[[obj]] <- list()
  #   }
  #   for (sp in names(shark_scaling_factors[[obj]])) {
  #     scaling_factors[[obj]][[sp]] <- shark_scaling_factors[[obj]][[sp]]
  #   }
  # }
  
  ensure_static_file <- function(filename) {
    local_path <- file.path(tempdir(), filename)
    if (!file.exists(local_path)) {
      base_url <- "https://raw.githubusercontent.com/EcoCast-Plus/operationalization/main/model_prediction/gulf/data/"
      download_url <- paste0(base_url, filename)
      tryCatch({
        download.file(download_url, local_path, mode = "wb", quiet = TRUE)
      }, error = function(e) {
        warning(paste("Failed to download:", filename))
      })
    }
    return(local_path)
  }
  
  fishing_poly <- tryCatch({
    path <- ensure_static_file("fishing_buffer.gpkg")
    st_read(path, quiet = TRUE) 
  }, error = function(e) { NULL })
  
  desoto_poly <- tryCatch({
    ensure_static_file("desotocanyon_restricted.shp")
    ensure_static_file("desotocanyon_restricted.shx")
    ensure_static_file("desotocanyon_restricted.dbf")
    ensure_static_file("desotocanyon_restricted.prj")
    path <- file.path(tempdir(), "desotocanyon_restricted.shp")
    st_read(path, quiet = TRUE)
  }, error = function(e) { NULL })
  
  species_lookup <- tibble(
    # --- TO RESTORE MANTA RAY: ADD 'MANTA_RAY' BACK INTO THIS ID LIST ---
    id = c("dolphin_fish_mahi_mahi", "tuna_yellowfin", "swordfish", "wahoo", 
           "skipjack", "tuna_bigeye", "tuna_bluefin", "marlin_blue", 
           "shark_silky", "shark_mako_shortfin", "sailfish", "marlin_white", "Shark_Depredation"),
    
    # --- TO RESTORE MANTA RAY: ADD 'Manta Ray' BACK INTO THIS CLEAN NAME LIST ---
    clean_name = c("Mahimahi", "Yellowfin Tuna", "Swordfish", "Wahoo", 
                   "Skipjack", "Bigeye Tuna", "Bluefin Tuna", "Blue Marlin", 
                   "Silky Shark", "Shortfin Mako", "Sailfish", "White Marlin", "Shark Depredation")
  )
  
  env_lookup <- c(
    "sst" = "Sea Surface Temperature", "ssh" = "Sea Surface Height", "chlorophyll" = "Chlorophyll-a",
    "salinity" = "Salinity", "current_speed" = "Current Speed", "eke" = "Eddy Kinetic Energy",
    "tke" = "Total Kinetic Energy", "bathymetry" = "Bathymetry", "distance_to_shore" = "Distance to Shore",
    "mld" = "Mixed Layer Depth", "bottom_temp" = "Bottom Temperature", "ugos" = "Geostrophic Velocity (U)",
    "vgos" = "Geostrophic Velocity (V)", "front_z" = "SST Fronts", "thetao_150m" = "Temperature at 150m",
    "thetao_500m" = "Temperature at 500m", "uo" = "Eastward Velocity (U)", "vo" = "Northward Velocity (V)"
  )
  
  env_units <- c(
    "sst" = "°C", "ssh" = "m", "chlorophyll" = "mg/m³", "salinity" = "PSU", "current_speed" = "m/s",
    "eke" = "m²/s²", "tke" = "m²/s²", "bathymetry" = "m", "distance_to_shore" = "m", "mld" = "m",
    "bottom_temp" = "°C", "ugos" = "m/s", "vgos" = "m/s", "front_z" = "Normalized",
    "thetao_150m" = "°C", "thetao_500m" = "°C", "uo" = "m/s", "vo" = "m/s"
  )
  
  reliability_data_raw <- tibble::tribble(
    ~objective, ~species_name, ~quarter, ~Metric_Used, ~Score,
    "Swordfish_Target", "Yellowfin Tuna", "JFM", "tss", 0.609, "Swordfish_Target", "Yellowfin Tuna", "AMJ", "tss", 0.679, "Swordfish_Target", "Yellowfin Tuna", "JAS", "tss", 0.239, "Swordfish_Target", "Yellowfin Tuna", "OND", "tss", 0.084, "Swordfish_Target", "Swordfish", "JFM", "tss", 0.290, "Swordfish_Target", "Swordfish", "AMJ", "tss", 0.064, "Swordfish_Target", "Swordfish", "JAS", "tss", -0.020, "Swordfish_Target", "Swordfish", "OND", "tss", 0.232, "Swordfish_Target", "Wahoo", "JFM", "tss", 0.107, "Swordfish_Target", "Wahoo", "AMJ", "tss", 0.347, "Swordfish_Target", "Wahoo", "JAS", "tss", 0.171, "Swordfish_Target", "Wahoo", "OND", "tss", 0.000, "Swordfish_Target", "Mahimahi", "JFM", "tss", 0.156, "Swordfish_Target", "Mahimahi", "AMJ", "tss", 0.429, "Swordfish_Target", "Mahimahi", "JAS", "tss", 0.293, "Swordfish_Target", "Mahimahi", "OND", "tss", 0.023, "Swordfish_Target", "Skipjack", "JFM", "tss", 0.162, "Swordfish_Target", "Skipjack", "AMJ", "tss", 0.116, "Swordfish_Target", "Skipjack", "JAS", "tss", -0.118, "Swordfish_Target", "Skipjack", "OND", "tss", 0.280, "Swordfish_Target", "Bigeye Tuna", "JFM", "tss", 0.362, "Swordfish_Target", "Bigeye Tuna", "AMJ", "tss", 0.045, "Swordfish_Target", "Bigeye Tuna", "JAS", "tss", 0.080, "Swordfish_Target", "Bigeye Tuna", "OND", "tss", 0.232, "Swordfish_Target", "Bluefin Tuna", "JFM", "sensitivity", 0.412, "Swordfish_Target", "Bluefin Tuna", "AMJ", "sensitivity", 0.667, "Swordfish_Target", "Bluefin Tuna", "OND", "sensitivity", 0.000, "Swordfish_Target", "Blue Marlin", "JFM", "sensitivity", 0.263, "Swordfish_Target", "Blue Marlin", "AMJ", "sensitivity", 0.478, "Swordfish_Target", "Blue Marlin", "JAS", "sensitivity", 0.750, "Swordfish_Target", "Blue Marlin", "OND", "sensitivity", 0.167, "Swordfish_Target", "Silky Shark", "JFM", "sensitivity", 0.644, "Swordfish_Target", "Silky Shark", "AMJ", "sensitivity", 0.451, "Swordfish_Target", "Silky Shark", "JAS", "sensitivity", 0.227, "Swordfish_Target", "Silky Shark", "OND", "sensitivity", 0.250, "Swordfish_Target", "Shortfin Mako", "JFM", "sensitivity", 0.312, "Swordfish_Target", "Shortfin Mako", "AMJ", "sensitivity", 0.431, "Swordfish_Target", "Shortfin Mako", "JAS", "sensitivity", 0.556, "Swordfish_Target", "Shortfin Mako", "OND", "sensitivity", 0.286, "Swordfish_Target", "Sailfish", "JFM", "sensitivity", 0.714, "Swordfish_Target", "Sailfish", "AMJ", "sensitivity", 0.630, "Swordfish_Target", "Sailfish", "JAS", "sensitivity", 0.750, "Swordfish_Target", "Sailfish", "OND", "sensitivity", 0.200, "Swordfish_Target", "White Marlin", "JFM", "sensitivity", 0.000, "Swordfish_Target", "White Marlin", "AMJ", "sensitivity", 0.561, "Swordfish_Target", "White Marlin", "JAS", "sensitivity", 0.903, "Swordfish_Target", "White Marlin", "OND", "sensitivity", 0.429, "Yellowfin_Target", "Yellowfin Tuna", "JFM", "tss", 0.229, "Yellowfin_Target", "Yellowfin Tuna", "AMJ", "tss", 0.168, "Yellowfin_Target", "Yellowfin Tuna", "JAS", "tss", 0.454, "Yellowfin_Target", "Yellowfin Tuna", "OND", "tss", -0.177, "Yellowfin_Target", "Swordfish", "JFM", "tss", 0.458, "Yellowfin_Target", "Swordfish", "AMJ", "tss", 0.244, "Yellowfin_Target", "Swordfish", "JAS", "tss", 0.151, "Yellowfin_Target", "Swordfish", "OND", "tss", 0.127, "Yellowfin_Target", "Wahoo", "JFM", "tss", 0.000, "Yellowfin_Target", "Wahoo", "AMJ", "tss", 0.163, "Yellowfin_Target", "Wahoo", "JAS", "tss", 0.149, "Yellowfin_Target", "Wahoo", "OND", "tss", -0.224, "Yellowfin_Target", "Mahimahi", "JFM", "tss", 0.000, "Yellowfin_Target", "Mahimahi", "AMJ", "tss", 0.472, "Yellowfin_Target", "Mahimahi", "JAS", "tss", 0.475, "Yellowfin_Target", "Mahimahi", "OND", "tss", 0.000, "Yellowfin_Target", "Skipjack", "JFM", "tss", 0.151, "Yellowfin_Target", "Skipjack", "AMJ", "tss", 0.126, "Yellowfin_Target", "Skipjack", "JAS", "tss", 0.169, "Yellowfin_Target", "Skipjack", "OND", "tss", 0.121, "Yellowfin_Target", "Bigeye Tuna", "JFM", "tss", 0.259, "Yellowfin_Target", "Bigeye Tuna", "AMJ", "tss", 0.390, "Yellowfin_Target", "Bigeye Tuna", "JAS", "tss", 0.347, "Yellowfin_Target", "Bigeye Tuna", "OND", "tss", 0.085, "Yellowfin_Target", "Bluefin Tuna", "JFM", "sensitivity", 0.667, "Yellowfin_Target", "Bluefin Tuna", "AMJ", "sensitivity", 0.753, "Yellowfin_Target", "Blue Marlin", "JFM", "sensitivity", 0.000, "Yellowfin_Target", "Blue Marlin", "AMJ", "sensitivity", 0.423, "Yellowfin_Target", "Blue Marlin", "JAS", "sensitivity", 0.784, "Yellowfin_Target", "Blue Marlin", "OND", "sensitivity", 0.429, "Yellowfin_Target", "Silky Shark", "JFM", "sensitivity", 0.000, "Yellowfin_Target", "Silky Shark", "AMJ", "sensitivity", 0.211, "Yellowfin_Target", "Silky Shark", "JAS", "sensitivity", 0.250, "Yellowfin_Target", "Silky Shark", "OND", "sensitivity", 0.000, "Yellowfin_Target", "Shortfin Mako", "JFM", "sensitivity", 0.250, "Yellowfin_Target", "Shortfin Mako", "AMJ", "sensitivity", 0.235, "Yellowfin_Target", "Shortfin Mako", "JAS", "sensitivity", 0.167, "Yellowfin_Target", "Shortfin Mako", "OND", "sensitivity", 0.500, "Yellowfin_Target", "Sailfish", "JFM", "sensitivity", 0.000, "Yellowfin_Target", "Sailfish", "AMJ", "sensitivity", 0.724, "Yellowfin_Target", "Sailfish", "JAS", "sensitivity", 0.810, "Yellowfin_Target", "Sailfish", "OND", "sensitivity", 0.429, "Yellowfin_Target", "White Marlin", "JFM", "sensitivity", 0.000, "Yellowfin_Target", "White Marlin", "AMJ", "sensitivity", 0.480, "Yellowfin_Target", "White Marlin", "JAS", "sensitivity", 0.982, "Yellowfin_Target", "White Marlin", "OND", "sensitivity", 0.655, 
    "Swordfish_Target", "Shark Depredation", "JFM", "sensitivity", 0.491, "Swordfish_Target", "Shark Depredation", "AMJ", "sensitivity", 0.898, "Swordfish_Target", "Shark Depredation", "JAS", "sensitivity", 0.500, "Swordfish_Target", "Shark Depredation", "OND", "sensitivity", 0.118, "Yellowfin_Target", "Shark Depredation", "JFM", "sensitivity", 0.172, "Yellowfin_Target", "Shark Depredation", "AMJ", "sensitivity", 0.745, "Yellowfin_Target", "Shark Depredation", "JAS", "sensitivity", 0.578, "Yellowfin_Target", "Shark Depredation", "OND", "sensitivity", 0.000
    
    # --- TO RESTORE MANTA RAY: ADD THIS LINE BACK TO THE TIBBLE ---
    # , "MANTA_RAY", "Manta Ray", "All", "AUC", 0.852
  )
  
  observeEvent(input$display_objective, {
    req(input$display_objective)
    if(input$display_objective != input$calc_objective) {
      updateSelectInput(session, "calc_objective", selected = input$display_objective)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$calc_objective, {
    req(input$calc_objective)
    if(input$display_objective != input$calc_objective) {
      updateSelectInput(session, "display_objective", selected = input$calc_objective)
    }
  }, ignoreInit = TRUE)
  
  current_data <- reactiveValues(stack = NULL)
  
  env_vars <- c("sst", "ssh", "chlorophyll", "salinity", "current_speed", "eke", "tke", "bathymetry", "distance_to_shore", "mld", "bottom_temp", "front_z", "thetao_150m", "thetao_500m", "uo", "vo")
  
  leatherback_poly <- reactive({
    req(current_data$stack)
    lyrs <- names(current_data$stack)
    match <- lyrs[str_detect(lyrs, "CORE_") & str_detect(lyrs, "leatherback")]
    if(length(match) == 0) return(NULL)
    
    r <- current_data$stack[[match[1]]]
    r[r == 0] <- NA
    
    # Convert the binary raster into a sharp vector polygon
    poly <- tryCatch({
      p <- terra::as.polygons(r, dissolve = TRUE, na.rm = TRUE)
      sf::st_as_sf(p)
    }, error = function(e) NULL)
    
    return(poly)
  })
  
  output$leatherback_warning <- renderUI({
    req(input$show_leatherback_core)
    if(is.null(leatherback_poly())) {
      span(style="color: #d9534f; font-size: 0.8rem; font-style: italic;", "No core habitat detected for this date.")
    } else {
      span(style="color: #28a745; font-size: 0.8rem;", "Core habitat active (Dark Green).")
    }
  })
  
  github_files <- reactive({
    tryCatch({
      res <- GET(paste0(GITHUB_REPO_API, "/", PRED_FOLDER_PATH))
      df <- data.frame(name = sapply(content(res), `[[`, "name"), download_url = sapply(content(res), `[[`, "download_url")) %>% filter(str_detect(name, "\\.tif$"))
      df$date <- str_extract(df$name, "\\d{4}-\\d{2}-\\d{2}")
      return(df %>% filter(!is.na(date)))
    }, error = function(e) NULL)
  })
  
  output$date_selector_ui <- renderUI({ req(github_files()); selectInput("selected_date", "Select Date:", choices = sort(unique(github_files()$date), decreasing = TRUE)) })
  
  current_reliability_scored <- reactive({
    req(input$selected_date, input$calc_objective)
    
    date_obj <- as.Date(input$selected_date)
    month_num <- as.numeric(format(date_obj, "%m"))
    current_quarter <- case_when(month_num %in% 1:3 ~ "JFM", month_num %in% 4:6 ~ "AMJ", month_num %in% 7:9 ~ "JAS", month_num %in% 10:12 ~ "OND")
    
    subset_data <- reliability_data_raw %>% 
      filter(objective == input$calc_objective, quarter == current_quarter)
    
    if(nrow(subset_data) == 0) return(NULL)
    
    q25 <- quantile(subset_data$Score, 0.25, na.rm = TRUE)
    q75 <- quantile(subset_data$Score, 0.75, na.rm = TRUE)
    
    subset_data %>%
      mutate(
        Reliability = case_when(
          Score <= 0 ~ "Low",
          Score >= q75 ~ "High",
          Score > q25 ~ "Medium",   
          TRUE ~ "Low"             
        )
      )
  })
  
  # --- UI CHECKBOX RENDERING (UPDATED TO FETCH WEIGHTS & ADD 'REQUIRED' HEADER) ---
  output$species_selector_ui <- renderUI({
    req(input$calc_objective)
    
    scored_data <- current_reliability_scored()
    
    # Tiny helper to pull the exact weight for the UI label
    get_weight <- function(name) {
      if(is.null(scored_data)) return(1.0)
      res <- scored_data %>% filter(species_name == name)
      if(nrow(res) == 0) return(1.0)
      
      raw_score <- res$Score[1]
      if(res$Metric_Used[1] == "tss") return(max(0, raw_score)) else return(raw_score)
    }
    
    get_val <- function(id, def) { 
      isolate({
        if(!is.null(input[[id]])) input[[id]] else def 
      })
    }
    
    default_yft <- if(is.null(input$calc_objective)) TRUE else (input$calc_objective == "Yellowfin_Target")
    default_swo <- if(is.null(input$calc_objective)) TRUE else (input$calc_objective == "Swordfish_Target")
    
    tagList(
      div(
        div(class = "column-header", style="color: #005da2; display: flex; justify-content: space-between; align-items: flex-end;", 
            span("Target Species"),
            span(style="color: #666; font-size: 0.75rem; font-weight: normal; font-style: italic;", "Weight = Model Reliability")
        ),
        species_checkbox("include_yft", "Yellowfin Tuna", get_val("include_yft", default_yft), "target", get_weight("Yellowfin Tuna")),
        species_checkbox("include_swo", "Swordfish", get_val("include_swo", default_swo), "target", get_weight("Swordfish")),
        species_checkbox("include_wah", "Wahoo", get_val("include_wah", TRUE), "target", get_weight("Wahoo")),
        species_checkbox("include_mah", "Mahimahi", get_val("include_mah", TRUE), "target", get_weight("Mahimahi")),
        species_checkbox("include_skj", "Skipjack", get_val("include_skj", TRUE), "target", get_weight("Skipjack")),
        species_checkbox("include_bet", "Bigeye", get_val("include_bet", TRUE), "target", get_weight("Bigeye Tuna"))
      ),
      br(),
      div(
        # Updated header to include "(Required)" inline
        div(class = "column-header", style="color: #d9534f; display: flex; justify-content: space-between;", 
            span("Bycatch Species"),
            span(style="font-size: 0.8rem; font-weight: normal; margin-top: 4px;", "(Required)")
        ),
        species_checkbox("include_bft", "Bluefin Tuna", TRUE, "bycatch", get_weight("Bluefin Tuna")),
        species_checkbox("include_bM", "Blue Marlin", TRUE, "bycatch", get_weight("Blue Marlin")),
        species_checkbox("include_ssy", "Silky Shark", TRUE, "bycatch", get_weight("Silky Shark")),
        species_checkbox("include_smk", "Shortfin Mako", TRUE, "bycatch", get_weight("Shortfin Mako")),
        species_checkbox("include_sfi", "Sailfish", TRUE, "bycatch", get_weight("Sailfish")),
        species_checkbox("include_wma", "White Marlin", TRUE, "bycatch", get_weight("White Marlin"))
        
        # --- 3. UNCOMMENT THE LINE BELOW TO RESTORE SHARK DEPREDATION CHECKBOX UI ---
        # , species_checkbox("include_shp", "Shark Depredation", TRUE, "bycatch", get_weight("Shark Depredation"))
      )
    )
  })
  
  observeEvent(c(input$selected_date, input$display_objective), {
    req(input$selected_date, input$display_objective)
    output$download_status <- renderUI({ p("Loading Data...", style="color: blue;") })
    all_files <- github_files() %>% filter(date == input$selected_date)
    if (nrow(all_files) == 0) { output$download_status <- renderUI({ p("No data found.", style="color: red;") }); return() }
    
    r_stack <- rast()
    for (i in 1:nrow(all_files)) {
      dest <- file.path(tempdir(), all_files$name[i])
      if(!file.exists(dest)) download.file(all_files$download_url[i], dest, mode="wb", quiet=TRUE)
      r <- rast(dest)
      names(r) <- str_remove(all_files$name[i], "\\.tif$")
      add(r_stack) <- r
    }
    
    # --- TO RESTORE MANTA RAY BATHYMETRY FILTERING: UNCOMMENT THIS BLOCK ---
    # layer_names <- names(r_stack)
    # manta_idx <- which(str_detect(layer_names, "MANTA_RAY"))
    # bathy_idx  <- which(str_detect(layer_names, "bathymetry"))
    # if (length(manta_idx) > 0 && length(bathy_idx) > 0) {
    #   manta_layer <- r_stack[[manta_idx]]
    #   bathy_layer <- r_stack[[bathy_idx]]
    #   manta_layer[abs(bathy_layer) > 2200] <- NA 
    #   r_stack[[manta_idx]] <- manta_layer
    # }
    
    current_data$stack <- r_stack
    output$download_status <- renderUI({ p("Data loaded.", style="color: green;") })
  })
  
  observeEvent(current_data$stack, {
    req(current_data$stack)
    
    all_layers <- names(current_data$stack)
    is_env <- str_detect(tolower(all_layers), paste(env_vars, collapse = "|"))
    raw_pred <- all_layers[!is_env]
    raw_env  <- all_layers[is_env]
    
    current_obj <- input$display_objective 
    fishery_files <- raw_pred[str_detect(raw_pred, current_obj) & !str_detect(toupper(raw_pred), "MANTA_RAY")]
    
    # --- 4. COMMENT OUT THE LINE BELOW IF YOU WANT TO RESTORE SHARK DEPREDATION TO THE DROPDOWN ---
    fishery_files <- fishery_files[!str_detect(tolower(fishery_files), "shark_depredation")]
    
    manta_files <- raw_pred[str_detect(toupper(raw_pred), "MANTA_RAY")]
    valid_files <- unique(c(fishery_files, manta_files))
    
    # --- TO RESTORE MANTA RAY TO THE DROPDOWN: COMMENT OUT THE LINE BELOW ---
    valid_files <- valid_files[!str_detect(toupper(valid_files), "MANTA_RAY")]
    
    files_df <- tibble(raw_filename = valid_files)
    files_df$clean_name <- sapply(files_df$raw_filename, function(f) {
      match <- species_lookup %>% filter(str_detect(f, id))
      if(nrow(match) > 0) return(match$clean_name[1]) else return(f)
    })
    pred_choices <- setNames(files_df$raw_filename, files_df$clean_name)
    pred_choices <- pred_choices[order(names(pred_choices))]
    
    env_clean_names <- sapply(raw_env, function(x) {
      core_name <- str_remove(x, paste0("PRED_", input$selected_date, "_"))
      if(core_name %in% names(env_lookup)) return(env_lookup[[core_name]])
      return(str_to_title(gsub("_", " ", core_name)))
    })
    env_choices <- setNames(raw_env, env_clean_names)
    
    updateSelectInput(session, "fisheries_layer", choices = c("Select one..."="", pred_choices))
    updateSelectInput(session, "env_layer", choices = c("Select one..."="", env_choices))
  })
  
  observeEvent(input$fisheries_layer, { if (input$fisheries_layer != "") updateSelectInput(session, "env_layer", selected = "") })
  observeEvent(input$env_layer, { if (input$env_layer != "") updateSelectInput(session, "fisheries_layer", selected = "") })
  
  output$layer_date_info <- renderUI({
    req(current_data$stack)
    l <- if(input$fisheries_layer != "") input$fisheries_layer else input$env_layer
    req(l)
    date_str <- str_extract(l, "\\d{4}-\\d{2}-\\d{2}")
    if (!is.na(date_str)) span(class = "date-label", paste("Data Date:", date_str)) else NULL
  })
  
  render_map <- function() {
    leaflet() %>% 
      addTiles() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -82.5, lat = 26, zoom = 5) %>% 
      addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, autoCenter = TRUE, maxZoom = 10, setView = TRUE)) %>%
      addMapPane("topLayer", zIndex = 450)
  }
  
  output$leafletMap <- renderLeaflet({ render_map() })
  output$integratedLeafletMap <- renderLeaflet({ render_map() })
  
  observeEvent(input$show_fishing_area, {
    proxy <- leafletProxy("leafletMap"); proxy_int <- leafletProxy("integratedLeafletMap")
    if(input$show_fishing_area && !is.null(fishing_poly)) {
      proxy %>% addPolygons(data = fishing_poly, group = "fishing", color = "black", fill = FALSE, weight = 2)
      proxy_int %>% addPolygons(data = fishing_poly, group = "fishing", color = "black", fill = FALSE, weight = 2)
    } else {
      proxy %>% clearGroup("fishing"); proxy_int %>% clearGroup("fishing")
    }
  })
  
  observeEvent(input$show_desoto_canyon, {
    proxy <- leafletProxy("leafletMap"); proxy_int <- leafletProxy("integratedLeafletMap")
    if(input$show_desoto_canyon && !is.null(desoto_poly)) {
      proxy %>% addPolygons(data = desoto_poly, group = "desoto", color = "red", fill = FALSE, weight = 2)
      proxy_int %>% addPolygons(data = desoto_poly, group = "desoto", color = "red", fill = FALSE, weight = 2)
    } else {
      proxy %>% clearGroup("desoto"); proxy_int %>% clearGroup("desoto")
    }
  })
  
  observe({
    proxy_single <- leafletProxy("leafletMap")
    proxy_int <- leafletProxy("integratedLeafletMap")
    
    proxy_single %>% clearGroup("leatherback_core")
    proxy_int %>% clearGroup("leatherback_core")
    
    if(input$show_leatherback_core) {
      p_core <- leatherback_poly()
      if(!is.null(p_core) && nrow(p_core) > 0) {
        
        # Use addPolygons with pathOptions so it perfectly locks into the topLayer pane
        proxy_single %>% addPolygons(data = p_core, fillColor = "#006400", fillOpacity = 0.6, 
                                     stroke = FALSE, group = "leatherback_core", 
                                     options = pathOptions(pane = "topLayer"))
        
        proxy_int %>% addPolygons(data = p_core, fillColor = "#006400", fillOpacity = 0.6, 
                                  stroke = FALSE, group = "leatherback_core", 
                                  options = pathOptions(pane = "topLayer"))
      }
    }
  })
  
  observe({
    req(current_data$stack)
    l <- if(input$fisheries_layer != "") input$fisheries_layer else input$env_layer
    req(l, l %in% names(current_data$stack))
    
    is_fishery_layer <- (input$fisheries_layer != "")
    
    r <- current_data$stack[[l]]; if (ncell(r) > 1e6) r <- aggregate(r, fact=2)
    pal <- colorNumeric("viridis", values(r), na.color = "transparent")
    title_clean <- species_lookup %>% filter(str_detect(l, id)) %>% pull(clean_name)
    core_name <- str_remove(l, paste0("PRED_", input$selected_date, "_"))
    
    if(length(title_clean) == 0) {
      if(core_name %in% names(env_lookup)) title_clean <- env_lookup[[core_name]]
      else title_clean <- str_to_title(gsub("_", " ", core_name))
    }
    
    proxy <- leafletProxy("leafletMap") %>% 
      clearGroup("Data") %>%
      addRasterImage(r, colors = pal, opacity = 0.8, group = "Data", project = TRUE) %>%
      clearControls() 
    
    if(is_fishery_layer) {
      proxy %>% addLegend(pal = pal, values = values(r), title = title_clean,
                          labFormat = function(type, cuts, p) {
                            n = length(cuts); labels <- paste0(cuts)
                            labels[1] <- paste0(labels[1], " (Low Prob.)"); labels[n] <- paste0(labels[n], " (High Prob.)")
                            return(labels)
                          })
      
      obj_match <- "Unknown"
      if(str_detect(l, "Swordfish_Target")) obj_match <- "Swordfish_Target"
      if(str_detect(l, "Yellowfin_Target")) obj_match <- "Yellowfin_Target"
      if(str_detect(l, "MANTA_RAY"))        obj_match <- "MANTA_RAY"
      
      scored_data <- current_reliability_scored()
      
      if(obj_match == "MANTA_RAY") {
        html_content <- sprintf(
          "<div style='background-color: white; padding: 8px 12px; border-radius: 4px; box-shadow: 0 0 5px rgba(0,0,0,0.3); font-size: 14px;'>
             <strong style='display:block; margin-bottom:4px; border-bottom:1px solid #eee;'>Model Reliability (All Year)</strong>
             <span style='color: #28a745; font-size: 18px; vertical-align: middle;'>&#9679;</span> 
             <b>High</b> <span style='color:#666; font-size:12px;'>(AUC: 0.85)</span>
           </div>"
        )
        proxy %>% addControl(html = html_content, position = "bottomright")
      } else if (!is.null(scored_data)) {
        rel_info <- scored_data %>% filter(species_name == title_clean)
        if(nrow(rel_info) > 0) {
          if(rel_info$Reliability == "Low") {
            html_content <- sprintf(
              "<div style='background-color: white; padding: 10px 14px; border-radius: 4px; box-shadow: 0 0 5px rgba(0,0,0,0.3); font-size: 14px; border-left: 5px solid #dc3545;'>
                 <strong style='display:block; margin-bottom:4px; color: #dc3545;'>Low Reliability (Down-weighted)</strong>
                 <span style='color: #dc3545; font-size: 18px; vertical-align: middle;'>&#9888;</span> 
                 <b>Low</b> <br>
                 <span style='color:#666; font-size:12px;'>Score: %.2f (%s)</span>
               </div>",
              rel_info$Score, toupper(rel_info$Metric_Used)
            )
          } else {
            status_color <- switch(rel_info$Reliability, "High" = "#28a745", "Medium" = "#ffc107", "Low" = "#dc3545")
            html_content <- sprintf(
              "<div style='background-color: white; padding: 8px 12px; border-radius: 4px; box-shadow: 0 0 5px rgba(0,0,0,0.3); font-size: 14px;'>
                 <strong style='display:block; margin-bottom:4px; border-bottom:1px solid #eee;'>Model Reliability</strong>
                 <span style='color: %s; font-size: 18px; vertical-align: middle;'>&#9679;</span> 
                 <b>%s</b> <span style='color:#666; font-size:12px;'>(%s: %.2f)</span>
               </div>",
              status_color, rel_info$Reliability, toupper(rel_info$Metric_Used), rel_info$Score
            )
          }
          proxy %>% addControl(html = html_content, position = "bottomright")
        }
      }
    } else {
      unit_label <- env_units[core_name]
      final_title <- title_clean
      if(!is.na(unit_label) && !is.null(unit_label)) final_title <- paste0(title_clean, " (", unit_label, ")")
      proxy %>% addLegend(pal = pal, values = values(r), title = final_title)
    }
  })
  
  integrated_surface_reactive <- reactive({
    req(current_data$stack)
    s <- current_data$stack
    lyrs <- names(s)
    current_target <- input$calc_objective
    
    valid_targets <- lyrs[str_detect(lyrs, current_target)]
    if(length(valid_targets) > 0) ref_layer <- s[[valid_targets[1]]] else ref_layer <- s[[1]]
    
    data_mask <- !is.na(ref_layer)
    init_raster <- ref_layer * 0; init_raster[is.na(init_raster)] <- 0
    
    w_map <- list(
      "tuna_yellowfin"= if(is.null(input$include_yft)) (if(current_target=="Yellowfin_Target") 1 else 0) else (if(isTRUE(input$include_yft)) 1 else 0),
      "swordfish"= if(is.null(input$include_swo)) (if(current_target=="Swordfish_Target") 1 else 0) else (if(isTRUE(input$include_swo)) 1 else 0),
      "wahoo"= if(is.null(input$include_wah)) 1 else (if(isTRUE(input$include_wah)) 1 else 0),
      "dolphin_fish"= if(is.null(input$include_mah)) 1 else (if(isTRUE(input$include_mah)) 1 else 0),
      "skipjack"= if(is.null(input$include_skj)) 1 else (if(isTRUE(input$include_skj)) 1 else 0),
      "tuna_bigeye"= if(is.null(input$include_bet)) 1 else (if(isTRUE(input$include_bet)) 1 else 0)
    )
    b_map <- list("tuna_bluefin"=1, "marlin_blue"=1, "shark_silky"=1, "shark_mako"=1, "sailfish"=1, "marlin_white"=1
                  
                  # --- 5. UNCOMMENT THE LINE BELOW TO ADD SHARK DEPREDATION BACK TO THE MULTISPECIES SCORE ---
                  # , "Shark_Depredation"=1 
    )
    
    scored_data <- current_reliability_scored()
    
    get_layer_for_species <- function(species_string) {
      matches <- lyrs[str_detect(lyrs, species_string) & str_detect(lyrs, current_target)]
      
      if(length(matches) == 0) return(NULL)
      r <- s[[matches[1]]]; r[is.na(r)] <- 0
      
      lookup_species <- species_string
      if(species_string == "dolphin_fish") lookup_species <- "dolphin_fish_mahi_mahi"
      if(species_string == "shark_mako") lookup_species <- "shark_mako_shortfin"
      
      factor_val <- if (!is.null(scaling_factors[[current_target]][[lookup_species]])) scaling_factors[[current_target]][[lookup_species]] else 1
      
      if (factor_val > 0) {
        r <- r / factor_val
        r[r > 1] <- 1 
      }
      return(r)
    }
    
    t_score_sum <- init_raster
    sum_target_weights <- 0
    
    for(sp in names(w_map)) {
      is_active <- w_map[[sp]]
      
      clean_lookup <- species_lookup %>% filter(str_detect(id, sp)) 
      if(nrow(clean_lookup) == 0) {
        if(sp == "dolphin_fish") clean_name <- "Mahimahi"
        else clean_name <- "Unknown"
      } else {
        clean_name <- clean_lookup$clean_name[1]
      }
      
      if(is_active > 0) {
        r_pred <- get_layer_for_species(sp)
        if(!is.null(r_pred)) { 
          # Apply Model Reliability Weight
          rel_row <- scored_data %>% filter(species_name == clean_name)
          rel_weight <- 1
          if(nrow(rel_row) > 0) {
            raw_score <- rel_row$Score[1]
            if(rel_row$Metric_Used[1] == "tss") rel_weight <- max(0, raw_score) else rel_weight <- raw_score
          }
          
          r_weighted <- r_pred * rel_weight
          sum_target_weights <- sum_target_weights + rel_weight
          
          r_weighted[r_weighted < (0.25 * rel_weight)] <- 0
          t_score_sum <- t_score_sum + r_weighted 
        }
      }
    }
    
    b_score_sum <- init_raster
    sum_bycatch_weights <- 0
    
    for(sp in names(b_map)) {
      is_active <- b_map[[sp]]
      
      clean_lookup <- species_lookup %>% filter(str_detect(id, sp))
      if(nrow(clean_lookup) == 0) {
        if(sp == "shark_mako") clean_name <- "Shortfin Mako"
        else clean_name <- "Unknown"
      } else {
        clean_name <- clean_lookup$clean_name[1]
      }
      
      if(is_active > 0) {
        r_pred <- get_layer_for_species(sp)
        if(!is.null(r_pred)) {
          # Apply Model Reliability Weight
          rel_row <- scored_data %>% filter(species_name == clean_name)
          rel_weight <- 1
          if(nrow(rel_row) > 0) {
            raw_score <- rel_row$Score[1]
            if(rel_row$Metric_Used[1] == "tss") rel_weight <- max(0, raw_score) else rel_weight <- raw_score
          }
          
          r_weighted <- r_pred * rel_weight
          sum_bycatch_weights <- sum_bycatch_weights + rel_weight
          
          b_score_sum <- b_score_sum + r_weighted
        }
      }
    }
    
    if (sum_target_weights > 0 && sum_bycatch_weights > 0) {
      beta <- sum_bycatch_weights / sum_target_weights
    } else {
      beta <- 1
    }
    
    t_final_balanced <- t_score_sum * beta
    raw_score <- t_final_balanced - b_score_sum
    raw_score[!data_mask] <- NA
    
    rng <- global(raw_score, c("min", "max"), na.rm=TRUE)
    r_min <- rng[1, "min"]; r_max <- rng[1, "max"]
    
    if(is.na(r_max) || is.na(r_min) || (r_max == r_min)) return(raw_score * 0) 
    return(2 * ((raw_score - r_min) / (r_max - r_min)) - 1)
  })
  
  observe({
    req(input$main_tabs == "Integrated Multispecies Map")
    req(integrated_surface_reactive())
    
    r <- integrated_surface_reactive()
    pal <- colorNumeric(c("#FF0000", "#FFFFFF", "#0000FF"), domain = c(-1, 1), na.color = "transparent")
    
    proxy_int <- leafletProxy("integratedLeafletMap") %>% 
      clearGroup("Data") %>%
      addRasterImage(r, colors = pal, opacity = 0.7, group = "Data", project = TRUE) %>%
      clearControls()
    
    proxy_int %>% addLegend(colors = c("darkblue", "white", "darkred"), labels = c("Catch", "0", "Avoid"), title = "Fishing Suitability", position = "topleft")
    
    if(input$show_leatherback_core && !is.null(leatherback_poly())) {
      proxy_int %>% addLegend(colors = "#006400", labels = "Leatherback Core Habitat", title = "Protected Species", position = "bottomleft")
    }
  })
  
  # --- UPDATED: DOWNLOAD HANDLERS FOR DYNAMIC EXTENSION (.tif or .kmz) ---
  output$downloadSingleMap <- downloadHandler(
    filename = function() { 
      ext <- input$format_single
      paste0(if(input$fisheries_layer != "") input$fisheries_layer else input$env_layer, ext) 
    },
    content = function(file) { 
      req(current_data$stack); 
      l <- if(input$fisheries_layer != "") input$fisheries_layer else input$env_layer; 
      writeRaster(current_data$stack[[l]], file, overwrite=TRUE) 
    }
  )
  
  output$downloadIntegrated <- downloadHandler(
    filename = function() { 
      ext <- input$format_int
      paste0("GulfCast_Integrated_", input$selected_date, ext) 
    },
    content = function(file) {
      req(integrated_surface_reactive())
      r <- integrated_surface_reactive()
      writeRaster(r, file, overwrite = TRUE)
    }
  )
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("GulfCast_Report_", input$selected_date, ".", tolower(input$report_format))
    },
    content = function(file) {
      req(current_data$stack)
      
      # --- 1. HELPER: CALCULATE INTEGRATED MAPS ---
      calc_integrated_for_report <- function(target_obj) {
        s <- current_data$stack
        lyrs <- names(s)
        valid_targets <- lyrs[str_detect(lyrs, target_obj)]
        if(length(valid_targets) > 0) ref_layer <- s[[valid_targets[1]]] else ref_layer <- s[[1]]
        data_mask <- !is.na(ref_layer)
        init_raster <- ref_layer * 0; init_raster[is.na(init_raster)] <- 0
        
        # If the target matches the UI, use custom checkboxes. Otherwise, use defaults.
        if(target_obj == input$calc_objective) {
          w_map <- list(
            "tuna_yellowfin"=if(isTRUE(input$include_yft)) 1 else 0,
            "swordfish"=if(isTRUE(input$include_swo)) 1 else 0,
            "wahoo"=if(isTRUE(input$include_wah)) 1 else 0,
            "dolphin_fish"=if(isTRUE(input$include_mah)) 1 else 0,
            "skipjack"=if(isTRUE(input$include_skj)) 1 else 0,
            "tuna_bigeye"=if(isTRUE(input$include_bet)) 1 else 0
          )
        } else {
          yft_inc <- if(target_obj == "Swordfish_Target") 0 else 1
          swo_inc <- if(target_obj == "Yellowfin_Target") 0 else 1
          w_map <- list("tuna_yellowfin"=yft_inc, "swordfish"=swo_inc, "wahoo"=1, "dolphin_fish"=1, "skipjack"=1, "tuna_bigeye"=1)
        }
        b_map <- list("tuna_bluefin"=1, "marlin_blue"=1, "shark_silky"=1, "shark_mako"=1, "sailfish"=1, "marlin_white"=1)
        
        # Determine current quarter for reliability filtering
        date_obj <- as.Date(input$selected_date)
        month_num <- as.numeric(format(date_obj, "%m"))
        current_quarter <- case_when(month_num %in% 1:3 ~ "JFM", month_num %in% 4:6 ~ "AMJ", month_num %in% 7:9 ~ "JAS", month_num %in% 10:12 ~ "OND")
        
        subset_data <- reliability_data_raw %>% filter(objective == target_obj, quarter == current_quarter)
        if(nrow(subset_data) > 0) {
          q25 <- quantile(subset_data$Score, 0.25, na.rm = TRUE)
          q75 <- quantile(subset_data$Score, 0.75, na.rm = TRUE)
          scored <- subset_data %>% mutate(Reliability = case_when(Score <= 0 ~ "Low", Score >= q75 ~ "High", Score > q25 ~ "Medium", TRUE ~ "Low"))
        } else {
          scored <- NULL
        }
        
        get_layer_for_species <- function(species_string) {
          matches <- lyrs[str_detect(lyrs, species_string) & str_detect(lyrs, target_obj)]
          if(length(matches) == 0) return(NULL)
          r <- s[[matches[1]]]; r[is.na(r)] <- 0
          lookup_species <- species_string
          if(species_string == "dolphin_fish") lookup_species <- "dolphin_fish_mahi_mahi"
          if(species_string == "shark_mako") lookup_species <- "shark_mako_shortfin"
          
          factor_val <- if (!is.null(scaling_factors[[target_obj]][[lookup_species]])) scaling_factors[[target_obj]][[lookup_species]] else 1
          if (factor_val > 0) { r <- r / factor_val; r[r > 1] <- 1 }
          return(r)
        }
        
        t_score_sum <- init_raster
        sum_target_weights <- 0
        for(sp in names(w_map)) {
          is_active <- w_map[[sp]]
          clean_name <- species_lookup %>% filter(str_detect(id, sp)) %>% pull(clean_name) %>% .[1]
          if(sp == "dolphin_fish") clean_name <- "Mahimahi"
          
          if(is_active > 0) {
            r_pred <- get_layer_for_species(sp)
            if(!is.null(r_pred)) {
              rel_weight <- 1
              if(!is.null(scored)) {
                rel_row <- scored %>% filter(species_name == clean_name)
                if(nrow(rel_row) > 0) {
                  if(rel_row$Metric_Used[1] == "tss") rel_weight <- max(0, rel_row$Score[1]) else rel_weight <- rel_row$Score[1]
                }
              }
              
              r_weighted <- r_pred * rel_weight
              sum_target_weights <- sum_target_weights + rel_weight
              r_weighted[r_weighted < (0.25 * rel_weight)] <- 0
              t_score_sum <- t_score_sum + r_weighted
            }
          }
        }
        
        b_score_sum <- init_raster
        sum_bycatch_weights <- 0
        for(sp in names(b_map)) {
          is_active <- b_map[[sp]]
          clean_name <- species_lookup %>% filter(str_detect(id, sp)) %>% pull(clean_name) %>% .[1]
          if(sp == "shark_mako") clean_name <- "Shortfin Mako"
          
          if(is_active > 0) {
            r_pred <- get_layer_for_species(sp)
            if(!is.null(r_pred)) {
              rel_weight <- 1
              if(!is.null(scored)) {
                rel_row <- scored %>% filter(species_name == clean_name)
                if(nrow(rel_row) > 0) {
                  if(rel_row$Metric_Used[1] == "tss") rel_weight <- max(0, rel_row$Score[1]) else rel_weight <- rel_row$Score[1]
                }
              }
              
              r_weighted <- r_pred * rel_weight
              sum_bycatch_weights <- sum_bycatch_weights + rel_weight
              b_score_sum <- b_score_sum + r_weighted
            }
          }
        }
        
        beta <- if (sum_target_weights > 0 && sum_bycatch_weights > 0) sum_bycatch_weights / sum_target_weights else 1
        t_final_balanced <- t_score_sum * beta
        raw_score <- t_final_balanced - b_score_sum
        raw_score[!data_mask] <- NA
        
        rng <- global(raw_score, c("min", "max"), na.rm=TRUE)
        r_min <- rng[1, "min"]; r_max <- rng[1, "max"]
        if(is.na(r_max) || is.na(r_min) || (r_max == r_min)) return(raw_score * 0)
        return(2 * ((raw_score - r_min) / (r_max - r_min)) - 1)
      }
      
      # Generate and save temporary rasters for both objectives
      temp_sword_int_path <- file.path(tempdir(), "temp_sword_integrated.tif")
      temp_yellow_int_path <- file.path(tempdir(), "temp_yellow_integrated.tif")
      
      writeRaster(calc_integrated_for_report("Swordfish_Target"), temp_sword_int_path, overwrite = TRUE)
      writeRaster(calc_integrated_for_report("Yellowfin_Target"), temp_yellow_int_path, overwrite = TRUE)
      
      # --- 2. FILTER THE STACK: Drop Manta and Shark Depredation ---
      s_names <- names(current_data$stack)
      is_manta <- str_detect(toupper(s_names), "MANTA_RAY")
      is_shark_dep <- str_detect(tolower(s_names), "shark_depredation")
      
      valid_layers <- s_names[!is_manta & !is_shark_dep]
      report_stack <- current_data$stack[[valid_layers]]
      
      # --- 3. ORGANIZE LAYERS INTO CATEGORIES ---
      env_vars <- c("sst", "ssh", "chlorophyll", "salinity", "current_speed", "eke", "tke", "bathymetry", "distance_to_shore", "mld", "bottom_temp", "front_z", "thetao_150m", "thetao_500m", "uo", "vo")
      env_layers <- valid_layers[str_detect(tolower(valid_layers), paste(env_vars, collapse = "|"))]
      sword_layers <- valid_layers[str_detect(valid_layers, "Swordfish_Target")]
      yellow_layers <- valid_layers[str_detect(valid_layers, "Yellowfin_Target")]
      
      # --- 4. BUILD TITLE MAP & RELIABILITY MAP ---
      title_map <- list()
      rel_map <- list()
      
      date_obj <- as.Date(input$selected_date)
      month_num <- as.numeric(format(date_obj, "%m"))
      current_quarter <- case_when(month_num %in% 1:3 ~ "JFM", month_num %in% 4:6 ~ "AMJ", month_num %in% 7:9 ~ "JAS", month_num %in% 10:12 ~ "OND")
      
      for(lyr in valid_layers) {
        title_clean_lookup <- species_lookup %>% filter(str_detect(lyr, id)) %>% pull(clean_name)
        core_name <- str_remove(lyr, paste0("PRED_", input$selected_date, "_"))
        
        if(length(title_clean_lookup) == 0) {
          # Environmental Layers
          if(core_name %in% names(env_lookup)) title_clean <- env_lookup[[core_name]]
          else title_clean <- str_to_title(gsub("_", " ", core_name))
          title_map[[lyr]] <- paste0(title_clean, " (", input$selected_date, ")")
          rel_map[[lyr]] <- "" # No reliability for env layers
        } else {
          # Fishery Layers
          title_clean <- title_clean_lookup[1]
          title_map[[lyr]] <- paste0(title_clean, " (", input$selected_date, ")")
          
          # Lookup Reliability
          obj <- if(str_detect(lyr, "Swordfish_Target")) "Swordfish_Target" else "Yellowfin_Target"
          rel_row <- reliability_data_raw %>% filter(objective == obj, species_name == title_clean, quarter == current_quarter)
          
          if(nrow(rel_row) > 0) {
            subset_data <- reliability_data_raw %>% filter(objective == obj, quarter == current_quarter)
            q25 <- quantile(subset_data$Score, 0.25, na.rm = TRUE)
            q75 <- quantile(subset_data$Score, 0.75, na.rm = TRUE)
            
            score <- rel_row$Score[1]
            metric <- toupper(rel_row$Metric_Used[1])
            rel_status <- if(score <= 0) "Low" else if(score >= q75) "High" else if(score > q25) "Medium" else "Low"
            
            rel_map[[lyr]] <- paste0("Reliability: ", rel_status, " (", metric, ": ", round(score, 3), ")")
          } else {
            rel_map[[lyr]] <- "Reliability: Not Evaluated"
          }
        }
      }
      
      # --- 5. WRITE THE RMARKDOWN DOCUMENT ---
      lines <- c(
        "---",
        "title: \"GulfCast Daily Report\"",
        "output: html_document",
        "params:",
        "  date: NA",
        "  sword_int_path: NA",
        "  yellow_int_path: NA",
        "  stack_obj: NA",
        "  title_map: NA",
        "  rel_map: NA",
        "  sword_layers: NA",
        "  yellow_layers: NA",
        "  env_layers: NA",
        "---",
        "",
        paste0("```", "{r setup, include=FALSE}"),
        "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)",
        "library(terra)",
        "library(stringr)",
        paste0("```"),
        "",
        "## Integrated Fishing Suitability",
        "These maps represent the calculated fishing suitability. Scale: Blue (Target Catch) to Red (Avoid Bycatch).",
        "",
        "### Swordfish Target (Night)",
        paste0("```", "{r int_sword}"),
        "r_swo <- rast(params$sword_int_path)",
        "plot(r_swo, col = colorRampPalette(c('#FF0000', '#FFFFFF', '#0000FF'))(100), main = paste('Integrated Suitability - Swordfish (', params$date, ')'))",
        paste0("```"),
        "",
        "### Yellowfin Target (Day)",
        paste0("```", "{r int_yellow}"),
        "r_yft <- rast(params$yellow_int_path)",
        "plot(r_yft, col = colorRampPalette(c('#FF0000', '#FFFFFF', '#0000FF'))(100), main = paste('Integrated Suitability - Yellowfin (', params$date, ')'))",
        paste0("```"),
        "",
        "## Species Predictions - Swordfish Target (Night)",
        "Modeled probabilities optimized for night fishing with light sticks.",
        "",
        paste0("```", "{r sword_maps, results='asis'}"),
        "s <- params$stack_obj",
        "if(length(params$sword_layers) > 0) {",
        "  for(lyr in params$sword_layers) {",
        "    c_title <- params$title_map[[lyr]]",
        "    c_rel <- params$rel_map[[lyr]]",
        "    cat(paste0('### ', c_title, '\\n\\n'))",
        "    cat(paste0('**', c_rel, '**\\n\\n'))",
        "    plot(s[[lyr]], col = terra::map.pal('viridis', 100), main = c_title)",
        "    cat('\\n\\n')",
        "  }",
        "}",
        paste0("```"),
        "",
        "## Species Predictions - Yellowfin Target (Day)",
        "Modeled probabilities optimized for daytime fishing.",
        "",
        paste0("```", "{r yellow_maps, results='asis'}"),
        "if(length(params$yellow_layers) > 0) {",
        "  for(lyr in params$yellow_layers) {",
        "    c_title <- params$title_map[[lyr]]",
        "    c_rel <- params$rel_map[[lyr]]",
        "    cat(paste0('### ', c_title, '\\n\\n'))",
        "    cat(paste0('**', c_rel, '**\\n\\n'))",
        "    plot(s[[lyr]], col = terra::map.pal('viridis', 100), main = c_title)",
        "    cat('\\n\\n')",
        "  }",
        "}",
        paste0("```"),
        "",
        "## Environmental Variables",
        "",
        paste0("```", "{r env_maps, results='asis'}"),
        "if(length(params$env_layers) > 0) {",
        "  for(lyr in params$env_layers) {",
        "    c_title <- params$title_map[[lyr]]",
        "    cat(paste0('### ', c_title, '\\n\\n'))",
        "    plot(s[[lyr]], col = terra::map.pal('viridis', 100), main = c_title)",
        "    cat('\\n\\n')",
        "  }",
        "}",
        paste0("```")
      )
      
      rmd_content <- paste(lines, collapse = "\n")
      
      temp_rmd <- file.path(tempdir(), "report.Rmd")
      writeLines(rmd_content, temp_rmd)
      
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        params = list(
          date = input$selected_date,
          sword_int_path = temp_sword_int_path,
          yellow_int_path = temp_yellow_int_path,
          stack_obj = report_stack,
          title_map = title_map,
          rel_map = rel_map,
          sword_layers = sword_layers,
          yellow_layers = yellow_layers,
          env_layers = env_layers
        ),
        output_format = if(input$report_format == "PDF") "pdf_document" else "html_document",
        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)