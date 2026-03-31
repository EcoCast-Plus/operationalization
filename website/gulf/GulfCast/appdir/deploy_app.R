setwd("appdir/app")
rsconnect::addServer(
  "https://connect.fisheries.noaa.gov", # Or the "test-connect" URL
  name = "NMFS_Prod")

rsconnect::connectApiUser(server = "NMFS_Prod", 
                          account = "sarah.roberts@noaa.gov", # The username you log in with
                          apiKey = "yFFE37bmFOuYQow8qjbpK8wGDQGBHuuu")



rsconnect::deployApp(
  appDir = "/Users/sarahroberts/Library/CloudStorage/GoogleDrive-srobs32@gmail.com/My Drive/Work/Projects/CEFI/EcoCast/GulfCast/appdir/app",
  appName = "Gulf_PLL_EcoCast",
  appPrimaryDoc = "EcoCastGulf.R"  # <-- Add this line
)


#if you install a new library run this 
renv::snapshot(force = TRUE) #usually type 2 
