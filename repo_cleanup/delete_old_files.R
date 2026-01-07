### Script to delete processed files directly from GH repo via API
# prevents repo bloat without needing to clone/push huge history

library(httr)
library(jsonlite)
library(lubridate)
library(stringr)

# --- CONFIGURATION ---

# [UPDATED] Automatically get repo info from GitHub Action environment
full_repo <- Sys.getenv("GITHUB_REPOSITORY") # Returns "Owner/RepoName"
parts <- str_split(full_repo, "/", simplify = TRUE)

if (length(parts) == 2) {
  OWNER <- parts[1]
  REPO  <- parts[2]
} else {
  # Fallback for local testing (Replace with your details if testing locally)
  OWNER <- "EcoCast-Plus"
  REPO  <- "operationalization"
}

BRANCH <- "main" 
DAYS_OLD <- 7  # [UPDATED] Changed from 60 to 7 days

# Set to TRUE to ONLY print what would be deleted. 
# Set to FALSE to perform the actual deletion.
DRY_RUN <- FALSE 

# GitHub Token is automatically read from the environment in GitHub Actions
GITHUB_TOKEN <- Sys.getenv("GITHUB_PAT")

if (is.null(GITHUB_TOKEN) || GITHUB_TOKEN == "") {
  stop("GITHUB_TOKEN environment variable not set. This is required.")
}

HEADERS <- c(
  Authorization = paste("token", GITHUB_TOKEN),
  Accept = "application/vnd.github.v3+json"
)

# Calculate cutoff date
CUTOFF_DATE <- today() - days(DAYS_OLD)
print(paste("Cutoff Date (files older than this will be deleted):", CUTOFF_DATE))

# Adjust DATE_PATTERN if your file names use a different structure
DATE_PATTERN <- "(\\d{4}-\\d{2}-\\d{2})" 
DATE_FORMAT <- "%Y-%m-%d"

# ----------------------------------------------------

# Create function to clean a set of dirs from repo
clean_old_files <- function(directory_path, 
                            owner = OWNER, 
                            repo = REPO, 
                            branch = BRANCH, 
                            cutoff_date = CUTOFF_DATE, 
                            date_pattern = DATE_PATTERN,
                            date_format = DATE_FORMAT,
                            dry_run = DRY_RUN,
                            headers = HEADERS) {
  
  print(paste("--- Starting cleanup for directory:", directory_path, "---"))
  BASE_URL <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", directory_path)
  deletion_count <- 0
  
  # Step 1: Fetch directory contents
  response <- GET(BASE_URL, add_headers(.headers = headers))
  
  if (http_error(response)) {
    print(paste("Error fetching directory contents:", http_status(response)$message))
    if (response$status_code == 404) {
      print("Directory not found or is empty. Skipping.")
      return(0)
    }
    stop("GitHub API error encountered.")
  }
  
  files_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  if (is.null(files_data) || !is.data.frame(files_data) || nrow(files_data) == 0) {
    print("Directory is empty. Skipping.")
    return(0)
  }
  
  # Step 2: Iterate and process files
  for (i in 1:nrow(files_data)) {
    file <- files_data[i,]
    
    if (file$type != 'file') {
      next 
    }
    
    file_name <- file$name
    
    # Extract Date using Regex
    date_match <- str_extract(file_name, date_pattern)
    
    # Check if a date was actually found
    if (!is.na(date_match)) {
      
      tryCatch({
        file_date <- as.Date(date_match, format = date_format)
        
        # Step 3: Compare Date
        if (file_date < cutoff_date) {
          
          # Step 4: Delete file
          if (dry_run) {
            print(paste("👀 DRY RUN (Would Delete):", file_name, "(Date:", file_date, ")"))
          } else {
            
            delete_payload <- list(
              message = paste("Automated cleanup: Deleting old file", file_name),
              sha = file$sha, 
              branch = branch
            )
            
            delete_response <- DELETE(
              file$url, 
              add_headers(.headers = headers),
              body = delete_payload,
              encode = "json"
            )
            
            if (http_error(delete_response)) {
              print(paste("❌ FAILED to delete:", file_name, "- Status:", http_status(delete_response)$message))
            } else {
              print(paste("✅ DELETED:", file_name))
            }
          }
          deletion_count <- deletion_count + 1
        }
      }, error = function(e) {
        print(paste("⚠️ Skipping", file_name, ": Date parsing error (", e$message, ")"))
      })
      
    } else {
      # Optional: Print skipping message only for debugging to reduce log noise
      # print(paste("ℹ️ Skipping", file_name, ": No date found matching pattern."))
    }
  }
  
  print(paste("--- Directory cleanup finished. Total deleted:", deletion_count, "---"))
  return(deletion_count)
}
# ----------------------------------------------------

### Clean up dirs

# [UPDATED] List of actual directories used in your project
directories_to_clean <- c(
  "data_acquisition/netcdfs/cmems_ncdfs",   # Raw NetCDFs
  "model_prediction/gulf/predictions"       # Prediction TIFFs
)

total_deleted <- 0

# Loop through the list and apply the function
for (dir in directories_to_clean) {
  deleted_count <- clean_old_files(directory_path = dir)
  total_deleted <- total_deleted + deleted_count
}

print("=====================================================================")
print(paste("GLOBAL CLEANUP COMPLETE. TOTAL FILES DELETED:", total_deleted))
