### Script to delete old processed files locally
# This script deletes files from the local runner environment. 
# The GitHub Action YAML will then commit these deletions in a single batch.

library(lubridate)
library(stringr)

# --- CONFIGURATION ---
DAYS_OLD <- 7
CUTOFF_DATE <- today() - days(DAYS_OLD)
print(paste("Cutoff Date (files older than this will be deleted):", CUTOFF_DATE))

DATE_PATTERN <- "(\\d{4}-\\d{2}-\\d{2})" 
DATE_FORMAT <- "%Y-%m-%d"

directories_to_clean <- c(
  "data_acquisition/netcdfs/cmems_ncdfs",   # Raw NetCDFs
  "model_prediction/gulf/predictions"       # Prediction TIFFs
)

total_deleted <- 0

# --- EXECUTE CLEANUP ---
for (dir in directories_to_clean) {
  print(paste("--- Scanning directory:", dir, "---"))
  
  if (!dir.exists(dir)) {
    print("Directory does not exist. Skipping.")
    next
  }
  
  files <- list.files(dir, full.names = TRUE)
  
  for (file_path in files) {
    file_name <- basename(file_path)
    date_match <- str_extract(file_name, DATE_PATTERN)
    
    if (!is.na(date_match)) {
      tryCatch({
        file_date <- as.Date(date_match, format = DATE_FORMAT)
        
        if (file_date < CUTOFF_DATE) {
          # Delete the file locally
          file.remove(file_path)
          print(paste("✅ DELETED:", file_name))
          total_deleted <- total_deleted + 1
        }
      }, error = function(e) {
        print(paste("⚠️ Skipping", file_name, ": Date parsing error (", e$message, ")"))
      })
    }
  }
}

print("=====================================================================")
print(paste("GLOBAL CLEANUP COMPLETE. TOTAL FILES DELETED:", total_deleted))
