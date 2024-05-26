library(readxl)
library(dplyr)
library(lubridate)
library(RSQLite)
library(temizhavaR)

source("init.TemizHava.R")
setwd(raw_dir)

#SAVE EXCEL TO DATABASE FOR DAILY DETAIL
istasyonlar <- list.files(pattern = "xlsx", recursive = TRUE)
gunluk_istasyonlar <- istasyonlar[grep("gunluk_detay", istasyonlar)]
error_log <- list()

for (istasyon_file in gunluk_istasyonlar) {
  tryCatch({

    istasyon <- read_excel(istasyon_file)

    processed_data <- data_preprocessing(istasyon)

    daily_detail_save_to_database(processed_data)

    cat(paste("Processed and saved:", istasyon_file, "\n"))
  }, error = function(e) {
    # Log the error with file name
    error_log[[istasyon_file]] <- e
    cat(paste("Error processing:", istasyon_file, "\n"))
  })
}

# Print error log
if (length(error_log) > 0) {
  cat("Errors occurred during processing:\n")
  print(error_log)
}

# SAVE EXCEL TO DATABASE FOR HOURLY DETAIL
istasyonlar <- list.files(pattern = "xlsx", recursive = TRUE)
saatlik_istasyonlar <- istasyonlar[grep("saatlik_detay", istasyonlar)]
error_log <- list()

for (istasyon_file in saatlik_istasyonlar) {
  tryCatch({

    istasyon <- read_excel(istasyon_file)

    processed_data <- data_preprocessing(istasyon)

    hourly_detail_save_to_database(processed_data)

    cat(paste("Processed and saved:", istasyon_file, "\n"))
  }, error = function(e) {

    error_log[[istasyon_file]] <- e
    cat(paste("Error processing:", istasyon_file, "\n"))
  })
}

# Print error log
if (length(error_log) > 0) {
  cat("Errors occurred during processing:\n")
  print(error_log)
}














# for (istasyon_file in saatlik_istasyonlar) {
#   # Read the Excel file
#   istasyon <- read_excel(istasyon_file)
#
#   # Process the data
#   processed_data <- data_preprocessing(istasyon)
#
#   # Save processed data to the database
#   hourly_detail_save_to_database(processed_data)
# }







# for loop
#istasyon <- read_excel(gunluk_istasyon[1])


# hourly_detail_save_to_database(processed_data, "Adana - Çatalan")




