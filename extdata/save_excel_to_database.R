library(readxl)
library(dplyr)
library(lubridate)
library(RSQLite)
library(temizhavaR)

init.temizhavaR()

YEAR <- options()$temizhavaR.YEAR

#SAVE EXCEL TO DATABASE FOR DAILY DETAIL
if (YEAR == "2022") {
  # create a connection to database
  mydb <- dbConnect(RSQLite::SQLite(), file.path(raw_dir,"temiz-hava.sqlite"))
  location2022 <- dbReadTable(mydb, paste0("location_", YEAR))
} else {
  #2023
  #istasyonlar <- list.files(pattern = "xlsx", recursive = TRUE)
  #gunluk_istasyonlar <- istasyonlar[grep("gunluk_detay", istasyonlar)]
}

error_log <- list()

istasyonlarS <- location2022

#istasyonlarS <- istasyonlarS[1:2,]

for (I in 1:nrow(istasyonlarS)) {
  #istasyon_file <- "Manisa/Manisa - Turgutlu_gunluk_detay_2023.xlsx"
  istasyon_file <- istasyonlarS$gunluk_files2022[I]
  istasyon_name <- istasyonlarS$Istasyonlar[I]
  print(paste(istasyon_name, " : GUNLUK"))
  a=1
  tryCatch({
    istasyon <- read_excel(istasyon_file, .name_repair = "unique_quiet")

    processed_data <- data_preprocessing(istasyon, istasyon_name)

    detail_save_to_database(processed_data, "daily_detail", verbose = FALSE)

    cat(paste("Processed and saved:", istasyon_file, "\n"))
  }, error = function(e) {
    # Log the error with file name
    error_log[[istasyon_name]] <- e
    cat(paste("Error processing:", istasyon_name, "\n"))
  })
}

# Print error log
if (length(error_log) > 0) {
  cat("Errors occurred during processing:\n")
  print(error_log)
}

# SAVE EXCEL TO DATABASE FOR HOURLY DETAIL
# istasyonlar <- list.files(pattern = "xlsx", recursive = TRUE)
# saatlik_istasyonlar <- istasyonlar[grep("saatlik_detay", istasyonlar)]
# error_log <- list()

for (I in 1:nrow(istasyonlarS)) {
  istasyon_file <- istasyonlarS$saatlik_files2022[I]
  istasyon_name <- istasyonlarS$Istasyonlar[I]

  print(paste(istasyon_name, " : SAATLIK"))
  tryCatch({
    istasyon <- read_excel(istasyon_file, .name_repair = "unique_quiet")

    processed_data <- data_preprocessing(istasyon, istasyon_name)

    detail_save_to_database(processed_data, "hourly_detail", verbose = FALSE)

    cat(paste("Processed and saved:", istasyon_name, "\n"))
  }, error = function(e) {

    error_log[[istasyon_name]] <- e
    cat(paste("Error processing:", e, "\n"))
  })
}

# Print error log
if (length(error_log) > 0) {
  cat("Errors occurred during processing:\n")
  print(error_log)
}
