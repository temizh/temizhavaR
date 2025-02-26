library(readxl)
library(dplyr)
library(lubridate)
library(RSQLite)
library(temizhavaR)

init.temizhavaR()

YEAR <- options()$temizhavaR.YEAR

#INPUT : find and select excel file
if (YEAR == "2022") {

} else {
  #2023
  istasyonlar <- list.files(pattern = "xlsx", recursive = TRUE)
  gunluk_istasyonlar <- istasyonlar[grep("gunluk_detay", istasyonlar)]
  saatlik_istasyonlar <- istasyonlar[grep("saatlik_detay", istasyonlar)]
  stopifnot(length(gunluk_istasyonlar) == length(saatlik_istasyonlar))
}

error_log <- list()

# create a connection to database
mydb <- dbConnect(RSQLite::SQLite(), file.path(raw_dir,"temiz-hava.sqlite"))
locationT <- dbReadTable(mydb, paste0("location_", YEAR))
dbDisconnect(mydb)

if (1) {
  #Write to DB only a selected Station file contents
  My_station <- "Erzurum - Palandöken"
  locationT <- locationT[grep(My_station, locationT$Istasyonlar),]
} else {
  #Write to DB ALL found Station file contents
}

for (I in 1:nrow(locationT)) {
  istasyon_name <- locationT$Istasyonlar[I]

  #tryCatch({
  print(" ")
  istasyon_file <- gunluk_istasyonlar[grep(istasyon_name, gunluk_istasyonlar)]

  print(paste(istasyon_file))
  istasyonData <- read_excel(istasyon_file, .name_repair = "unique_quiet")
  processed_data <- data_preprocessing(istasyonData, istasyon_name)
  detail_save_to_database(processed_data, "daily_detail", verbose = FALSE)
  cat(paste("Processed and saved :", istasyon_file, "\n"))

  print(" ")
  istasyon_file <- saatlik_istasyonlar[grep(istasyon_name, saatlik_istasyonlar)]
  print(paste(istasyon_file))
  istasyonData <- read_excel(istasyon_file, .name_repair = "unique_quiet")
  processed_data <- data_preprocessing(istasyonData, istasyon_name)
  detail_save_to_database(processed_data, "hourly_detail", verbose = FALSE)
  cat(paste("Processed and saved:", istasyon_file, "\n"))

  #}, error = function(e) {
  # Log the error with file name
  #  error_log[[istasyon_name]] <- e
  #  cat(paste("Error processing:", istasyon_name, "\n"))
  #})
}

# Print error log
if (length(error_log) > 0) {
  cat("Errors occurred during processing:\n")
  print(error_log)
}

# Print error log
if (length(error_log) > 0) {
  cat("Errors occurred during processing:\n")
  print(error_log)
}
