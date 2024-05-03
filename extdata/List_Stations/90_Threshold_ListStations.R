library(tidyverse)
library(readxl)
library(temizhavaR)
library(DBI)
library(uuid)

##BUNLARI GUNLUK DETAY VERiLERiNE UYGULA

data_dir <- "C:/Hourly_detail/"


excel_files <- list.files(path = data_dir, pattern = "\\.xlsx$", full.names = TRUE)

all_station_names <- c()

# apply all excel files
for (file in excel_files) {

  istasyon <- read_excel(file)


  processed_data <- data_preprocessing(istasyon)


  #hourly_detail_save_to_database(processed_data)



  station_names <- list_stations_above_threshold(processed_data, "CO")

  all_station_names <- c(all_station_names, station_names)
}

all_station_names <- unique(all_station_names)

# Tum istasyonları listeleEE
for (station_name in all_station_names) {
  cat("İstasyon: ", station_name, "\n")
}




