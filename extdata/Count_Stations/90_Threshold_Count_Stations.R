#library(tidyverse)
library(readxl)
library(temizhavaR)
library(uuid)

data_dir <- "C:/Hourly_detail/"
excel_files <- list.files(path = data_dir, pattern = "\\.xlsx$", full.names = TRUE)
all_station_counts <- c()

for (file in excel_files) {
  istasyon <- read_excel(file)
  processed_data <- data_preprocessing(istasyon)
  ####### SET PARAMETER
  param_name <- "CO"
  station_counts <- list_stations_counts_above_threshold(processed_data, param_name)
  all_station_counts <- c(all_station_counts, station_counts)
}

total_station_counts <- sum(all_station_counts)
cat(param_name, " parametresine sahip istasyon sayısı: ", total_station_counts, "\n")

