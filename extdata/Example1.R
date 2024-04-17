library(tidyverse)
library(readxl)
library(dplyr)
library(temizhavaR)

data_dir <- "C:/Fonksiyon_sehirler/"

istasyon <- read_excel(paste0(data_dir, "adana-catalan-saatlik-detay.xlsx"))
processed_data <- data_preprocessing(istasyon)
list_stations(processed_data, "PM10")
count_stations(processed_data, "PM10")
