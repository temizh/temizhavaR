
##### Specified station based examples

library(tidyverse)
library(readxl)
library(dplyr)
library(temizhavaR)
library(DBI)
library(readxl)
library(uuid)
library(dygraphs)

data_dir <- "C:/Hourly_detail/"

istasyon <- read_excel(paste0(data_dir, "adana-meteoroloji-saatlik-detay.xlsx"))
processed_data <- data_preprocessing(istasyon)
hourly_detail_save_to_database(processed_data, "Adana - Meteoroloji")
aa <- hourly_detail_load_from_database(processed_data, "Adana - Meteoroloji")
#### database'den gelen veriler
create_station_graph(aa, "Tarih", "PM10")
calculate_parameter_mean(aa, "NOX")

