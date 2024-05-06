library(tidyverse)
library(readxl)
library(dplyr)
library(temizhavaR)
library(DBI)
library(readxl)
library(uuid)
library(dygraphs)


#SAVE EXCEL TO DATABASE
# data_dir <- "C:/Daily_detail/"
#
# istasyon <- read_excel(paste0(data_dir, "mersin-tarsus-gunluk-detay.xlsx"))
# processed_data <- data_preprocessing(istasyon)
# daily_detail_save_to_database(processed_data, "Mersin - Tarsus")
daily_detail_data <- daily_detail_load_from_database(processed_data, "Mersin - Huzurkent")
create_station_graph(daily_detail_data, "Tarih", "PM10")
calculate_parameter_mean(daily_detail_data, "NOX")
