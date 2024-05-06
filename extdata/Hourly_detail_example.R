
##### Specified station based examples

library(tidyverse)
library(readxl)
library(dplyr)
library(temizhavaR)
library(DBI)
library(readxl)
library(uuid)
library(dygraphs)

# SAVE EXCEL TO DATABASE
# data_dir <- "C:/Hourly_detail/"
#
# istasyon <- read_excel(paste0(data_dir, "adana-catalan-saatlik-detay.xlsx"))
# processed_data <- data_preprocessing(istasyon)
# hourly_detail_save_to_database(processed_data, "Adana - Çatalan")
hourly_detail_data <- hourly_detail_load_from_database(processed_data, "Mersin - Huzurkent")
plot_time_series(hourly_detail_data, "PM10")
calculate_parameter_mean(hourly_detail_data, "NOX")

