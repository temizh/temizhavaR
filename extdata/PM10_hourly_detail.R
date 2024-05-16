
#SONUCLAR EXCEL'e




library(tidyverse)
library(readxl)
library(dplyr)
library(temizhavaR)
library(DBI)
library(readxl)
library(uuid)
library(dygraphs)
# istasyonu sqlden alamadıgında boslukları birlestir
station_name <- "Adana-Seyhan"
parameters <- c("PM10")
total_days <- 8761
parameter = "PM10"
parameter_name <- "PM10"
hourly_detail_data <- hourly_detail_load_from_database(station_name)

create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)

calculate_parameter_mean(hourly_detail_data , parameter, threshold = 0.9, total_days, verbose = TRUE)

print(paste(parameter_name,": Veri alınan istasyon listesi" ))
hourly_list_stations_with_parameter(parameter_name)

print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
hourly_list_stations_with_parameter_count(parameter_name)

print(paste(parameter_name,": Istasyon ortalamaları" ))
hourly_station_average(parameter_name, threshold = 90)

print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
