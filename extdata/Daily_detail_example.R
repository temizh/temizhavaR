#library(tidyverse)
library(readxl)
library(dplyr)
library(temizhavaR)
library(readxl)
library(uuid)
library(dygraphs)

station_name <- "Adana - Çukurova"
parameters <- c("PM2.5")
total_days <- 365
parameter_name <- "'PM2.5'"
#parameter_name <- "PM10"
create_hourly_time_series_graph(daily_detail_data, station_name, parameters)

daily_detail_data <- daily_detail_load_from_database(station_name)

x <- calculate_parameter_mean(daily_detail_data, parameter = parameter_name, threshold = 0.9, total_days, verbose = TRUE)
daily_list_stations_with_parameter(parameter_name)
print(paste(parameter_name,": Veri alınan istasyon listesi" ))

daily_list_stations_with_parameter_count(parameter_name)
print(paste(parameter_name,": Veri alınan istasyon sayısı" ))

average -> station_average(parameter_name, threshold = 90)
daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
list_stations_above_data_threshold(parameter_name, threshold = 40)
list_stations_below_data_threshold(parameter_name, threshold = 40)
average <- station_average(parameter_name, threshold = 90)

