library(tidyverse)
library(readxl)
library(dplyr)
library(temizhavaR)
library(DBI)
library(readxl)
library(uuid)
library(dygraphs)

station_name <- "Mersin - Tarsus"
station_names <- c("Mersin - Tarsus", "Adana-Seyhan")
parameters <- c("PM10")
total_days <- 365
parameter = "PM10"
parameter_name <- "PM10"
daily_detail_data <- daily_detail_load_from_database(station_name)

create_hourly_time_series_graph(daily_detail_data, station_name, parameters)

#create_hourly_time_series_graph_multiple_stations(daily_detail_data, station_names, parameter)


calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)

print(paste(parameter_name,": Veri alınan istasyon listesi" ))
daily_list_stations_with_parameter(parameter_name)

print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
daily_list_stations_with_parameter_count(parameter_name)

print(paste(parameter_name,": icin istasyon ortalamaları" ))
daily_station_average(parameter_name, threshold = 90)

print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün üstündeki istasyonların listesi " ))
list_stations_above_data_threshold(parameter_name, threshold = 40)

print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün altındaki istasyonların listesi " ))
list_stations_below_data_threshold(parameter_name, threshold = 40)


exceedance_days <- calculate_exceedance_days_daily(daily_detail_data, parameter, threshold = 50)
print(paste("50 esiginin uzerinde asilma", exceedance_days, "gun boyunca gerceklesti."))


exceedance_days <- calculate_exceedance_days_daily(daily_detail_data, parameter, threshold = 45)
print(paste("45 esiginin uzerinde asilma", exceedance_days, "gun boyunca gerceklesti."))
