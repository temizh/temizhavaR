library(dplyr)
library(temizhavaR)
library(DBI)
library(dygraphs)

station_name <- "Adana-Seyhan"
parameters <- c("O3")
total_days <- 365
parameter = "O3"
parameter_name <- "O3"
daily_detail_data <- daily_detail_load_from_database(station_name)

create_hourly_time_series_graph(daily_detail_data, station_name, parameters)

calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)

print(paste(parameter_name,": Veri alınan istasyon listesi" ))
daily_list_stations_with_parameter(parameter_name)

print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
daily_list_stations_with_parameter_count(parameter_name)

print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
