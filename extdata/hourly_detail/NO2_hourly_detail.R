library(dplyr)
library(temizhavaR)
library(DBI)
library(dygraphs)

station_name <- "Erzincan"
parameters <- c("NO2")
total_days <- 8761
parameter = "NO2"
parameter_name <- "NO2"
hourly_detail_data <- hourly_detail_load_from_database(station_name)

create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)

calculate_parameter_mean(hourly_detail_data , parameter, threshold = 0.9, total_days, verbose = TRUE)

print(paste(parameter_name,": Veri alınan istasyon listesi" ))
hourly_list_stations_with_parameter(parameter_name)

print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

print(paste(parameter_name,": Saatlik ortalaması 200 esik degerini 18 kere asan istasyonlar" ))
hourly_calculate_above_exceedance_days_all_stations(parameter_name, threshold = 200, exceed_limit = 18)

print(paste(parameter_name,": 3 ardisik saat " , threshold = 5 , "degerini asan istasyonlar ve kac kere astiklari"))
consecutive_hourly_list_stations(parameter_name, threshold = 50)


