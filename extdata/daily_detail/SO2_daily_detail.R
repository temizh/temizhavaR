library(dplyr)
library(temizhavaR)
library(DBI)
library(dygraphs)

station_name <- "Adana-Seyhan"
parameters <- c("SO2")
total_days <- 365
parameter = "SO2"
parameter_name <- "SO2"
daily_detail_data <- daily_detail_load_from_database(station_name)

create_hourly_time_series_graph(daily_detail_data, station_name, parameters)

calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)

print(paste(parameter_name,": Veri alınan istasyon listesi" ))
daily_list_stations_with_parameter(parameter_name)

print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
daily_list_stations_with_parameter_count(parameter_name)

result <- daily_station_average(parameter_name, threshold = 90)
result_sorted <- result %>%
  arrange(desc(average))
print(paste(parameter_name, ": için istasyon ortalamaları"))
print(result_sorted)

print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

result <- list_stations_above_data_threshold(parameter_name, threshold = 40)
result_sorted <- result %>%
  arrange(desc(yearly_average))
print(paste(parameter_name," : Gunluk ortalaması 40 µg/m3'ün üstündeki istasyonların listesi " ))
print(result_sorted)

result <- list_stations_above_data_threshold(parameter_name, threshold = 125)
result_sorted <- result %>%
  arrange(desc(yearly_average))
print(paste(parameter_name," : Gunluk ortalaması 125 µg/m3'ün üstündeki istasyonların listesi " ))
print(result_sorted)

result <- list_stations_above_data_threshold(parameter_name, threshold = 20)
result_sorted <- result %>%
  arrange(desc(yearly_average))
print(paste(parameter_name," : Yıllık ortalaması 20 µg/m3'ün üstündeki istasyonların listesi " ))
print(result_sorted)
