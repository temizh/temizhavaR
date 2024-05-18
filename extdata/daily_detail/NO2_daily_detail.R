library(dplyr)
library(temizhavaR)
library(DBI)
library(dygraphs)

station_name <- "Adana-Seyhan"
parameters <- c("NO2")
total_days <- 365
parameter = "NO2"
parameter_name <- "NO2"
daily_detail_data <- daily_detail_load_from_database(station_name)

create_hourly_time_series_graph(daily_detail_data, station_name, parameters)

calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)

print(paste(parameter_name,": Veri alınan istasyon listesi" ))
daily_list_stations_with_parameter(parameter_name)

print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

result_above <- calculate_above_exceedance_days_all_stations(parameter, threshold = 25)
print("Istasyonlar 25 esigini kac gun boyunca astilar:")
print(result_above$ExceedanceDays %>% arrange(desc(ExceedsThreshold)))

print(paste(parameter,": Il yillik ortalamasi" ))
calculate_overall_average_by_city(parameter)

result <- list_stations_above_data_threshold(parameter_name, threshold = 40)
result_sorted <- result %>%
  arrange(desc(yearly_average))
print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün üstündeki istasyonların listesi " ))
print(result_sorted)

result <- list_stations_above_data_threshold(parameter_name, threshold = 10)
result_sorted <- result %>%
  arrange(desc(yearly_average))
print(paste(parameter_name," : Yıllık ortalaması 10 µg/m3'ün üstündeki istasyonların listesi " ))
print(result_sorted)
