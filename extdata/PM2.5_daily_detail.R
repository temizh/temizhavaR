library(dplyr)
library(temizhavaR)
library(DBI)
library(dygraphs)
# istasyonu sqlden alamadıgında boslukları birlestir
station_name <- "Adana-Seyhan"
parameters <- c("PM2.5")
total_days <- 365
parameter = "PM2.5"
parameter_name <- "\"PM2.5\""
city_name <- "ADANA"
daily_detail_data <- daily_detail_load_from_database(station_name)

create_hourly_time_series_graph(daily_detail_data, station_name, parameters)

calculate_parameter_mean(daily_detail_data , parameter, threshold = 0.9, total_days, verbose = TRUE)

print(paste(parameter_name,": Veri alınan istasyon listesi" ))
daily_list_stations_with_parameter(parameter_name)

print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
daily_list_stations_with_parameter_count(parameter_name)

print(paste(parameter_name,": Istasyon ortalamaları" ))
daily_station_average(parameter_name, threshold = 90)

print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

print(paste(parameter_name,": %75 veri alınan istasyon listesi" ))
daily_list_stations_with_parameter_threshold(parameter_name, threshold = 75)

print(paste(parameter_name," : %75 Veri alınan istasyon sayısı" ))
daily_count_stations_with_parameter_threshold(parameter_name, threshold = 75)

print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün üstündeki istasyonların listesi " ))
list_stations_above_data_threshold(parameter_name, threshold = 40)

print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün altındaki istasyonların listesi " ))
list_stations_below_data_threshold(parameter_name, threshold = 40)

exceedance_days <- calculate_exceedance_days_daily(daily_detail_data, parameter, threshold = 15)
print(paste("15 esiginin uzerinde asilma", exceedance_days, "gun boyunca gerceklesti."))

result_above <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 15)
print("Istasyonlar 15 esigini kac gun boyunca astilar:")
print(result_above$ExceedanceDays %>% arrange(desc(ExceedsThreshold)))

result_below <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 15)
print("Istasyonlar 15 esiginin kac gun boyunca altında kaldılar:")
print(result_below$ExceedanceDays %>% arrange(desc(ExceedsThreshold)))

calculate_overall_average_by_city(parameter_name)

new_pm25_from_pm10_yearly_average()
