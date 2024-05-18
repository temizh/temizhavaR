library(dplyr)
library(temizhavaR)
library(DBI)
library(dygraphs)

source(init.TemizHavaR.R)
# if there is no result_dir give error that edit couldnt found and run again

station_name <- "Erzincan"
station_names <- c("Mersin - Tarsus", "Adana-Seyhan")
parameters <- c("PM10")
total_days <- 365
parameter = "PM10"
parameter_name <- "PM10"
city_name <- "ADANA"
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
print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün üstündeki istasyonların listesi " ))
print(result_sorted)


result <- list_stations_below_data_threshold(parameter_name, threshold = 40)
result_sorted <- result %>%
  arrange(desc(yearly_average))
print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün altindaki istasyonların listesi " ))
print(result_sorted)

exceedance_days <- calculate_exceedance_days_daily(daily_detail_data, parameter, threshold = 50)
print(paste("50 esiginin uzerinde asilma", exceedance_days, "gun boyunca gerceklesti."))


exceedance_days <- calculate_exceedance_days_daily(daily_detail_data, parameter, threshold = 45)
print(paste("45 esiginin uzerinde asilma", exceedance_days, "gun boyunca gerceklesti."))

result_above <- calculate_above_exceedance_days_all_stations(parameter, threshold = 50)
print("Istasyonlar 50 esigini kac gun boyunca astilar:")
print(result_above$ExceedanceDays %>% arrange(desc(ExceedsThreshold)))

result_below <- calculate_below_exceedance_days_all_stations(parameter, threshold = 45)
PM10_6 <- ("Istasyonlar 45 esiginin kac gun boyunca altında kaldılar:")
print(PM10_6)
print(result_below$ExceedanceDays %>% arrange(desc(ExceedsThreshold)))

calculate_overall_average_by_city(parameter)

#excel file : paste(result_dir, "PM10_daily.xlsx", "/")
#add to excel PM10_6
