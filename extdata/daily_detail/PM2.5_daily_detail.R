library(dplyr)
library(temizhavaR)
library(dygraphs)

# istasyonu sqlden alamadıgında boslukları birlestir
station_name <- "Mersin - Akdeniz"
parameters <- c("PM2.5")
total_days <- 365
parameter = "PM2.5"
parameter_name <- "\"PM2.5\""
city_name <- "ADANA"
daily_detail_data <- daily_detail_load_from_database(station_name)
all_daily_detail_data <- all_daily_detail_load_from_database()

create_hourly_time_series_graph(daily_detail_data, station_name, parameters)

calculate_parameter_mean(daily_detail_data , parameter, threshold = 0.9, total_days, verbose = TRUE)

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
print(result_message)
pm25_1 <- daily_list_stations_with_parameter(parameter_name)
pm25_1 <- rbind(result_message, pm25_1)

result_message <- print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
print(result_message)
pm25_2 <- daily_list_stations_with_parameter_count(parameter_name)
pm25_2 <- rbind(result_message, pm25_2)


result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
print(result_message)
pm25_3 <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- pm25_3 %>% arrange(desc(data_percentage))
pm25_3 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
print(result_message)
pm25_4 <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
pm25_4 <- rbind(result_message, pm25_4)

result_message <- print(paste(parameter_name,": %75 veri alınan istasyon listesi" ))
print(result_message)
pm25_5 <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 75)
result_sorted <- pm25_5 %>% arrange(desc(data_percentage))
pm25_5 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : %75 Veri alınan istasyon sayısı" ))
print(result_message)
pm25_6 <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 75)
pm25_6 <- rbind(result_message, pm25_6)

pm25_7 <- daily_station_average(parameter_name, threshold = 75)
result_sorted <- pm25_7 %>%
  arrange(desc(average))
result_message <- print(paste(parameter_name, ": için istasyon ortalamaları"))
print(result_message)
pm25_7 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : Yıllık ortalaması 5 µg/m3 üstü istasyonların listesi ve kaç gün boyunca " ))
print(result_message)
pm25_8 <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 5)
result_sorted <- pm25_8$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm25_8 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : Yıllık ortalaması 5 µg/m3'ün altı istasyonların listesi ve kaç gün boyunca " ))
print(result_message)
pm25_9 <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 5)
result_sorted <- pm25_9$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm25_9 <- rbind(result_message, result_sorted)


# result_message <- print(paste(parameter_name," : Yıllık ortalaması 5 µg/m3 üstü istasyonların listesi" ))
# print(result_message)
# pm25_8 <- list_stations_above_data_threshold(parameter_name, threshold = 5)
# result_sorted <- pm25_8 %>% arrange(desc(yearly_average))
# pm25_8 <- rbind(result_message, result_sorted)
#
#
# result_message <- print(paste(parameter_name," : Yıllık ortalaması 5 µg/m3 altı istasyonların listesi" ))
# print(result_message)
# pm25_9 <- list_stations_below_data_threshold(parameter_name, threshold = 5)
# result_sorted <- pm25_9 %>% arrange(desc(yearly_average))
# pm25_9 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : Günlük ortalaması 15 µg/m3'ün üstündeki istasyonların listesi ve astigi gun sayisi" ))
print(result_message)
pm25_10 <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 15)
result_sorted <- pm25_10$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm25_10 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : Günlük ortalaması 15 µg/m3'ün altındaki istasyonların listesi ve astigi gun sayisi " ))
print(result_message)
pm25_12 <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 15)
result_sorted <- pm25_12$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm25_12 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," :PM10 yıllık ortalamalarından 0,6667 faktörü ile çarpılarak elde edilecek hesaplanan PM25" ))
print(result_message)
pm25_13 <- new_pm25_from_pm10_yearly_average()
pm25_13 <- rbind(result_message, pm25_13)

result_message <- print(paste(parameter_name," :İl PM25 yıllık ortalaması" ))
print(result_message)
pm25_14 <- calculate_overall_average_by_city(parameter_name)
result_sorted <- pm25_14 %>% arrange(desc(Ortalama))
pm25_14 <- rbind(result_message, result_sorted)

output <- list(PM25_1 = pm25_1,
               PM25_2 = pm25_2,
               PM25_3 = pm25_3,
               PM25_4 = pm25_4,
               PM25_5 = pm25_5,
               PM25_6 = pm25_6,
               PM25_7 = pm25_7,
               PM25_8 = pm25_8,
               PM25_9 = pm25_9,
               PM25_10 = pm25_10,
               PM25_12 = pm25_12,
               PM25_13 = pm25_13,
               PM25_14 = pm25_14 )
write_xlsx(
  output,
  path = result_pm25_daily_excel_file,
  col_names = FALSE,
  format_headers = TRUE,
  use_zip64 = FALSE
)




# print(paste(parameter_name,": Veri alınan istasyon listesi" ))
# daily_list_stations_with_parameter(parameter_name)
#
# print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
# daily_list_stations_with_parameter_count(parameter_name)
#
# print(paste(parameter_name,": Istasyon ortalamaları" ))
# daily_station_average(parameter_name, threshold = 90)
#
# print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
# daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
#
# print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
# daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
#
# print(paste(parameter_name,": %75 veri alınan istasyon listesi" ))
# daily_list_stations_with_parameter_threshold(parameter_name, threshold = 75)
#
# print(paste(parameter_name," : %75 Veri alınan istasyon sayısı" ))
# daily_count_stations_with_parameter_threshold(parameter_name, threshold = 75)
#
# print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün üstündeki istasyonların listesi " ))
# list_stations_above_data_threshold(parameter_name, threshold = 40)
#
# print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün altındaki istasyonların listesi " ))
# list_stations_below_data_threshold(parameter_name, threshold = 40)
#
# exceedance_days <- calculate_exceedance_days_daily(daily_detail_data, parameter, threshold = 15)
# print(paste("15 esiginin uzerinde asilma", exceedance_days, "gun boyunca gerceklesti."))
#
# result_above <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 15)
# print("Istasyonlar 15 esigini kac gun boyunca astilar:")
# print(result_above$ExceedanceDays %>% arrange(desc(ExceedsThreshold)))
#
# result_below <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 15)
# print("Istasyonlar 15 esiginin kac gun boyunca altında kaldılar:")
# print(result_below$ExceedanceDays %>% arrange(desc(ExceedsThreshold)))
#
# calculate_overall_average_by_city(parameter_name)
#
# new_pm25_from_pm10_yearly_average()
