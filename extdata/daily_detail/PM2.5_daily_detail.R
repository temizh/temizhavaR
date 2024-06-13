library(dplyr)
library(temizhavaR)
library(dygraphs)
library(writexl)

total_days <- 365
parameter_name <- "\"PM2.5\""

if (0) {
  station_name <- "Mersin - Akdeniz"
  parameters <- c("PM2.5")
  daily_detail_data <- daily_detail_load_from_database(station_name)
  calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
  all_daily_detail_data <- all_daily_detail_load_from_database()
  create_hourly_time_series_graph(daily_detail_data, station_name, parameters)
}

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
pm25_1 <- daily_list_stations_with_parameter(parameter_name)
pm25_1 <- rbind(result_message, pm25_1)

result_message <- print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
pm25_2 <- daily_list_stations_with_parameter_count(parameter_name)
pm25_2 <- rbind(result_message, pm25_2)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
pm25_3 <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- pm25_3 %>% arrange(desc(data_percentage))
pm25_3 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
pm25_4 <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
pm25_4 <- rbind(result_message, pm25_4)

result_message <- print(paste(parameter_name,": %75 veri alınan istasyon listesi" ))
pm25_5 <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 75)
result_sorted <- pm25_5 %>% arrange(desc(data_percentage))
pm25_5 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : %75 Veri alınan istasyon sayısı" ))
pm25_6 <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 75)
pm25_6 <- rbind(result_message, pm25_6)

pm25_7 <- daily_station_average(parameter_name, threshold = 75)
result_sorted <- pm25_7 %>% arrange(desc(average))
result_message <- print(paste(parameter_name, ": için istasyon ortalamaları"))
pm25_7 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : Yıllık ortalaması 5 µg/m3 üstü istasyonların listesi ve kaç gün boyunca " ))
pm25_8 <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 5)
result_sorted <- pm25_8$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm25_8 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : Yıllık ortalaması 5 µg/m3'ün altı istasyonların listesi ve kaç gün boyunca " ))
pm25_9 <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 5)
result_sorted <- pm25_9$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm25_9 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : Günlük ortalaması 15 µg/m3'ün üstündeki istasyonların listesi ve astigi gun sayisi" ))
pm25_10 <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 15)
result_sorted <- pm25_10$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm25_10 <- rbind(result_message, result_sorted)

#pm25_11 EKSIK

result_message <- print(paste(parameter_name," : Günlük ortalaması 15 µg/m3'ün altındaki istasyonların listesi ve astigi gun sayisi " ))
pm25_12 <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 15)
result_sorted <- pm25_12$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm25_12 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," :PM10 yıllık ortalamalarından 0,6667 faktörü ile çarpılarak elde edilecek hesaplanan PM25" ))
pm25_13 <- new_pm25_from_pm10_yearly_average()
pm25_13 <- rbind(result_message, pm25_13)

result_message <- print(paste(parameter_name," :İl PM25 yıllık ortalaması" ))
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

write_output_to_excel(output, result_pm25_daily_excel_file)
