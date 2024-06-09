library(dplyr)
library(temizhavaR)

station_name <- "Adana-Seyhan"
parameters <- c("NO2")
total_days <- 365
parameter = "NO2"
parameter_name <- "NO2"
daily_detail_data <- daily_detail_load_from_database(station_name)
all_daily_detail_data <- all_daily_detail_load_from_database()

create_hourly_time_series_graph(daily_detail_data, station_name, parameters)

calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
print(result_message)
no2_1 <- daily_list_stations_with_parameter(parameter_name)
no2_1 <- rbind(result_message, no2_1)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
print(result_message)
no2_2 <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- no2_2 %>% arrange(desc(data_percentage))
no2_2 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
print(result_message)
no2_3 <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
no2_3 <- rbind(result_message, no2_3)

result_message <- print(paste(parameter_name,": Günlük ortalaması 25 esik degerini 3 kere asan istasyonlar ve kaç gün boyunca" ))
print(result_message)
no2_5 <- daily_above_exceedance_days_double_threshold(parameter_name, threshold = 25, 3)
result_sorted <- no2_5$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
no2_5 <- rbind(result_message, result_sorted)

no2_6 <- daily_station_average(parameter_name, threshold = 90)
result_sorted <- no2_6 %>%
  arrange(desc(average))
result_message <- print(paste(parameter_name, ": için istasyon ortalamaları"))
print(result_message)
no2_6 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3 üstü istasyonların listesi ve kaç gün boyunca " ))
print(result_message)
no2_7 <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 40)
result_sorted <- no2_7$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
no2_7 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : Yıllık ortalaması 10 µg/m3 üstü istasyonların listesi ve kaç gün boyunca " ))
print(result_message)
no2_8 <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 10)
result_sorted <- no2_8$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
no2_8<- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name,": İl NO2 yıllık ortalaması" ))
print(result_message)
no2_9 <- calculate_overall_average_by_city_threshold(parameter)
result_sorted <- no2_9 %>% arrange(desc(Ortalama))
no2_9 <- rbind(result_message, result_sorted)


output <- list(NO2_1 = no2_1,
               NO2_2 = no2_2,
               NO2_3 = no2_3,
               NO2_5 = no2_5,
               NO2_6 = no2_6,
               NO2_7 = no2_7,
               NO2_8 = no2_8,
               NO2_9 = no2_9)
write_xlsx(
  output,
  path = result_no2_daily_excel_file,
  col_names = FALSE,
  format_headers = TRUE,
  use_zip64 = FALSE
)






