library(dplyr)
library(temizhavaR)
library(dygraphs)
library(readxl)
library(writexl)

total_days <- 365
parameter_name <- "SO2"

if (0) {
  station_name <- "Adana-Seyhan"
  parameters <- c("SO2", "NO")

  daily_detail_data <- daily_detail_load_from_database(station_name)
  calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
  all_daily_detail_data <- all_daily_detail_load_from_database()
  create_hourly_time_series_graph(daily_detail_data, station_name, parameters)

}

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
so2_1 <- daily_list_stations_with_parameter(parameter_name)
so2_1 <- rbind(result_message, so2_1)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
so2_2 <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- so2_2 %>% arrange(desc(data_percentage))
so2_2 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
so2_3 <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
so2_3 <- rbind(result_message, so2_3)

so2_4 <- daily_station_average(parameter_name, threshold = 90)
result_sorted <- so2_4 %>% arrange(desc(average))
result_message <- print(paste(parameter_name, ": için istasyon ortalamaları"))
so2_4 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : Günlük ortalaması 40 µg/m3'ün üstündeki istasyonların listesi ve astigi gun sayisi" ))
so2_7 <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 40)
result_sorted <- so2_7$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
so2_7 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : Günlük ortalaması 125 µg/m3'ün üstündeki istasyonların listesi ve astigi gun sayisi" ))
so2_8 <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 125)
result_sorted <- so2_8$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
so2_8 <- rbind(result_message, result_sorted)

so2_10 <- daily_station_average(parameter_name, threshold = 90)
result_sorted <- so2_10 %>% arrange(desc(average))
result_message <- print(paste(parameter_name, ": için istasyon ortalamaları"))
so2_10 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : Yıllık ortalaması 20 µg/m3 üstü istasyonların listesi" ))
so2_11 <- list_stations_above_data_threshold(parameter_name, threshold = 20)
result_sorted <- so2_11 %>% arrange(desc(yearly_average))
so2_11 <- rbind(result_message, result_sorted)

output <- list(SO2_1 = so2_1,
               SO2_2 = so2_2,
               SO2_3 = so2_3,
               SO2_4 = so2_4,
               SO2_7 = so2_7,
               SO2_8 = so2_8,
               SO2_10 = so2_10,
               SO2_11 = so2_11)

write_output_to_excel(output, result_so2_daily_excel_file)
