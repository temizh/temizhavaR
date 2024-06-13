library(dplyr)
library(temizhavaR)
library(dygraphs)

total_days <- 8761
parameter_name <- "NO2"

if(0) {
  station_name <- "Erzincan"
  parameters <- c("NO2")
  all_hourly_detail_data <- all_hourly_detail_load_from_database()

  create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)
  calculate_parameter_mean(hourly_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
}

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
no2_1 <- hourly_list_stations_with_parameter(parameter_name)
no2_1 <- rbind(result_message, no2_1)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
no2_2 <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- no2_2 %>% arrange(desc(data_percentage))
no2_2 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
no2_3 <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
no2_3 <- rbind(result_message, no2_3)

result_message <- print(paste(parameter_name,": Saatlik ortalaması 200 esik degerini 18 kere asan istasyonlar ve kaç gün boyunca" ))
no2_4 <- hourly_above_exceedance_days_double_threshold(parameter_name, threshold = 200, 18)
result_sorted <- no2_4$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
no2_4 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name,": 3 ardisik saat " , threshold = 400 , "degerini asan istasyonlar ve kac kere astiklari" ))
no2_10 <- consecutive_hourly_list_stations(parameter_name, threshold = 400)
no2_10 <- rbind(result_message, no2_10)

output <- list(NO2_1 = no2_1,
               NO2_2 = no2_2,
               NO2_3 = no2_3,
               NO2_4 = no2_4,
               NO2_10 = no2_10)

write_output_to_excel(output, result_no2_hourly_excel_file)
