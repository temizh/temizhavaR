library(dplyr)
library(temizhavaR)
library(DBI)
library(dygraphs)

station_name <- "Erzincan"
parameters <- c("NO2")
total_days <- 8761
parameter = "NO2"
parameter_name <- "NO2"
all_hourly_detail_data <- all_hourly_detail_load_from_database()

create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)

calculate_parameter_mean(hourly_detail_data , parameter, threshold = 0.9, total_days, verbose = TRUE)

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
print(result_message)
no2_1 <- hourly_list_stations_with_parameter(parameter_name)
no2_1 <- rbind(result_message, no2_1)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
print(result_message)
no2_2 <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- no2_2 %>% arrange(desc(data_percentage))
no2_2 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
print(result_message)
no2_3 <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
no2_3 <- rbind(result_message, no2_3)


result_message <- print(paste(parameter_name,": Saatlik ortalaması 200 esik degerini 18 kere asan istasyonlar ve kaç gün boyunca" ))
print(result_message)
no2_4 <- hourly_above_exceedance_days_double_threshold(parameter_name, threshold = 200, 18)
result_sorted <- no2_4$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
no2_4 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name,": 3 ardisik saat " , threshold = 400 , "degerini asan istasyonlar ve kac kere astiklari" ))
print(result_message)
no2_10 <- consecutive_hourly_list_stations(parameter_name, threshold = 400)
no2_10 <- rbind(result_message, no2_10)

output <- list(NO2_1 = no2_1,
               NO2_2 = no2_2,
               NO2_3 = no2_3,
               NO2_4 = no2_4,
               NO2_10 = no2_10)
write_xlsx(
  output,
  path = result_no2_hourly_excel_file,
  col_names = FALSE,
  format_headers = TRUE,
  use_zip64 = FALSE
)

















#
# print(paste(parameter_name,": Veri alınan istasyon listesi" ))
# hourly_list_stations_with_parameter(parameter_name)
#
# print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
# hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
#
# print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
# hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
#
# print(paste(parameter_name,": Saatlik ortalaması 200 esik degerini 18 kere asan istasyonlar" ))
# hourly_calculate_above_exceedance_days_all_stations(parameter_name, threshold = 200, exceed_limit = 18)
#
# print(paste(parameter_name,": 3 ardisik saat " , threshold = 5 , "degerini asan istasyonlar ve kac kere astiklari"))
# consecutive_hourly_list_stations(parameter_name, threshold = 50)



