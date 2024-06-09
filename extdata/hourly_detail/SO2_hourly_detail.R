library(dplyr)
library(temizhavaR)
library(dygraphs)
library(writexl)

station_name <- "Adana-Seyhan"
parameters <- c("SO2")
total_days <- 8761
parameter = "SO2"
parameter_name <- "SO2"
hourly_detail_data <- hourly_detail_load_from_database(station_name)
all_hourly_detail_data <- all_hourly_detail_load_from_database()

create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)

calculate_parameter_mean(hourly_detail_data , parameter, threshold = 0.9, total_days, verbose = TRUE)

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
print(result_message)
so2_1 <- hourly_list_stations_with_parameter(parameter_name)
so2_1 <- rbind(result_message, so2_1)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
print(result_message)
so2_2 <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- so2_2 %>% arrange(desc(data_percentage))
so2_2 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
print(result_message)
so2_3 <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
so2_3 <- rbind(result_message, so2_3)

result_message <- print(paste(parameter_name," : Saatlik ortalaması 350 esik degerini asan istasyonlar ve kac gün boyunca" ))
print(result_message)
so2_5 <- hourly_above_exceedance_days_threshold(parameter_name, threshold = 350)
result_sorted <- so2_5$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
so2_5 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name,": Saatlik ortalaması 350 esik degerini 24 kere asan istasyonlar ve kaç gün boyunca" ))
print(result_message)
so2_6 <- hourly_above_exceedance_days_double_threshold(parameter_name, threshold = 350, 24)
result_sorted <- so2_6$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
so2_6 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name,": Saatlik ortalaması 125 esik degerini 3 kere asan istasyonlar ve kaç gün boyunca" ))
print(result_message)
so2_9 <- hourly_above_exceedance_days_double_threshold(parameter_name, threshold = 125, 3)
result_sorted <- so2_9$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
so2_9 <- rbind(result_message, result_sorted)


output <- list(SO2_1 = so2_1,
               SO2_2 = so2_2,
               SO2_3 = so2_3,
               SO2_4 = so2_4,
               SO2_7 = so2_7,
               SO2_8 = so2_8,
               SO2_10 = so2_10,
               SO2_11 = so2_11)
write_xlsx(
  output,
  path = result_so2_hourly_excel_file,
  col_names = FALSE,
  format_headers = TRUE,
  use_zip64 = FALSE
)




#
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
# print(paste(parameter_name,": Saatlik ortalaması 350 esik degerini asan istasyonlar" ))
# list_stations_above_hourly_mean(parameter_name, threshold = 350)
#
# print(paste(parameter_name,": Saatlik ortalaması 350 esik degerini 24 kere asan istasyonlar" ))
# hourly_calculate_above_exceedance_days_all_stations(parameter_name, threshold = 350, exceed_limit = 24)
#
# print(paste(parameter_name,": Saatlik ortalaması 125 esik degerini 3 kere asan istasyonlar" ))
# hourly_calculate_above_exceedance_days_all_stations(parameter_name, threshold = 125, exceed_limit = 3)
#
# print(paste(parameter_name,": 3 ardisik saat " , threshold = 500 , "degerini asan istasyonlar ve kac kere astiklari"))
# d <- consecutive_hourly_list_stations(parameter_name, threshold = 6)
#

