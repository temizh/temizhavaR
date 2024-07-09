library(dplyr)
library(temizhavaR)
library(dygraphs)
library(readxl)
library(writexl)

total_days <- 365
parameter_name <- "SO2"

init.temizhavaR()

if (0) {
  station_name <- "Adana-Seyhan"
  parameters <- c("SO2", "NO")

  daily_detail_data <- daily_detail_load_from_database(station_name)
  calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
  all_daily_detail_data <- all_daily_detail_load_from_database()
  create_hourly_time_series_graph(daily_detail_data, station_name, parameters)
}

output <- list(SO2_1 = list(),
               SO2_2 = list(),
               SO2_3 = list(),
               SO2_4 = list(),
               SO2_7 = list(),
               SO2_8 = list(),
               SO2_10 = list(),
               SO2_11 = list())

output$SO2_1$result_message <- print(paste0(parameter_name, "_1 : Veri alınan istasyon listesi" ))
output$SO2_1$data <- daily_list_stations_with_parameter(parameter_name)

output$SO2_2$result_message <- print(paste0(parameter_name, "_2 : %90 veri alınan istasyon listesi" ))
output$SO2_2$data <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

output$SO2_3$result_message <- print(paste0(parameter_name, "_3 : %90 Veri alınan istasyon sayısı" ))
output$SO2_3$data <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

output$SO2_4$result_message <- print(paste0(parameter_name, "_4 : için istasyon ortalamaları"))
output$SO2_4$data <- daily_station_average(parameter_name, threshold = 90)

# 5 & 6 missing

output$SO2_7$result_message <- print(paste0(parameter_name, "_7 : Günlük ortalaması 40 µg/m3'ün üstündeki istasyonların listesi ve aştıkları gun sayısı" ))
output$SO2_7$data <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 40)

output$SO2_8$result_message <- print(paste0(parameter_name, "_8 : Günlük ortalaması 125 µg/m3'ün üstündeki istasyonların listesi ve aştıkları gun sayısı" ))
output$SO2_8$data <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 125)

# 9 missing

output$SO2_10$result_message <- print(paste0(parameter_name, "_10 : için istasyon ortalamaları"))
output$SO2_10$data <- daily_station_average(parameter_name, threshold = 90)

output$SO2_11$result_message <- print(paste0(parameter_name, "_11 : Yıllık ortalaması 20 µg/m3 üstü istasyonların listesi" ))
output$SO2_11$data <- list_stations_above_data_threshold(parameter_name, threshold = 20)

write_output_to_excel(output, result_so2_daily_excel_file)
