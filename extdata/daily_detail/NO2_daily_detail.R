library(dplyr)
library(temizhavaR)

total_days <- 365
parameter_name <- "NO2"

init.temizhavaR()

if (0) {
  station_name <- "Konya Laboratuvar Yaygınlaştırma"
  station_name <- "Konya Laboratuvar Yaygınlaştırma"
  station_name <- "Kocaeli - Yeniköy-MTHM"
  station_name <- "Kocaeli - Yeniköy-MTHM"
  station_name <- "Yalova-Altınova-MTHM"

  parameters <- c("NO2")
  daily_detail_data <- daily_detail_load_from_database(station_name)
  all_daily_detail_data <- all_daily_detail_load_from_database()

  create_hourly_time_series_graph(daily_detail_data, station_name, parameters)
  calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
}

output <- list(NO2_1 = list(),
               NO2_2 = list(),
               NO2_3 = list(),
               NO2_5 = list(),
               NO2_6 = list(),
               NO2_7 = list(),
               NO2_8 = list(),
               NO2_9 = list())

output$NO2_1$result_message <- print(paste0(parameter_name, "_1 : Veri alınan istasyon listesi" ))
output$NO2_1$data <- daily_list_stations_with_parameter(parameter_name)

output$NO2_2$result_message <- print(paste0(parameter_name, "_2 : %90 veri alınan istasyon listesi" ))
output$NO2_2$data <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

output$NO2_3$result_message  <- print(paste0(parameter_name, "_3 : %90 Veri alınan istasyon sayısı" ))
output$NO2_3$data <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

# 4 missing

output$NO2_5$result_message  <- print(paste0(parameter_name, "_5 : Günlük ortalaması 25 esik degerini 3 kere asan istasyonlar ve aştıkları gun sayisi" ))
output$NO2_5$data <- daily_above_exceedance_days_double_threshold(parameter_name, threshold = 25, 3)

output$NO2_6$result_message <- print(paste0(parameter_name, "_6 : için istasyon ortalamaları"))
output$NO2_6$data <- daily_station_average(parameter_name, threshold = 90)

output$NO2_7$result_message <- print(paste0(parameter_name, "_7 : Yıllık ortalaması 40 µg/m3 üstü istasyonların listesi ve aştıkları gun sayisi" ))
output$NO2_7$data <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 40)

output$NO2_8$result_message <- print(paste0(parameter_name, "_8 : Yıllık ortalaması 10 µg/m3 üstü istasyonların listesi ve aştıkları gun sayisi" ))
output$NO2_8$data <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 10)

output$NO2_9$result_message <- print(paste0(parameter_name, "_9 : İl NO2 yıllık ortalaması" ))
output$NO2_9$data <- calculate_overall_average_by_city_threshold(parameter_name)

write_output_to_excel(output, result_no2_daily_excel_file)
