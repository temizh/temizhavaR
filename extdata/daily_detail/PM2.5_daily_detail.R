library(dplyr)
library(temizhavaR)
library(dygraphs)
library(writexl)

total_days <- 365
parameter_name <- "\"PM2.5\""

init.temizhavaR()

if (0) {
  station_name <- "Mersin - Akdeniz"
  parameters <- c("PM2.5")
  daily_detail_data <- daily_detail_load_from_database(station_name)
  calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
  all_daily_detail_data <- all_daily_detail_load_from_database()
  create_hourly_time_series_graph(daily_detail_data, station_name, parameters)
}

output <- list(PM25_1 = list(),
               PM25_2 = list(),
               PM25_3 = list(),
               PM25_4 = list(),
               PM25_5 = list(),
               PM25_6 = list(),
               PM25_7 = list(),
               PM25_8 = list(),
               PM25_9 = list(),
               PM25_10 = list(),
               PM25_12 = list(),
               PM25_13 = list(),
               PM25_14 = list())

output$PM25_1$result_message <- print(paste0(parameter_name, "_1 : Veri alınan istasyon listesi" ))
output$PM25_1$data <- daily_list_stations_with_parameter(parameter_name)

output$PM25_2$result_message <- print(paste0(parameter_name, "_2 : Veri alınan istasyon sayısı" ))
output$PM25_2$data <- daily_list_stations_with_parameter_count(parameter_name)

output$PM25_3$result_message <- print(paste0(parameter_name, "_3 : %90 veri alınan istasyon listesi" ))
output$PM25_3$data <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

output$PM25_4$result_message <- print(paste0(parameter_name, "_4 : %90 Veri alınan istasyon sayısı" ))
output$PM25_4$data <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

output$PM25_5$result_message <- print(paste0(parameter_name, "_5 : %75 veri alınan istasyon listesi" ))
output$PM25_5$data <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 75)

output$PM25_6$result_message <- print(paste0(parameter_name, "_6 : %75 Veri alınan istasyon sayısı" ))
output$PM25_6$data <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 75)

output$PM25_7$result_message <- print(paste0(parameter_name, "_7 : için istasyon ortalamaları"))
output$PM25_7$data <- daily_station_average(parameter_name, threshold = 75)

output$PM25_8$result_message <- print(paste0(parameter_name, "_8 : Yıllık ortalaması 5 µg/m3 üstü istasyonların listesi ve aştıkları gun sayisi" ))
output$PM25_8$data <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 5)

output$PM25_9$result_message <- print(paste0(parameter_name, "_9 : Yıllık ortalaması 5 µg/m3'ün altı istasyonların listesive aştıkları gun sayisi" ))
output$PM25_9$data <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 5)

output$PM25_10$result_message <- print(paste0(parameter_name, "_10 : Günlük ortalaması 15 µg/m3'ün üstündeki istasyonların listesi ve aştıkları gun sayisi" ))
output$PM25_10$data <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 15)

#pm25_11 EKSIK

output$PM25_12$result_message <- print(paste0(parameter_name, "_12 : Günlük ortalaması 15 µg/m3'ün altındaki istasyonların listesi ve aştıkları gun sayisi" ))
output$PM25_12$data <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 15)

output$PM25_13$result_message <- print(paste0(parameter_name, "_13 : PM10 yıllık ortalamalarından 0,6667 faktörü ile çarpılarak elde edilecek hesaplanan PM25" ))
output$PM25_13$data <- new_pm25_from_pm10_yearly_average()

output$PM25_14$result_message <- print(paste0(parameter_name, "_14 : İl PM25 yıllık ortalaması" ))
output$PM25_14$data <- calculate_overall_average_by_city(parameter_name)

write_output_to_excel(output, result_pm25_daily_excel_file)
