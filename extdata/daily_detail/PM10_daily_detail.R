library(dplyr)
library(temizhavaR)
library(writexl)

init.temizhavaR()

total_days <- 365
parameter_name <- "PM10"

init.temizhavaR()

if (0) {
  station_name <- "Karabük-Safranbolu"
  daily_detail_data <- daily_detail_load_from_database(station_name)
  all_daily_detail_data <- all_daily_detail_load_from_database()
  create_hourly_time_series_graph(daily_detail_data, station_name, c("PM10", "PM2.5"))
  calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
}

output <- list(PM10_1 = list(),
               PM10_2 = list(),
               PM10_3 = list(),
               PM10_4 = list(),
               PM10_5 = list(),
               PM10_6 = list(),
               PM10_7 = list(),
               PM10_8 = list(),
               PM10_9 = list(),
               PM10_10 = list(),
               PM10_12 = list(),
               PM10_13 = list(),
               PM10_14 = list())

output$PM10_1$result_message <- print(paste0(parameter_name, "_1 : Veri alınan istasyon listesi" ))
output$PM10_1$data <- daily_list_stations_with_parameter(parameter_name)

output$PM10_2$result_message <- print(paste0(parameter_name, "_2 : Veri alınan istasyon sayısı" ))
output$PM10_2$data <- daily_list_stations_with_parameter_count(parameter_name)

output$PM10_3$result_message <- print(paste0(parameter_name, "_3 : %90 veri alınan istasyon listesi" ))
output$PM10_3$data <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

output$PM10_4$result_message <- print(paste0(parameter_name, "_4 : %90 Veri alınan istasyon sayısı" ))
output$PM10_4$data <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

output$PM10_5$result_message <- print(paste0(parameter_name, "_5 : için istasyon ortalamaları"))
output$PM10_5$data <- daily_station_average(parameter_name, threshold = 90)

output$PM10_6$result_message <- print(paste0(parameter_name, "_6 : Yıllık ortalaması 40 µg/m3'ün üstündeki istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_6$data <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 40)

output$PM10_7$result_message <- print(paste0(parameter_name, "_7 : Yıllık ortalaması 40 µg/m3'ün altı istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_7$data <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 40)

output$PM10_8$result_message <- print(paste0(parameter_name, "_8 : Yıllık ortalaması 15 µg/m3'ün üstündeki istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_8$data <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 15)

output$PM10_9$result_message <- print(paste0(parameter_name, "_9 : Yıllık ortalaması 15 µg/m3'ün altı istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_9$data <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 15)

output$PM10_10$result_message <- print(paste0(parameter_name, "_10 : Günlük ortalaması 50 µg/m3'ün üstündeki istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_10$data <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 50)

###### 10 ile ayni. Hata?
# output$PM10_11$result_message <- print(paste0(parameter_name, "_11 : Istasyonlar 50 esigini kac gun boyunca astilar" ))
# output$PM10_11$data <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 50)

output$PM10_12$result_message <- print(paste0(parameter_name, "_12 : Yıllık ortalaması 45 µg/m3'ün altı istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_12$data <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 45)

output$PM10_13$result_message <- print(paste0(parameter_name, "_13 : Her bir istasyonun 45 esigini astigi gun sayisi" ))
output$PM10_13$data <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 45)

output$PM10_14$result_message <- print(paste0(parameter_name, "_14: İl PM10 yıllık ortalaması" ))
output$PM10_14$data <- calculate_overall_average_by_city_threshold(parameter_name)

write_output_to_excel(output, result_pm10_daily_excel_file)
