library(dplyr)
library(temizhavaR)
library(writexl)
library(lubridate)
library(DBI)
library(openxlsx)

total_days <- 365
parameter_name <- "PM10"

base_dir <- getOption("temizhavaR.base_dir")
output_dir <- file.path(base_dir, "__results")  

result_pm10_daily_excel_file <- file.path(output_dir, "results_PM10_daily.xlsx")

init.temizhavaR()

if (0) {
  station_name <- "Karabük-Safranbolu"
  station_name <- "İstanbul-Arnavutköy"

  daily_detail_data <- daily_detail_load_from_database(station_name)
  all_daily_detail_data <- all_daily_detail_load_from_database()
  create_hourly_time_series_graph(daily_detail_data, station_name, c('PM10'))
  calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)

  hourly_detail_data <- hourly_detail_load_from_database(station_name)
  create_hourly_time_series_graph(hourly_detail_data, station_name, c('PM10'))
}

output <- list(PM10_1 = list(),
               PM10_2 = list(),
               PM10_3.1 = list(),
               PM10_3.2 = list(),
               PM10_4 = list(),
               PM10_5 = list(),
               PM10_6 = list(),
               PM10_7 = list(),
               PM10_8 = list(),
               PM10_9 = list(),
               PM10_10 = list(),
               PM10_11 = list(),
               PM10_12 = list(),
               PM10_13 = list(),
               PM10_14 = list())

output$PM10_1$result_message <- print(paste0(parameter_name, "_1 : Veri alınan istasyon listesi" ))
output$PM10_1$data <- list_stations_with_parameter(parameter_name, "daily")

output$PM10_2$result_message <- print(paste0(parameter_name, "_2 : Veri alınan istasyon sayısı (yıllara göre)" ))
output$PM10_2$data <- list_stations_with_parameter_count(parameter_name, "daily")

output$PM10_3.1$result_message <- print(paste0(parameter_name, "_3.1 : %90 veri alınan istasyon listesi" ))
output$PM10_3.1$data <- list_stations_with_parameter(parameter_name, "daily", threshold = 90)

output$PM10_3.2$result_message <- print(paste0(parameter_name, "_3.2 : %75 veri alınan istasyon listesi" ))
output$PM10_3.2$data <- list_stations_with_parameter(parameter_name, "daily", threshold = 75)

output$PM10_4$result_message <- print(paste0(parameter_name, "_4 : %90 Veri alınan istasyon sayısı" ))
output$PM10_4$data <- count_stations_with_parameter_threshold(parameter_name, threshold = 90, data_type = "daily")

output$PM10_5$result_message <- print(paste0(parameter_name, "_5 : için istasyon ortalamaları"))
output$PM10_5$data <- station_average(parameter_name, "daily", threshold = 90)

output$PM10_6$result_message <- print(paste0(parameter_name, "_6 : Yıllık ortalaması 40 µg/m3'ün üstündeki istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_6$data <- calculate_above_exceedance_all_stations(parameter_name, "daily", pollutant_threshold = 40)

output$PM10_7$result_message <- print(paste0(parameter_name, "_7 : Yıllık ortalaması 40 µg/m3'ün altı istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_7$data <- calculate_below_exceedance_all_stations(parameter_name, "daily", pollutant_threshold = 40)

output$PM10_8$result_message <- print(paste0(parameter_name, "_8 : Yıllık ortalaması 15 µg/m3'ün üstündeki istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_8$data <- calculate_above_exceedance_all_stations(parameter_name, "daily", pollutant_threshold = 15)

output$PM10_9$result_message <- print(paste0(parameter_name, "_9 : Yıllık ortalaması 15 µg/m3'ün altı istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_9$data <- calculate_below_exceedance_all_stations(parameter_name, "daily", pollutant_threshold = 15)

output$PM10_10$result_message <- print(paste0(parameter_name, "_10 : Yıllık ortalaması 50 µg/m3'ün üstündeki istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_10$data <- calculate_above_exceedance_all_stations(parameter_name, "daily", pollutant_threshold = 50)

output$PM10_11$result_message <- print(paste0(parameter_name, "_11 : Yıllık ortalaması 45 µg/m3'ün üstündeki istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_11$data <- calculate_above_exceedance_all_stations(parameter_name, "daily", pollutant_threshold = 45)

output$PM10_12$result_message <- print(paste0(parameter_name, "_12 : Yıllık ortalaması 45 µg/m3'ün altı istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_12$data <- calculate_below_exceedance_all_stations(parameter_name, "daily", pollutant_threshold = 45)

output$PM10_13$result_message <- print(paste0(parameter_name, "_13 : Yıllık ortalaması 45 µg/m3'ün üstündeki istasyonların listesi ve aştıkları gun sayisi" ))
output$PM10_13$data <- calculate_above_exceedance_all_stations(parameter_name, "daily", pollutant_threshold = 45)

output$PM10_14$result_message <- print(paste0(parameter_name, "_14: İl PM10 yıllık ortalaması" ))
output$PM10_14$data <- calculate_overall_average_by_city_threshold(parameter_name, "daily")


write_output_to_excel(output, result_pm10_daily_excel_file)
