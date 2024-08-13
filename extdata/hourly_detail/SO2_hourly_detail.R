library(dplyr)
library(temizhavaR)
library(dygraphs)

total_hours <- 8761
parameter_name <- "SO2"

init.temizhavaR()

if (0) {
  station_name <- "Adana-Seyhan"
  parameters <- c("SO2")
  hourly_detail_data <- hourly_detail_load_from_database(station_name)
  all_hourly_detail_data <- all_hourly_detail_load_from_database()

  create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)
  calculate_parameter_mean(hourly_detail_data , parameter_name, threshold = 0.9, total_hours, verbose = TRUE)
}


output <- list(SO2_1 = list(),
               SO2_2 = list(),
               SO2_3 = list(),
               SO2_5 = list(),
               SO2_6 = list(),
               SO2_9 = list()
               )

output$SO2_1$result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
output$SO2_1$data <- hourly_list_stations_with_parameter(parameter_name)


output$SO2_2$result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
output$SO2_2$data <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)


output$SO2_3$result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
output$SO2_3$data <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)


output$SO2_5$result_message <- print(paste(parameter_name," : Saatlik ortalaması 350 esik degerini asan istasyonlar ve kac gün boyunca" ))
output$SO2_5$data <- hourly_above_exceedance_days_threshold(parameter_name, threshold = 350)


output$SO2_6$result_message <- print(paste(parameter_name,": Saatlik ortalaması 350 esik degerini 24 kere asan istasyonlar ve kaç gün boyunca" ))
output$SO2_6$data <- hourly_above_exceedance_days_double_threshold(parameter_name, threshold = 350, 24)


output$SO2_9$result_message <- print(paste(parameter_name,": Saatlik ortalaması 125 esik degerini 3 kere asan istasyonlar ve kaç gün boyunca" ))
output$SO2_9$data <- hourly_above_exceedance_days_double_threshold(parameter_name, threshold = 125, 3)


write_output_to_excel(output, result_so2_hourly_excel_file)

