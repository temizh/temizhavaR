library(dplyr)
library(temizhavaR)
library(dygraphs)
library(lubridate)
total_hours <- 8761
parameter_name <- "O3"

init.temizhavaR()


if (0) {
  station_name <- "Erzincan"
  parameters <- c("O3")
  hourly_detail_data <- hourly_detail_load_from_database(station_name)
  all_hourly_detail_data <- all_hourly_detail_load_from_database()

  create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)
  calculate_parameter_mean(hourly_detail_data , parameter_name, threshold = 0.9, total_hours, verbose = TRUE)
}


output <- list(O3_1 = list(),
               O3_2 = list(),
               O3_3 = list(),
               O3_4 = list(),
               O3_9 = list()
               )


output$O3_1$result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
output$O3_1$data <- hourly_list_stations_with_parameter(parameter_name)


output$O3_2$result_message <- print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
output$O3_2$data <- hourly_list_stations_with_parameter_count(parameter_name)


output$O3_3$result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
output$O3_3$data <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)


output$O3_4$result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
output$O3_4$data <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)


output$O3_9$result_message <- print(paste(parameter_name," : AOT 40 icin 1 saatlik degerlerin %90 ve üstü veri alınan istasyon listesi"))
output$O3_9$data <- calculate_aot40_hourly_threshold(start_dates = c("2023-05-01", "2023-04-01"), end_dates = c("2023-07-31", "2023-09-30"),threshold = 90)


write_output_to_excel(output, result_o3_hourly_excel_file)
