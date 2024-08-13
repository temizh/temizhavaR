library(dplyr)
library(temizhavaR)
library(uuid)
library(dygraphs)

total_hours <- 8761
parameter_name <- "\"PM2.5\""


init.temizhavaR()


if (0) {
  station_name <- "Adana-Seyhan"
  parameters <- c("PM2.5")
  hourly_detail_data <- hourly_detail_load_from_database(station_name)
  all_hourly_detail_data <- all_hourly_detail_load_from_database()

  create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)
  calculate_parameter_mean(hourly_detail_data , parameter_name, threshold = 0.9, total_hours, verbose = TRUE)
}

output <- list(PM25_1 = list(),
               PM25_2 = list(),
               PM25_3 = list(),
               PM25_4 = list(),
               PM25_5 = list(),
               PM25_6 = list()
               )


output$PM25_1$result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
output$PM25_1$data <- hourly_list_stations_with_parameter(parameter_name)


output$PM25_2$result_message <- print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
output$PM25_2$data <- hourly_list_stations_with_parameter_count(parameter_name)


output$PM25_3$result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
output$PM25_3$data <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)


output$PM25_4$result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
output$PM25_4$data <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)


output$PM25_5$result_message <- print(paste(parameter_name,": %75 veri alınan istasyon listesi" ))
output$PM25_5$data <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 75)


output$PM25_6$result_message <- print(paste(parameter_name," : %75 Veri alınan istasyon sayısı" ))
output$PM25_6$data <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 75)


write_output_to_excel(output, result_pm25_hourly_excel_file)
