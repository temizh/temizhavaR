library(dplyr)
library(temizhavaR)
library(uuid)
library(dygraphs)

total_hours <- 8761
parameter_name <- "PM25"


init.temizhavaR()


if (0) {
  station_name <- "Adana-Seyhan"
  parameters <- c("PM25")
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

output$PM25_1$result_message <- print(paste0(parameter_name, "_1 : Veri alınan istasyon listesi" ))
output$PM25_1$data <- list_stations_with_parameter(parameter_name, data_type = "hourly")

output$PM25_2$result_message <- print(paste0(parameter_name, "_2 : Veri alınan istasyon sayısı" ))
output$PM25_2$data <-list_stations_with_parameter_count(parameter_name, "hourly")

output$PM25_3$result_message <- print(paste0(parameter_name, "_3 : %90 veri alınan istasyon listesi" ))
output$PM25_3$data <- list_stations_with_parameter(parameter_name, data_type = "hourly", threshold = 90)

output$PM25_4$result_message <- print(paste0(parameter_name, "_4 : %90 Veri alınan istasyon sayısı" ))
output$PM25_4$data <- count_stations_with_parameter_threshold(parameter_name, data_type = "hourly", threshold = 90)

output$PM25_5$result_message <- print(paste0(parameter_name, "_5 : %75 veri alınan istasyon listesi" ))
output$PM25_5$data <- list_stations_with_parameter(parameter_name, data_type = "hourly", threshold = 75)

output$PM25_6$result_message <- print(paste0(parameter_name, "_6 : %75 Veri alınan istasyon sayısı" ))
output$PM25_6$data <- count_stations_with_parameter_threshold(parameter_name, data_type = "hourly", threshold = 75)

write_output_to_excel(output, result_pm25_hourly_excel_file)
