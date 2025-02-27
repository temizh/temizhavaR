library(dplyr)
library(temizhavaR)
library(dygraphs)

total_hours <- 8761
parameter_name <- "NOX"

init.temizhavaR()

if (0) {
  station_name <- "Erzincan"
  parameters <- c("NOX")
  hourly_detail_data <- hourly_detail_load_from_database(station_name)

  create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)
  calculate_parameter_mean(hourly_detail_data , parameter_name, threshold = 0.9, total_hours, verbose = TRUE)
}


output <- list(NOx_1 = list(),
               NOx_2 = list(),
               NOx_3 = list()
               )

output$NOx_1$result_message <- print(paste0(parameter_name, "_1 : Veri alınan istasyon listesi" ))
output$NOx_1$data <- hourly_list_stations_with_parameter(parameter_name)

output$NOx_2$result_message <- print(paste0(parameter_name, "_2 : %90 veri alınan istasyon listesi" ))
output$NOx_2$data <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

output$NOx_3$result_message <- print(paste0(parameter_name, "_3 : %90 Veri alınan istasyon sayısı" ))
output$NOx_3$data <- count_stations_with_parameter_threshold(parameter_name, data_type = "hourly", threshold = 90)

write_output_to_excel(output, result_nox_hourly_excel_file)
