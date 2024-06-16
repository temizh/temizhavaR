library(dplyr)
library(temizhavaR)
library(dygraphs)

total_days <- 365
parameter_name <- "CO"

init.temizhavaR()

if (0) {
  station_name <- "Adana-Seyhan"
  parameters <- c("CO")
  daily_detail_data <- daily_detail_load_from_database(station_name)
  create_hourly_time_series_graph(daily_detail_data, station_name, parameters)
  calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
}

output <- list(CO_1 = list(),
               CO_2 = list(),
               CO_3 = list())

output$CO_1$result_message <- print(paste0(parameter_name, "_1 : Veri alınan istasyon listesi" ))
output$CO_1$data <- daily_list_stations_with_parameter(parameter_name)

output$CO_2$result_message <- print(paste0(parameter_name, "_2 : %90 veri alınan istasyon listesi" ))
output$CO_2$data <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

output$CO_3$result_message <- print(paste0(parameter_name, "_3 : %90 Veri alınan istasyon sayısı" ))
output$CO_3$data <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

write_output_to_excel(output, result_co_daily_excel_file)
