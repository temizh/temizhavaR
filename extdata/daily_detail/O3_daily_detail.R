library(dplyr)
library(temizhavaR)
library(dygraphs)

total_days <- 365
parameter_name <- "O3"

init.temizhavaR()

if (0) {
  station_name <- "Adana-Seyhan"
  parameters <- c("O3")
  daily_detail_data <- daily_detail_load_from_database(station_name)
  all_daily_detail_data <- all_daily_detail_load_from_database()

  create_hourly_time_series_graph(daily_detail_data, station_name, parameters)
  calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
}

output <- list(O3_1 = list(),
               O3_2 = list(),
               O3_3 = list(),
               O3_4 = list())

output$O3_1$result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
output$O3_1$data <- daily_list_stations_with_parameter(parameter_name)

output$O3_2$result_message <- print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
output$O3_2$data <- daily_list_stations_with_parameter_count(parameter_name)

output$O3_3$result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
output$O3_3$data <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

output$O3_4$result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
output$O3_4$data <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

# output$O3_5$result_message <- print(paste(parameter_name,": Yaz boyunca %90 veri alınan istasyon listesi" ))
# output$O3_5$data <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
#
# output$O3_6$result_message <- print(paste(parameter_name," : Yaz boyunca %90 Veri alınan istasyon sayısı" ))
# output$O3_6$data <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

write_output_to_excel(output, result_o3_daily_excel_file)
