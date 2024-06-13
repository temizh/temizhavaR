library(dplyr)
library(temizhavaR)
library(dygraphs)

total_days <- 8761
parameter_name <- "CO"

if (0) {
  station_name <- "Erzincan"
  parameters <- c("CO")
  hourly_detail_data <- hourly_detail_load_from_database(station_name)

  create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)
  calculate_parameter_mean(hourly_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
}

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
co_1 <- hourly_list_stations_with_parameter(parameter_name)
co_1 <- rbind(result_message, co_1)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
co_2 <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
co_2 <- rbind(result_message, co_2)

result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
co_3 <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
co_3 <- rbind(result_message, co_3)

write_output_to_excel(output, result_co_hourly_excel_file)
