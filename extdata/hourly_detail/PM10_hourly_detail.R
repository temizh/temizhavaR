library(dplyr)
library(temizhavaR)
library(readxl)
library(dygraphs)

total_hours <- 8761
parameter_name <- "PM10"

init.temizhavaR()


if (0) {
  station_name <- "Adana-Seyhan"
  parameters <- c("PM10")
  hourly_detail_data <- hourly_detail_load_from_database(station_name)
  all_hourly_detail_data <- all_hourly_detail_load_from_database()

  create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)
  calculate_parameter_mean(hourly_detail_data , parameter_name, threshold = 0.9, total_hours, verbose = TRUE)
}


output <- list(PM10_1 = list(),
               PM10_2 = list(),
               PM10_3 = list(),
               PM10_4 = list(),
               PM10_5 = list()
               )


# result_message <- paste("PM10_5 :", parameter_name,": Her bir istasyonun yıllık PM10 ortalaması " )
# print(result_message)
# pm10_5 <- calculate_all_stations_means(all_hourly_detail_data , parameter, threshold = 0.9, total_days, verbose = FALSE)
# pm10_5 <- rbind(result_message, pm10_5)

output$PM10_1$result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
output$PM10_1$data <- hourly_list_stations_with_parameter(parameter_name)


output$PM10_2$result_message <- print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
output$PM10_2$data <- hourly_list_stations_with_parameter_count(parameter_name)


output$PM10_3$result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
output$PM10_3$data <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)


output$PM10_4$result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
output$PM10_4$data <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)


output$PM10_5$result_message <- print(paste(parameter_name,": Her bir istasyonun yıllık PM10 ortalaması " ))
output$PM10_5$data <- hourly_station_average(parameter_name, threshold = 90)


write_output_to_excel(output, result_pm10_hourly_excel_file)

