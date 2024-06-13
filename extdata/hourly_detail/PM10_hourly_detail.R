library(dplyr)
library(temizhavaR)
library(readxl)
library(dygraphs)

total_days <- 8761
parameter_name <- "PM10"

if (0) {
  station_name <- "Adana-Seyhan"
  parameters <- c("PM10")
  hourly_detail_data <- hourly_detail_load_from_database(station_name)
  all_hourly_detail_data <- all_hourly_detail_load_from_database()

  create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)
  calculate_parameter_mean(hourly_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
}
# result_message <- paste("PM10_5 :", parameter_name,": Her bir istasyonun yıllık PM10 ortalaması " )
# print(result_message)
# pm10_5 <- calculate_all_stations_means(all_hourly_detail_data , parameter, threshold = 0.9, total_days, verbose = FALSE)
# pm10_5 <- rbind(result_message, pm10_5)

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
pm10_1 <- hourly_list_stations_with_parameter(parameter_name)
pm10_1 <- rbind(result_message, pm10_1)

result_message <- print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
pm10_2 <- hourly_list_stations_with_parameter_count(parameter_name)
pm10_2 <- rbind(result_message, pm10_2)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
pm10_3 <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- pm10_3 %>% arrange(desc(data_percentage))
pm10_3 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
pm10_4 <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
pm10_4 <- rbind(result_message, pm10_4)

result_message <- print(paste(parameter_name,": Her bir istasyonun yıllık PM10 ortalaması " ))
pm10_5 <- hourly_station_average(parameter_name, threshold = 90)
pm10_5 <- rbind(result_message, pm10_5)

output <- list(PM10_1 = pm10_1,
               PM10_2 = pm10_2,
               PM10_3 = pm10_3,
               PM10_4 = pm10_4,
               PM10_5 = pm10_5)

write_output_to_excel(output, result_pm10_hourly_excel_file)

