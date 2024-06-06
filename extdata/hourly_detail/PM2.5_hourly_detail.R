library(tidyverse)
library(readxl)
library(dplyr)
library(temizhavaR)
library(DBI)
library(readxl)
library(uuid)
library(dygraphs)
# istasyonu sqlden alamadıgında boslukları birlestir
station_name <- "Adana-Seyhan"
parameters <- c("PM2.5")
total_days <- 8761
parameter = "PM2.5"
parameter_name <- "\"PM2.5\""
hourly_detail_data <- hourly_detail_load_from_database(station_name)
all_hourly_detail_data <- all_hourly_detail_load_from_database()

create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)

calculate_parameter_mean(hourly_detail_data , parameter, threshold = 0.9, total_days, verbose = TRUE)

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
print(result_message)
pm25_1 <- hourly_list_stations_with_parameter(parameter_name)
pm25_1 <- rbind(result_message, pm25_1)

result_message <- print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
print(result_message)
pm25_2 <- hourly_list_stations_with_parameter_count(parameter_name)
pm25_2 <- rbind(result_message, pm25_2)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
print(result_message)
pm25_3 <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- pm25_3 %>% arrange(desc(data_percentage))
pm25_3 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
print(result_message)
pm25_4 <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
pm25_4 <- rbind(result_message, pm25_4)

result_message <- print(paste(parameter_name,": %75 veri alınan istasyon listesi" ))
print(result_message)
pm25_5 <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 75)
result_sorted <- pm25_5 %>% arrange(desc(data_percentage))
pm25_5 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : %75 Veri alınan istasyon sayısı" ))
print(result_message)
pm25_6 <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 75)
pm25_6 <- rbind(result_message, pm25_6)


output <- list(PM25_1 = pm25_1,
               PM25_2 = pm25_2,
               PM25_3 = pm25_3,
               PM25_4 = pm25_4,
               PM25_5 = pm25_5,
               PM25_6 = pm25_6)
write_xlsx(
  output,
  path = result_pm25_hourly_excel_file,
  col_names = FALSE,
  format_headers = TRUE,
  use_zip64 = FALSE
)



