library(dplyr)
library(temizhavaR)
library(dygraphs)

station_name <- "Erzincan"
parameters <- c("O3")
total_days <- 8761
parameter = "O3"
parameter_name <- "O3"
hourly_detail_data <- hourly_detail_load_from_database(station_name)

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
print(result_message)
o3_1 <- hourly_list_stations_with_parameter(parameter_name)
o3_1 <- rbind(result_message, o3_1)

result_message <- print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
print(result_message)
o3_2 <- hourly_list_stations_with_parameter_count(parameter_name)
o3_2 <- rbind(result_message, o3_2)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
print(result_message)
o3_3 <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- o3_3 %>% arrange(desc(data_percentage))
o3_3 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
print(result_message)
o3_4 <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
o3_4 <- rbind(result_message, o3_4)


output <- list(O3_1 = o3_1,
               O3_2 = o3_2,
               O3_3 = o3_3,
               O3_4 = o3_4)
write_xlsx(

  output,
  path = result_o3_hourly_excel_file,
  col_names = FALSE,
  format_headers = TRUE,
  use_zip64 = FALSE
)









create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)

calculate_parameter_mean(hourly_detail_data , parameter, threshold = 0.9, total_days, verbose = TRUE)

print(paste(parameter_name,": Veri alınan istasyon listesi" ))
hourly_list_stations_with_parameter(parameter_name)

print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
hourly_list_stations_with_parameter_count(parameter_name)

print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
