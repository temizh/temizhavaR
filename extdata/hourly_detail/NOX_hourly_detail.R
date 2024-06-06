library(dplyr)
library(temizhavaR)
library(DBI)
library(dygraphs)

station_name <- "Erzincan"
parameters <- c("NOX")
total_days <- 8761
parameter = "NOX"
parameter_name <- "NOX"
hourly_detail_data <- hourly_detail_load_from_database(station_name)

create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)

calculate_parameter_mean(hourly_detail_data , parameter, threshold = 0.9, total_days, verbose = TRUE)

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
print(result_message)
nox_1 <- hourly_list_stations_with_parameter(parameter_name)
nox_1 <- rbind(result_message, nox_1)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
print(result_message)
nox_2 <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- nox_2 %>% arrange(desc(data_percentage))
nox_2 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
print(result_message)
nox_3 <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
nox_3 <- rbind(result_message, nox_3)



output <- list(NOx_1 = nox_1,
               NOx_2 = nox_2,
               NOx_3 = nox_3)

write_xlsx(

  output,
  path = result_nox_hourly_excel_file,
  col_names = FALSE,
  format_headers = TRUE,
  use_zip64 = FALSE
)
