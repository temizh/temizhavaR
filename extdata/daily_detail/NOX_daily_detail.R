library(dplyr)
library(temizhavaR)
library(dygraphs)
library(writexl)

total_days <- 365
parameter_name <- "NOX"

if (0) {
  station_name <- "Adana-Seyhan"
  parameters <- c("NOX", "NO")
  daily_detail_data <- daily_detail_load_from_database(station_name)
  all_daily_detail_data <- all_daily_detail_load_from_database()

  create_hourly_time_series_graph(daily_detail_data, station_name, parameters)
  calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
}

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
nox_1 <- daily_list_stations_with_parameter(parameter_name)
nox_1 <- rbind(result_message, nox_1)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
nox_2 <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- nox_2 %>% arrange(desc(data_percentage))
nox_2 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
nox_3 <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
nox_3 <- rbind(result_message, nox_3)

result_message <- print(paste(parameter_name," : Yıllık ortalaması 30 µg/m3 üstündeki istasyonların listesi ve astigi gun sayisi" ))
nox_4 <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 30)
result_sorted <- nox_4$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
nox_4 <- rbind(result_message, result_sorted)

output <- list(NOx_1 = nox_1,
               NOx_2 = nox_2,
               NOx_3 = nox_3,
               NOx_4 = nox_4)

write_output_to_excel(output, result_nox_daily_excel_file)
