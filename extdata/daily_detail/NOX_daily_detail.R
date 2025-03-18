library(dplyr)
library(temizhavaR)
library(dygraphs)
library(writexl)
library(lubridate)

total_days <- 365
parameter_name <- "NOX"

base_dir <- getOption("temizhavaR.base_dir")
output_dir <- file.path(base_dir, "__results")  

result_nox_daily_excel_file <- file.path(output_dir, "results_NOX_daily.xlsx")

init.temizhavaR()

if (0) {
  station_name <- "Adana-Seyhan"
  parameters <- c("NOX", "NO")
  daily_detail_data <- daily_detail_load_from_database(station_name)
  all_daily_detail_data <- all_daily_detail_load_from_database()

  create_hourly_time_series_graph(daily_detail_data, station_name, parameters)
  calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)
}

output <- list(NOx_1 = list(),
               NOx_2 = list(),
               NOx_3 = list(),
               NOx_4 = list())

output$NOx_1$result_message <- print(paste0(parameter_name, "_1 : Veri alınan istasyon listesi" ))
output$NOx_1$data <- list_stations_with_parameter(parameter_name, data_type = "daily", until_year = 2024)

output$NOx_2$result_message <- print(paste0(parameter_name, "_2 : %90 veri alınan istasyon listesi" ))
output$NOx_2$data <- list_stations_with_parameter(parameter_name, data_type = "daily", threshold = 90, until_year = 2024)

output$NOx_3$result_message <- print(paste0(parameter_name, "_3 : %90 Veri alınan istasyon sayısı" ))
output$NOx_3$data <- count_stations_with_parameter_threshold(parameter_name, "daily", threshold = 90, until_year = 2024)

output$NOx_4$result_message <- print(paste0(parameter_name, "_4 : Yıllık ortalaması 30 µg/m3 üstündeki istasyonların listesi ve aştıkları gun sayısı" ))
output$NOx_4$data <- calculate_above_exceedance_all_stations(parameter_name, "daily", pollutant_threshold = 30, until_year = 2024)

write_output_to_excel(output, result_nox_daily_excel_file)
