library(dplyr)
library(temizhavaR)
library(dygraphs)
library(lubridate)
library(zoo)

total_hours <- 8761
parameter_name <- "O3"

base_dir <- getOption("temizhavaR.base_dir")
output_dir <- file.path(base_dir, "__results")  

result_o3_hourly_excel_file <- file.path(output_dir, "results_O3_hourly.xlsx")

init.temizhavaR()

if (0) {
  station_name <- "Erzincan"
  parameters <- c("O3")
  hourly_detail_data <- hourly_detail_load_from_database(station_name)
  all_hourly_detail_data <- all_hourly_detail_load_from_database()

  create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)
  calculate_parameter_mean(hourly_detail_data , parameter_name, threshold = 0.9, total_hours, verbose = TRUE)
}


output <- list(O3_1 = list(),
               O3_2 = list(),
               O3_3 = list(),
               O3_4 = list(),
               O3_9 = list(),
               O3_10 = list(),
               O3_11 = list(),
               O3_12 = list(),
               O3_13 = list(),
               O3_14 = list(),
               O3_15 = list(),
               O3_16 = list(),
               O3_17 = list(),
               O3_18 = list()
               )

# output$O3_1$result_message <- print(paste0(parameter_name, "_1 : Veri alınan istasyon listesi" ))
# output$O3_1$data <- list_stations_with_parameter(parameter_name, data_type = "hourly")

# output$O3_2$result_message <- print(paste0(parameter_name, "_2 : Veri alınan istasyon sayısı" ))
# output$O3_2$data <- list_stations_with_parameter_count(parameter_name, "hourly")

# output$O3_3$result_message <- print(paste0(parameter_name, "_3 : %90 veri alınan istasyon listesi" ))
# output$O3_3$data <- list_stations_with_parameter(parameter_name, data_type = "hourly", threshold = 90)

# output$O3_4$result_message <- print(paste0(parameter_name, "_4 : %90 Veri alınan istasyon sayısı" ))
# output$O3_4$data <- count_stations_with_parameter_threshold(parameter_name, data_type = "hourly", threshold = 90)

output$O3_9$result_message <- print(paste0(parameter_name, "_9 : AOT 40 icin 1 saatlik degerlerin %90 ve üstü veri alınan istasyon listesi"))
output$O3_9$data <- hourly_list_aot40_threshold(start_months = c(5, 4), end_months = c(7, 9),threshold = 90)

# output$O3_10$result_message <-  print(paste0(parameter_name, "_10 : AOT 40 icin 1 saatlik degerlerin %90 ve üstü veri alınan istasyon sayısı"))
# output$O3_10$data <- count_stations_aot40_threshold(start_dates = paste0(YEAR, c("-05-01", "-04-01")), end_dates = paste0(YEAR, c("-07-31", "-09-30")),threshold = 90)

# output$O3_11$result_message <-  print(paste0(parameter_name, "_11 : 8 saatlik ortalamaların günlük maksimum degerlerinden 120 µg/m3'ü asanların sayısı"))
# output$O3_11$data <- calculate_hourly_aot40_exceeding_stations(parameter_name,hour_window = 8, months = NULL, threshold = 120, aggregation_period = "daily")

# output$O3_12$result_message <- print(paste0(parameter_name, "_12 Mayıs ayından Temmuz ayına kadar AOT 40 degerlerinin toplamı "))
# output$O3_12$data <- calculate_aot40_vegetation_and_forest_protection(
#                           parameter_name = "O3",
#                           start_month = "05",
#                           end_month = "07",
#                           thresholds = c(6000, 18000),
#                           period = "May-Jul")

# output$O3_13$result_message <-  print(paste0(parameter_name, "_13 Nisan ayından Eylül ayına kadar AOT 40 degerlerinin toplamı "))
# output$O3_13$data <-  calculate_aot40_vegetation_and_forest_protection(
#                           parameter_name = "O3",
#                           start_month = "04",
#                           end_month = "09",
#                           thresholds = c(20000),
#                           period = "Apr-Sep")

# output$O3_14$result_message <- print(paste0(parameter_name, "_14 Nisan-Eylül aylarında 1 saatlik ortalamaların günlük maksimum degerlerinden 180 µg/m3'ü asanların sayısı"))
# output$O3_14$data <- calculate_hourly_aot40_exceeding_stations(parameter_name,hour_window = 1,months = c(4 ,5 ,6 ,7 ,8 ,9),threshold=180,aggregation_period = "daily")

# output$O3_15$result_message <- print(paste0(parameter_name, "_15 Nisan-Eylül aylarında 1 saatlik ortalamaların günlük maksimum degerlerinden 240 µg/m3'ü asanların sayısı"))
# output$O3_15$data <- calculate_hourly_aot40_exceeding_stations(parameter_name,hour_window = 1,months = c(4 ,5 ,6 ,7 ,8 ,9),threshold=240,aggregation_period = "daily")

# output$O3_16$result_message <- print(paste0(parameter_name, "_16 Nisan-Eylül aylarında 8 saatlik ortalamaların günlük maksimum degerlerinden 120 µg/m3'ü asanların sayısı"))
# output$O3_16$data <- calculate_hourly_aot40_exceeding_stations(parameter_name,hour_window = 8,months = c(4 ,5 ,6 ,7 ,8 ,9),threshold=120,aggregation_period = "daily")

# output$O3_17$result_message <- print(paste0(parameter_name, "_17 Nisan-Eylül aylarında 1 saatlik ortalamaların aylık maksimum degerlerinden 120 µg/m3'ü asanların sayısı"))
# output$O3_17$data <- calculate_hourly_aot40_exceeding_stations(parameter_name,hour_window = 1,months = c(4 ,5 ,6 ,7 ,8 ,9),threshold=120,aggregation_period = "monthly")

# output$O3_18$result_message <- print(paste0(parameter_name, "_18 %90 veri ve üzeri istasyonlar için Kayan 8 saatlik ortalamalar"))
# output$O3_18$data <- calculate_rolling_8hour_avg()

write_output_to_excel(output, result_o3_hourly_excel_file)
