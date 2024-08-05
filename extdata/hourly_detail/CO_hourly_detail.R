library(dplyr)
library(temizhavaR)
library(dygraphs)

total_hours <- 8761
parameter_name <- "CO"

init.temizhavaR()

if (0) {
  station_name <- "Erzincan"
  parameters <- c("CO")
  hourly_detail_data <- hourly_detail_load_from_database(station_name)

  create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)
  calculate_parameter_mean(hourly_detail_data , parameter_name, threshold = 0.9, total_hours, verbose = TRUE)
}


output <- list(CO_1 = list(),
               CO_2 = list(),
               CO_3 = list(),
               CO_4 = list()
               )


output$CO_1$result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
output$CO_1$data <- hourly_list_stations_with_parameter(parameter_name)

output$CO_2$result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
output$CO_2$data <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)

output$CO_3$result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
output$CO_3$data <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)

output$CO_4$result_message <- print(paste(parameter_name,"Maksimum gunluk 8 saatlik ortalaması 10 mg/m3'u asan istasyonlar"))
output$CO_4$data <- calculate_co_exceeding_stations()


write_output_to_excel(output, result_co_hourly_excel_file)

