library(dplyr)
library(temizhavaR)
library(dygraphs)

total_hours <- 8761
parameter_name <- "NO2"

init.temizhavaR()

if(0) {
  station_name <- "Erzincan"
  parameters <- c("NO2")
  all_hourly_detail_data <- all_hourly_detail_load_from_database()

  create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)
  calculate_parameter_mean(hourly_detail_data , parameter_name, threshold = 0.9, total_hours, verbose = TRUE)
}


output <- list(NO2_1 = list(),
               NO2_2 = list(),
               NO2_3 = list(),
               NO2_4 = list(),
               NO2_10 = list()
               )

output$NO2_1$result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
output$NO2_1$data<- hourly_list_stations_with_parameter(parameter_name)


output$NO2_2$result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
output$NO2_2$data <- hourly_list_stations_with_parameter_threshold(parameter_name, threshold = 90)


output$NO2_3$result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
output$NO2_3$data <- hourly_count_stations_with_parameter_threshold(parameter_name, threshold = 90)


output$NO2_4$result_message <- print(paste(parameter_name,": Saatlik ortalaması 200 esik degerini 18 kere asan istasyonlar ve kaç gün boyunca aştıkları" ))
output$NO2_4$data <- hourly_above_exceedance_days_double_threshold(parameter_name, threshold = 200, 18)


output$NO2_10$result_message <- print(paste(parameter_name,": 3 ardisik saat " , threshold = 400 , "degerini asan istasyonlar ve kac kere astiklari" ))
output$NO2_10$data <- consecutive_hourly_list_stations(parameter_name, threshold = 400)


#data.frame tipinde olmayan data nesnesini data.frame nesnesine çevirir
for(i in seq_along(output)) {
  if(!is.data.frame(output[[i]]$data) && !is.matrix(output[[i]]$data)) {
    output[[i]]$data <- as.data.frame(output[[i]]$data)
  }
}


write_output_to_excel(output, result_no2_hourly_excel_file)
