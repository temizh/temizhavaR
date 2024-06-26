library(dplyr)
library(temizhavaR)
library(DBI)
library(dygraphs)
library(writexl)

#source(init.TemizHavaR_2023.R)
# if there is no result_dir give error that edit couldnt found and run again

station_name <- "Karabük - Safranbolu"
station_names <- c("Mersin - Tarsus", "Adana-Seyhan")
parameters <- c("PM10")
total_days <- 365
parameter = "PM10"
parameter_name <- "PM10"
city_name <- "ADANA"
daily_detail_data <- daily_detail_load_from_database(station_name)
all_daily_detail_data <- all_daily_detail_load_from_database()
create_hourly_time_series_graph(daily_detail_data, station_name, parameters)

calculate_parameter_mean(daily_detail_data , parameter_name, threshold = 0.9, total_days, verbose = TRUE)

result_message <- print(paste(parameter_name,": Veri alınan istasyon listesi" ))
print(result_message)
pm10_1 <- daily_list_stations_with_parameter(parameter_name)
pm10_1 <- rbind(result_message, pm10_1)

result_message <- print(paste(parameter_name,": Veri alınan istasyon sayısı" ))
print(result_message)
pm10_2 <- daily_list_stations_with_parameter_count(parameter_name)
pm10_2 <- rbind(result_message, pm10_2)

pm10_5 <- daily_station_average(parameter_name, threshold = 90)
result_sorted <- pm10_5 %>%
  arrange(desc(average))
result_message <- print(paste(parameter_name, ": için istasyon ortalamaları"))
print(result_message)
pm10_5 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name,": %90 veri alınan istasyon listesi" ))
print(result_message)
pm10_3 <- daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
result_sorted <- pm10_3 %>% arrange(desc(data_percentage))
pm10_3 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : %90 Veri alınan istasyon sayısı" ))
print(result_message)
pm10_4 <- daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
pm10_4 <- rbind(result_message, pm10_4)



result_message <- print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün üstündeki istasyonların listesi ve kaç gün boyunca " ))
print(result_message)
pm10_6 <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 40)
result_sorted <- pm10_6$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm10_6 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün altı istasyonların listesi ve kaç gün boyunca " ))
print(result_message)
pm10_7 <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 40)
result_sorted <- pm10_7$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm10_7 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : Yıllık ortalaması 15 µg/m3'ün üstündeki istasyonların listesi ve kaç gün boyunca " ))
print(result_message)
pm10_8 <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 15)
result_sorted <- pm10_8$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm10_8 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name," : Yıllık ortalaması 15 µg/m3'ün altı istasyonların listesi ve kaç gün boyunca " ))
print(result_message)
pm10_9 <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 15)
result_sorted <- pm10_9$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm10_9 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : Günlük ortalaması 50 µg/m3'ün üstündeki istasyonların listesi ve kaç gün boyunca " ))
print(result_message)
pm10_10 <- calculate_above_exceedance_days_all_stations(parameter_name, threshold = 50)
result_sorted <- pm10_10$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm10_10 <- rbind(result_message, result_sorted)

result_message <- print(paste(parameter_name," : Yıllık ortalaması 45 µg/m3'ün altı istasyonların listesi ve kaç gün boyunca " ))
print(result_message)
pm10_12 <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 45)
result_sorted <- pm10_12$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm10_12 <- rbind(result_message, result_sorted)



result_message <- print(paste(parameter_name," : Her bir istasyonun 45 esigini astigi gun sayisi" ))
print(result_message)
pm10_13 <- calculate_above_exceedance_days_all_stations(parameter, threshold = 45)
result_sorted <- pm10_13$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
pm10_13 <- rbind(result_message, result_sorted)



# pm10_7 <- list_stations_below_data_threshold(parameter_name, threshold = 40)
# result_sorted <- pm10_7 %>%
#   arrange(desc(yearly_average))
# result_message <- print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün altindaki istasyonların listesi " ))
# print(result_message)
# pm10_7 <- rbind(result_message, result_sorted)
#
# pm10_7 <- calculate_below_exceedance_days_all_stations(parameter_name, threshold = 40)
# result_sorted <- pm10_7 %>%
#   arrange(desc(yearly_average))
# result_message <- print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün altindaki istasyonların listesi " ))
# print(result_message)
# pm10_7 <- rbind(result_message, result_sorted)



# pm10_8 <- list_stations_above_data_threshold(parameter_name, threshold = 15)
# result_sorted <- pm10_8 %>%
#   arrange(desc(yearly_average))
# result_message <- print(paste(parameter_name," : Yıllık ortalaması 15 µg/m3'ün üstündeki istasyonların listesi " ))
# print(result_message)
# pm10_8 <- rbind(result_message, result_sorted)

#
# pm10_9 <- list_stations_below_data_threshold(parameter_name, threshold = 15)
# result_sorted <- pm10_9 %>%
#   arrange(desc(yearly_average))
# result_message <- print(paste(parameter_name," : Yıllık ortalaması 40 µg/m3'ün altindaki istasyonların listesi " ))
# print(result_message)
# pm10_9 <- rbind(result_message, result_sorted)


#
# pm10_10 <- list_stations_above_data_threshold(parameter_name, threshold = 50)
# result_sorted <- pm10_10 %>%
#   arrange(desc(yearly_average))
# result_message <- print(paste(parameter_name," : Yıllık ortalaması 50 µg/m3'ün üstündeki istasyonların listesi " ))
# print(result_message)
# pm10_10 <- rbind(result_message, result_sorted)


#
# result_message <- print(paste(parameter_name," : Her bir istasyonun 45 esigini astigi gun sayisi" ))
# print(result_message)
# pm10_13 <- calculate_above_exceedance_days_all_stations(parameter, threshold = 45)
# result_sorted <- pm10_13$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
# pm10_13 <- rbind(result_message, result_sorted)

#
# result_message <- print(paste(parameter_name," : ortalaması 50 esiginin üstüne cikan istasyonlar" ))
# print(result_message)
# pm10_10 <- calculate_exceedance_days_daily(all_daily_detail_data, parameter, threshold = 50)
# pm10_10 <- rbind(result_message, pm10_10)


#
# result_message <- print(paste(parameter_name," : Istasyonlar 50 esigini kac gun boyunca astilar" ))
# print(result_message)
# pm10_11 <- calculate_above_exceedance_days_all_stations(parameter, threshold = 50)
# result_sorted <- pm10_11$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
# pm10_11 <- rbind(result_message, result_sorted)



# result_message <- print(paste(parameter_name," : Istasyonlar 45 esiginin kac gun boyunca altında kaldılar" ))
# print(result_message)
# pm10_12 <- calculate_below_exceedance_days_all_stations(parameter, threshold = 45)
# result_sorted <- pm10_12$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
# pm10_12 <- rbind(result_message, result_sorted)

#
# pm10_12 <- list_stations_below_data_threshold(parameter_name, threshold = 15)
# result_sorted <- pm10_12 %>%
#   arrange(desc(yearly_average))
# result_message <- print(paste(parameter_name," : Gunluk ortalaması 45 µg/m3'ün alti istasyonların listesi " ))
# print(result_message)
# pm10_12 <- rbind(result_message, result_sorted)

#
# exceedance_days <- calculate_exceedance_days_daily(all_daily_detail_data, parameter, threshold = 45)
# print(paste("45 esiginin uzerinde asilma", exceedance_days, "gun boyunca gerceklesti."))

#pm_13
# result_message <- print(paste(parameter_name," : Her bir istasyonun 45 esigini astigi gun sayisi" ))
# print(result_message)
# pm10_13 <- calculate_above_exceedance_days_all_stations(parameter, threshold = 45)
# result_sorted <- pm10_13$ExceedanceDays %>% arrange(desc(ExceedsThreshold))
# pm10_13 <- rbind(result_message, result_sorted)


result_message <- print(paste(parameter_name,": İl PM10 yıllık ortalaması" ))
print(result_message)
pm10_14 <- calculate_overall_average_by_city_threshold(parameter)
result_sorted <- pm10_14 %>% arrange(desc(Ortalama))
pm10_14 <- rbind(result_message, result_sorted)


output <- list(PM10_1 = pm10_1,
               PM10_2 = pm10_2,
               PM10_3 = pm10_3,
               PM10_4 = pm10_4,
               PM10_5 = pm10_5,
               PM10_6 = pm10_6,
               PM10_7 = pm10_7,
               PM10_8 = pm10_8,
               PM10_9 = pm10_9,
               PM10_10 = pm10_10,
               PM10_12 = pm10_12,
               PM10_13 = pm10_13,
               PM10_14 = pm10_14 )
write_xlsx(
  output,
  path = result_pm10_daily_excel_file,
  col_names = FALSE,
  format_headers = TRUE,
  use_zip64 = FALSE
)
