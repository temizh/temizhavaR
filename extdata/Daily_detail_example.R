library(tidyverse)
library(readxl)
library(dplyr)
library(temizhavaR)
library(DBI)
library(readxl)
library(uuid)
library(dygraphs)


station_name <- "Mersin - Tarsus"
parameters <- c("NOX", "PM10")
total_days <- 365
threshold <- 0.9
parameter <- "PM10"
parameter_name <- "PM10"
daily_detail_data <- daily_detail_load_from_database(station_name)
create_hourly_time_series_graph(daily_detail_data, station_name, parameters)
x <- calculate_parameter_mean(daily_detail_data, parameter, threshold, total_days, verbose = TRUE)
#average <- station_average(daily_detail_data, parameter)# output:dataframe(station,average)
daily_list_stations_with_parameter(parameter_name)
daily_list_stations_with_parameter_count(parameter_name)
average -> station_average(parameter_name, threshold = 90)
daily_list_stations_with_parameter_threshold(parameter_name, threshold = 90)
daily_count_stations_with_parameter_threshold(parameter_name, threshold = 90)
average <- station_average(parameter_name, threshold = 90)
list_stations_above_data_threshold(parameter_name, threshold = 40)
list_stations_below_data_threshold(parameter_name, threshold = 40)

