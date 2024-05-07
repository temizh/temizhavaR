
##### Specified station based examples

library(tidyverse)
library(readxl)
library(dplyr)
library(temizhavaR)
library(DBI)
library(readxl)
library(uuid)
library(dygraphs)


station_name <- "Adana - Valilik"
total_days <- 8761
threshold <- 0.7
parameter <- "CO"
parameters <- c("SO2", "CO")
parameter_name <- "NOX"
hourly_detail_data <- hourly_detail_load_from_database(station_name)
create_hourly_time_series_graph(hourly_detail_data, station_name, parameters)
calculate_parameter_mean(hourly_detail_data, parameter, threshold, total_days)
daily_list_stations_with_parameter(parameter_name)
hourly_list_stations_with_parameter_count(parameter_name)
