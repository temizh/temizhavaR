
#SAVE FILES TO DATABASE

library(tidyverse)
library(readxl)
library(temizhavaR)

data_dir <- "C:/Hourly_detail/"

file_list <- list.files(data_dir, full.names = TRUE)


for (file_path in file_list) {

  istasyon <- read_excel(file_path)


  processed_data <- data_preprocessing(istasyon)


  hourly_detail_save_to_database(processed_data)


  cat("Processed:", file_path, "\n")
}


dbExecute(con, "DROP TABLE IF EXISTS hourly_detail")
library(DBI)
library(RSQLite)


con <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

unique_stations <- dbGetQuery(con, "SELECT DISTINCT Istasyon FROM hourly_detail")
print(unique_stations)



dbDisconnect(con)

