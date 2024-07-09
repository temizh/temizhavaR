library(readxl)
library(temizhavaR)
library(uuid)


#LIST STATIONS FOR THE SPECIFIED PARAMETER


data_dir <- "C:/Hourly_detail/"

excel_files <- list.files(path = data_dir, pattern = "\\.xlsx$", full.names = TRUE)

all_station_names <- c()

# Apply operations for each Excel file
for (file in excel_files) {

  istasyon <- read_excel(file)

  processed_data <- data_preprocessing(istasyon)


  #hourly_detail_save_to_database(processed_data)

  # SPECIFIED PARAMETER
  station_names <- list_stations(processed_data, "PM10")
  all_station_names <- c(all_station_names, station_names)
}
all_station_names <- unique(all_station_names)

for (station_name in all_station_names) {
  if (verbose) {
  print(paste("İstasyon:", station_name))
  }
}
