library(DBI)
library(dplyr)
library(openxlsx)

source("db_connect.R")  # Load database connection

# Function to get data from the database with nessesary filters
get_data <- function(frequency = "daily",
                     parameters = c("PM10", "PM2.5", "NO2", "SO2", "CO", "O3"),
                     start_date = "2013-01-01",
                     end_date = "2024-01-01",
                     region = NULL,
                     station = NULL,
                     station_type = NULL) {

  # Ensure database connection is valid
  check_db_connection()

  # Check if the frequency is valid
  if (frequency == "daily") {
    query <- "SELECT * FROM daily_detail"
  } else if (frequency == "hourly") {
    query <- "SELECT * FROM hourly_detail"
  } else {
    return("Invalid frequency.")
  }

  # Get the data
  data <- dbGetQuery(conn, query)

  # if region is provided, get stations in that region
  if (!is.null(region)) {
    stations <- dbGetQuery(conn, "SELECT * FROM location")
    stations <- stations %>% filter(Bolge == region)

    if (nrow(stations) == 0) {
      return("No stations found in the given region.")
    } else {
      station_ids <- stations$Id
      data <- data %>% filter(location_id %in% station_ids)
    }
  }

  # if station is provided, get data for that station
  if (!is.null(station)) {
    data <- data %>% filter(data$Istasyon_modified == station)
  }

  data <- data %>%
    mutate("Tarih&Saat" = openxlsx::convertToDateTime(Tarih)) # nolint

  # filter time range
  data <- data %>%
    # convert date format
    mutate(Tarih = as.Date(Tarih, format = "%Y-%m-%d", origin = "1899-12-30")) %>% # nolint
    # filter date range
    filter(Tarih >= start_date & Tarih <= end_date)

  # filter parameters(columns)
  data <- data %>% select("Tarih", "Tarih&Saat", "Istasyon_modified", all_of(parameters)) # nolint

  return(data)
}