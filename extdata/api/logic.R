library(DBI)
library(dplyr)
library(dbplyr)
library(openxlsx)
library(temizhavaR)
library(jsonlite)

source("extdata/api/db_connect.R")  # Load database connection
source("extdata/api/websocket_connection.R") # Load WebSocket connection

# Function to get data from the database with nessesary filters
get_data <- function(frequency = "daily",
                     parameters = c("PM10", "PM25", "NO2", "SO2", "CO", "O3"),
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

  # get column names of the data
  columns <- colnames(data)
  print(columns)

  # if station type is provided, get data for that station type
  if (!is.null(station_type)) {
    data <- data %>% filter(data$station_type == station_type)
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

# Function to get stations from the database
get_stations <- function() {
  conn <- create_postgres_conn()

  # Get the stations
  stations <- tbl(conn, "location") %>%
    select(Bolge, Sehir, Istasyon_modified) %>%
    collect()

  result <- stations %>%
  group_by(Bolge) %>%
  group_split() %>%
  lapply(function(region_group) {
    region_name <- unique(region_group$Bolge)
    
    cities <- region_group %>%
      group_by(Sehir) %>%
      group_split() %>%
      lapply(function(city_group) {
        city_name <- unique(city_group$Sehir)
        
        list(
          label = city_name,
          value = paste0("_", city_name),
          children = lapply(city_group$Istasyon_modified, function(station) {
            list(
              label = station,
              value = station
            )
          })
        )
      })
    
    list(
      label = region_name,
      value = paste0("__", region_name),
      children = cities
    )
  })

  # Convert to JSON (pretty print)
  json_output <- toJSON(result, pretty = TRUE, auto_unbox = TRUE)

  # Disconnect from the database
  dbDisconnect(conn)

  return(json_output)
}

# Function to activate analysis calculations
create_analysis <- function(start_date, end_date, schema_name = NULL, folder_id = NULL, 
                            daily = TRUE, hourly = TRUE,
                            aqi_analysis = TRUE, save_to_drive = TRUE,
                            parameters = c("PM10", "PM25", "NO2", "NOX", "SO2", "CO", "O3"),
                            stations = c()) {
  message("Starting analysis...")
  send_notification("Analysis started")

  message(paste0("Shema name: ", schema_name))
  # Shema name
  if(is.null(schema_name) || schema_name == "") {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    schema_name <- paste0("analysis_", timestamp)
  }
  
  # Load functions (relative to the script's location)
  source("extdata/analysis/create_daily_intermediate_analysis.R")
  source("extdata/analysis/create_hourly_intermediate_analysis.R")
  source("extdata/analysis/create_daily_analysis_views.R")
  source("extdata/analysis/create_hourly_analysis_views.R")
  source("extdata/analysis/create_AQI_analysis.R")
  source("extdata/analysis/save_views_to_drive.R")

  con <- create_postgres_conn()

  # Ensure schema exists (create if not)
  dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS ", DBI::dbQuoteIdentifier(con, schema_name)))

  # Disconnect
  dbDisconnect(con)

  if(daily) {
    create_daily_intermediate_analysis(start_date, end_date, schema_name, parameters, stations)
    send_notification("Daily intermediate analysis created")
    create_daily_analysis_views(start_date, end_date, schema_name, parameters)
    send_notification("Daily analysis views created")
  }
  if(hourly) {
    create_hourly_intermediate_analysis(start_date, end_date, schema_name, parameters, stations)
    send_notification("Hourly intermediate analysis created")
    create_hourly_analysis_views(start_date, end_date, schema_name, parameters)
    send_notification("Hourly analysis views created")
  }

  # Calculate AQI
  if(aqi_analysis) {
    create_AQI_analysis(start_date, end_date, schema_name)
    send_notification("AQI analysis created")
  }

  # Save views to drive as XLSX
  if(save_to_drive && !is.null(folder_id)) {
    save_views_to_drive(schema_name, folder_id, parameters)
    send_notification("Views saved to Google Drive")
  }

  send_notification("Analysis completed")
  message("Analysis completed")

  return()
}