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

  # Structure the data into a nested list
  # Group by region and city
  # to match Appsmith's structure
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
          value = paste0(region_name, "_", city_name),
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
      value = paste0("_", region_name),
      children = cities
    )
  })

  # Convert to JSON (pretty print)
  json_output <- toJSON(result, pretty = TRUE, auto_unbox = TRUE)

  # Disconnect from the database
  dbDisconnect(conn)

  return(json_output)
}

get_intermediate_analysis <- function() {

  conn <- create_postgres_conn()

  # Ensure database connection is valid
  check_db_connection()

  # Check if the table exists
  if (!dbExistsTable(conn, "intermediate_analysis")) {
    return("Table intermediate_analysis does not exist.")
  }

  # Get the intermediate analysis data
  analysis <- dbGetQuery(conn, "SELECT * FROM intermediate_analysis")

  # Convert the parameters column from JSON to character
  analysis$parameters <- as.character(analysis$parameters)

  # Convert to JSON format
  json_output <- toJSON(analysis, pretty = TRUE, auto_unbox = TRUE)

  # Disconnect from the database
  dbDisconnect(conn)

  return(json_output)
}

get_final_analysis <- function() {
  
  conn <- create_postgres_conn()

  # Ensure database connection is valid
  check_db_connection()

  # Check if the table exists
  if (!dbExistsTable(conn, "final_analysis")) {
    return("Table final_analysis does not exist.")
  }

  # Get the final analysis data
  analysis <- dbGetQuery(conn, "SELECT * FROM final_analysis")

  # Convert the filters column from JSON to character
  analysis$filters <- as.character(analysis$filters)

  # Convert to JSON format
  json_output <- toJSON(analysis, pretty = TRUE, auto_unbox = TRUE)

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

run_analysis <- function(start_date, end_date, schema_name = NULL, folder_id = NULL, save_to_drive = FALSE, analysis = c(), stations = c()) {

  # Shema name
  if(is.null(schema_name) || schema_name == "") {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    schema_name <- paste0("analysis_", timestamp)
  }

  print("A")
  # Save configuration to the database
  conn <- create_postgres_conn()
  # create table if doesn't exist
  if (!dbExistsTable(conn, "analysis_config")) {
    dbExecute(conn, "
      CREATE TABLE analysis_config (
        id SERIAL PRIMARY KEY,
        timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        start_date DATE NOT NULL,
        end_date DATE NOT NULL,
        schema_name TEXT NOT NULL,
        folder_id TEXT,
        save_to_drive BOOLEAN DEFAULT FALSE,
        analysis JSONB,
        stations JSONB
      )")
  }
  print("B")

  # Insert the analysis configuration into the table
  dbExecute(conn, "
    INSERT INTO analysis_config (start_date, end_date, schema_name, folder_id, save_to_drive, analysis, stations) 
    VALUES ($1, $2, $3, $4, $5, $6, $7)", 
    params = list(start_date, end_date, schema_name, folder_id, save_to_drive, jsonlite::toJSON(analysis, auto_unbox = TRUE), jsonlite::toJSON(stations, auto_unbox = TRUE))
  )

  # Disconnect from the database
  dbDisconnect(conn)

  # Run the analysis
  ###

  send_notification("Analysis completed")

  return()
}

create_intermediate_analysis <- function(name, analysis, data_type, pollutant, parameters) {
  # Ensure database connection is valid
  check_db_connection()

  # Check if the table exists
  if (!dbExistsTable(conn, "intermediate_analysis")) {
    return("Table intermediate_analysis does not exist.")
  }

  # Insert the new analysis into the table
  dbExecute(conn, "
    INSERT INTO intermediate_analysis 
    (name, analysis, data_type, pollutant, parameters, is_default) 
    VALUES ($1, $2, $3, $4, $5, FALSE)", 
    params = list(name, analysis, data_type, pollutant, jsonlite::toJSON(parameters, auto_unbox = TRUE))
  )

  message(paste("Intermediate analysis", name, "created successfully."))
}

delete_intermediate_analysis <- function(name) {
  # Ensure database connection is valid
  check_db_connection()

  # Check if the table exists
  if (!dbExistsTable(conn, "intermediate_analysis")) {
    return("Table intermediate_analysis does not exist.")
  }

  # Delete the analysis from the table
  dbExecute(conn, "
    DELETE FROM intermediate_analysis 
    WHERE name = $1 AND is_default = FALSE", 
    params = list(name)
  )

  message(paste("Intermediate analysis", name, "deleted successfully."))
}

create_final_analysis <- function(name, analysis, data, filters, group_by, description) {
  # Ensure database connection is valid
  check_db_connection()

  # Check if the table exists
  if (!dbExistsTable(conn, "final_analysis")) {
    return("Table final_analysis does not exist.")
  }

  # Insert the new analysis into the table
  dbExecute(conn, "
    INSERT INTO final_analysis 
    (name, analysis, data, filters, group_by, description, is_default) 
    VALUES ($1, $2, $3, $4, $5, $6, FALSE)", 
    params = list(name, analysis, data, jsonlite::toJSON(filters, auto_unbox = TRUE), group_by, description)
  )

  message(paste("Final analysis", name, "created successfully."))
}

delete_final_analysis <- function(name) {
  # Ensure database connection is valid
  check_db_connection()

  # Check if the table exists
  if (!dbExistsTable(conn, "final_analysis")) {
    return("Table final_analysis does not exist.")
  }

  # Delete the analysis from the table
  dbExecute(conn, "
    DELETE FROM final_analysis 
    WHERE name = $1 AND is_default = FALSE", 
    params = list(name)
  )

  message(paste("Final analysis", name, "deleted successfully."))
}

get_analysis_configs <- function() {
  # Ensure database connection is valid
  check_db_connection()

  # Check if the table exists
  if (!dbExistsTable(conn, "analysis_config")) {
    return("Table analysis_config does not exist.")
  }

  # Get the analysis configurations
  configs <- dbGetQuery(conn, "SELECT * FROM analysis_config")

  # Convert the analysis and stations columns from JSON to character
  configs$analysis <- as.character(configs$analysis)
  configs$stations <- as.character(configs$stations)

  # Convert to JSON format
  json_output <- toJSON(configs, pretty = TRUE, auto_unbox = TRUE)

  return(json_output)
}