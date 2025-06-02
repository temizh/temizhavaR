library(RestRserve)

source("extdata/api/logic.R")  # Load logic functions

get_data_handler <- function(.req, .res) {

  if (!is.null(.req$parameters_query[["frequency"]])) {
    frequency <- .req$parameters_query[["frequency"]]
  } else {
    frequency <- "daily"
  }

  if (!is.null(.req$parameters_query[["parameters"]])) {
    parameters <- .req$parameters_query[["parameters"]]
  } else {
    parameters <- c("PM10", "PM25", "NO2", "SO2", "CO", "O3")
  }

  if (!is.null(.req$parameters_query[["start_date"]])) {
    start_date <- .req$parameters_query[["start_date"]]
  } else {
    start_date <- "2013-01-01"
  }

  if (!is.null(.req$parameters_query[["end_date"]])) {
    end_date <- .req$parameters_query[["end_date"]]
  } else {
    end_date <- "2024-1-1"
  }

  region <- .req$parameters_query[["region"]]
  station <- .req$parameters_query[["station"]]
  station_type <- .req$parameters_query[["station_type"]]

  data <- get_data(frequency, parameters, start_date, end_date, region, station, station_type) # nolint

  .res$set_body(jsonlite::toJSON(data, auto_unbox = TRUE))
  .res$set_content_type("application/json")
}

get_data_by_config_handler <- function(.req, .res) {

  # Check if the request body is empty
  if (length(.req$body) == 0) {
    # Respond with a 400 Bad Request error
    raise(HTTPError$bad_request())
  }

  # Parse JSON body into an R object
  request_data <- .req$body

  # Extract values from the parsed object
  if (!is.null(request_data$frequency)) {
    frequency <- request_data$frequency
  } else {
    frequency <- "daily"
  }

  if (!is.null(request_data$parameters)) {
    parameters <- request_data$parameters
  } else {
    parameters <- c("PM10", "PM25", "NO2", "SO2", "CO", "O3")
  }

  if (!is.null(request_data$start_date)) {
    start_date <- request_data$start_date
  } else {
    start_date <- "2013-01-01"
  }

  if (!is.null(request_data$end_date)) {
    end_date <- request_data$end_date
  } else {
    end_date <- "2024-1-1"
  }

  region <- request_data$region
  station <- request_data$station
  station_type <- request_data$station_type

  data <- get_data(frequency, parameters, start_date, end_date, region, station, station_type) # nolint

  .res$set_body(jsonlite::toJSON(data, auto_unbox = TRUE))
  .res$set_content_type("application/json")
}

get_stations_handler <- function(.req, .res) {

  data <- get_stations()

  .res$set_body(jsonlite::toJSON(data, auto_unbox = TRUE))
  .res$set_content_type("application/json")
}

create_analysis_handler <- function(.req, .res) {
  print("request is made!")
  # Check if the request body is empty
  if (length(.req$body) == 0) {
    # Respond with a 400 Bad Request error
    raise(HTTPError$bad_request())
  }
  print("request is not empty!")

  # Parse JSON body into an R object
  request_data <- .req$body

  print(request_data)

  # Extract values from the parsed object
  start_date <- request_data$start_date
  end_date <- request_data$end_date
  schema_name <- request_data$schema_name
  folder_id <- request_data$folder_id
  daily <- request_data$daily
  hourly <- request_data$hourly
  aqi_analysis <- request_data$aqi_analysis
  save_to_drive <- request_data$save_to_drive
  parameters <- request_data$parameters
  stations <- request_data$stations

  # Check if the required parameters are provided
  if (is.null(start_date) || is.null(end_date)) {
    # Respond with a 400 Bad Request error
    raise(HTTPError$bad_request())
  }

  create_analysis(
    start_date = start_date,
    end_date = end_date,
    schema_name = schema_name,
    folder_id = folder_id,
    daily = daily,
    hourly = hourly,
    aqi_analysis = aqi_analysis,
    save_to_drive = save_to_drive,
    parameters = parameters,
    stations = stations
  )

  # Respond with a success message
  .res$set_body(jsonlite::toJSON(list(message = "Analysis run successfully."), auto_unbox = TRUE))
  .res$set_content_type("application/json")
}