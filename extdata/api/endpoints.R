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

get_intermediate_analysis_handler <- function(.req, .res) {

  data <- get_intermediate_analysis()

  .res$set_body(jsonlite::toJSON(data, auto_unbox = TRUE))
  .res$set_content_type("application/json")
}

get_final_analysis_handler <- function(.req, .res) {

  data <- get_final_analysis()

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
  analysis <- request_data$analysis
  save_to_drive <- request_data$save_to_drive
  stations <- request_data$stations

  # Check if the required parameters are provided
  if (is.null(start_date) || is.null(end_date)) {
    # Respond with a 400 Bad Request error
    raise(HTTPError$bad_request())
  }

  run_analysis(
    start_date = start_date,
    end_date = end_date,
    schema_name = schema_name,
    folder_id = folder_id,
    analysis = analysis,
    save_to_drive = save_to_drive,
    stations = stations
  )

  # Respond with a success message
  .res$set_body(jsonlite::toJSON(list(message = "Analysis run successfully."), auto_unbox = TRUE))
  .res$set_content_type("application/json")
}

create_intermediate_analysis_handler <- function(.req, .res) {

  print("helo")

  # Check if the request body is empty
  if (length(.req$body) == 0) {
    # Respond with a 400 Bad Request error
    raise(HTTPError$bad_request())
  }

  # Parse JSON body into an R object
  request_data <- .req$body

  name <- request_data$name
  analysis <- request_data$analysis
  data_type <- request_data$data_type
  pollutant <- request_data$pollutant
  parameters <- request_data$parameters

  # Check if the required parameters are provided
  if (is.null(name) || is.null(analysis) || is.null(data_type) || is.null(pollutant)) {
    # Respond with a 400 Bad Request error
    raise(HTTPError$bad_request())
  }

  if( is.null(parameters)) {
    parameters <- {}
  }

  create_intermediate_analysis(
    name = name,
    analysis = analysis,
    data_type = data_type,
    pollutant = pollutant,
    parameters = parameters
  )

  # Respond with a success message
  .res$set_body(jsonlite::toJSON(list(message = paste("Intermediate analysis", name, "created successfully.")), auto_unbox = TRUE))
  .res$set_content_type("application/json")
}

delete_intermediate_analysis_handler <- function(.req, .res) {

  # Check if the request body is empty
  if (length(.req$body) == 0) {
    # Respond with a 400 Bad Request error
    raise(HTTPError$bad_request())
  }

  # Parse JSON body into an R object
  request_data <- .req$body

  name <- request_data$name

  # Check if the required parameters are provided
  if (is.null(name)) {
    # Respond with a 400 Bad Request error
    raise(HTTPError$bad_request())
  }

  delete_intermediate_analysis(name)

  # Respond with a success message
  .res$set_body(jsonlite::toJSON(list(message = paste("Intermediate analysis", name, "deleted successfully.")), auto_unbox = TRUE))
  .res$set_content_type("application/json")
}

create_final_analysis_handler <- function(.req, .res) {

  # Check if the request body is empty
  if (length(.req$body) == 0) {
    # Respond with a 400 Bad Request error
    raise(HTTPError$bad_request())
  }

  # Parse JSON body into an R object
  request_data <- .req$body

  name <- request_data$name
  analysis <- request_data$analysis
  data <- request_data$data
  filters <- request_data$filters
  group_by <- request_data$group_by
  description <- request_data$description

  # Check if the required parameters are provided
  if (is.null(name) || is.null(analysis) || is.null(data) || is.null(group_by)) {
    # Respond with a 400 Bad Request error
    raise(HTTPError$bad_request())
  }

  if (is.null(filters)) {
    filters <- list()
  }

  create_final_analysis(
    name = name,
    analysis = analysis,
    data = data,
    filters = filters,
    group_by = group_by,
    description = description
  )

  # Respond with a success message
  .res$set_body(jsonlite::toJSON(list(message = paste("Final analysis", name, "created successfully.")), auto_unbox = TRUE))
  .res$set_content_type("application/json")
}

delete_final_analysis_handler <- function(.req, .res) {

  # Check if the request body is empty
  if (length(.req$body) == 0) {
    # Respond with a 400 Bad Request error
    raise(HTTPError$bad_request())
  }

  # Parse JSON body into an R object
  request_data <- .req$body

  name <- request_data$name

  # Check if the required parameters are provided
  if (is.null(name)) {
    # Respond with a 400 Bad Request error
    raise(HTTPError$bad_request())
  }

  delete_final_analysis(name)

  # Respond with a success message
  .res$set_body(jsonlite::toJSON(list(message = paste("Final analysis", name, "deleted successfully.")), auto_unbox = TRUE))
  .res$set_content_type("application/json")
}

get_analysis_configs_handler <- function(.req, .res) {

  data <- get_analysis_configs()

  .res$set_body(jsonlite::toJSON(data, auto_unbox = TRUE))
  .res$set_content_type("application/json")
}