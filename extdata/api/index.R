library(RestRserve)
library(DBI)
library(dotenv)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load environment variables from the .env file
dotenv::load_dot_env()

# Read the environment variables
db_host <- Sys.getenv("POSTGRES_HOST")
db_name <- Sys.getenv("TEMIZHAVA_DB")
db_user <- Sys.getenv("POSTGRES_TUSER")
db_password <- Sys.getenv("POSTGRES_TUSER_PASSWORD")
db_port <- Sys.getenv("POSTGRES_PORT")

app <- Application$new()

# Connect to the POSTGIS database
conn <- dbConnect(RPostgres::Postgres(),
  dbname = db_name,
  host = db_host,
  port = db_port,
  user = db_user,
  password = db_password
)

# Functions
get_data <- function(frequency = "daily",
                     parameters = c("PM10", "PM2.5", "NO2", "SO2", "CO", "O3"),
                     start_date = "2014-01-01",
                     end_date = "2024-01-01",
                     region = NULL,
                     station = NULL,
                     station_type = NULL) {
  # Check if the database connection is established
  if (is.null(conn)) {
    return("Database connection is not established.")
  }

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

  # filter time range
  data <- data %>%
    # convert date format
    mutate(Tarih = as.Date(Tarih, format = "%Y-%m-%d", origin = "1899-12-30")) %>% # nolint
    # filter date range
    filter(Tarih >= start_date & Tarih <= end_date)

  # filter parameters(columns)
  data <- data %>% select(Tarih, Istasyon_modified, all_of(parameters)) # nolint

  return(data)
}

# Handlers
get_data_handler <- function(.req, .res) {
  frequency <- .req$parameters_query[["frequency"]]
  parameters <- .req$parameters_query[["parameters"]]
  start_date <- .req$parameters_query[["start_date"]]
  end_date <- .req$parameters_query[["end_date"]]
  region <- .req$parameters_query[["region"]]
  station <- .req$parameters_query[["station"]]
  station_type <- .req$parameters_query[["station_type"]]

  data <- get_data(frequency, parameters, start_date, end_date, region, station, station_type) # nolint

  .res$set_body(data)
  .res$set_content_type("application/json")
}

# Tests
tdata <- get_data(
  frequency = "hourly",
  parameters = c("SO2", "O3"),
  start_date = "2016-06-06",
  region = "Marmara THM",
  station = "Edirne",
)

print(head(tdata))

# Routes
app$add_get(path = "/data", FUN = get_data_handler)

# Start the server
#backend <- BackendRserve$new()
#backend$start(app, http_port = 6666)
