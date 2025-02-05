# import required libraries
library(dplyr)
library(DBI)
library(logger)
library(stringdist)
library(readxl)
library(writexl)

# function to find the closest file in a directory (fuzzy search) (only for warning) # nolint
find_closest_file <- function(target_file, search_dir, threshold = 5) {
  # List all files in the directory
  files <- list.files(search_dir, full.names = TRUE)

  if (length(files) == 0) {
    return("")  # No files found
  }

  # Compute string distance using Levenshtein method
  distances <- stringdist::stringdist(target_file, basename(files), method = "lv")  # nolint

  # Find the best match
  best_match_index <- which.min(distances)
  best_match <- files[best_match_index]
  best_distance <- distances[best_match_index]

  # Check if the best match is within the allowed threshold
  if (best_distance <= threshold) {
    return(best_match)
  } else {
    return("")
  }
}

# import raw data path from options
raw_data_dir <- options()$temizhavaR.raw_dir

# Import log directory from options
log_dir <- options()$temizhavaR.log_dir

# Define log file name (daily timestamped log file), contains logs (info, warning, error) # nolint
log_file <- file.path(log_dir, paste0("data_check_", Sys.time(), ".log")) # nolint

# Define report file name (daily timestamped report file), contains data quality metrics # nolint
report_file <- file.path(log_dir, paste0("report_", Sys.time(), ".xlsx")) # nolint

# Configure logger to append logs to the file
log_appender(appender_file(log_file))

# Use a structured log format
log_layout(layout_glue_generator(format = "{time} | {level} | {msg}"))

# connect to the database
con <- dbConnect(RSQLite::SQLite(), dbname = file.path(raw_data_dir, "temiz-hava.sqlite")) # nolint

# get the location table
locations <- dbGetQuery(con, "SELECT * FROM location")

# instantiate report data frame
report <- locations %>%
  # if all columns are not NA, then the station data is available
  mutate(station_data = ifelse(!is.na(Sehir) & !is.na(Plaka) & !is.na(Bolge) & !is.na(Istasyonlar) & !is.na(Id) & !is.na(Istasyonlar_modified), 1, 0)) %>% # nolint
  select(Istasyonlar_modified, station_data) %>%
  rename(station = Istasyonlar_modified) %>%
  mutate(
    daily_data_file = "",
    daily_summary_file = "",
    hourly_data_file = "",
    hourly_summary_file = "",
    daily_data_file_station_name_match = 0,
    daily_summary_file_station_name_match = 0,
    hourly_data_file_station_name_match = 0,
    hourly_summary_file_station_name_match = 0
  )

# get missing locations
missing_locations <- locations %>%
  filter(is.na(Bolge) | is.na(Sehir) | is.na(Plaka) | is.na(Istasyonlar_modified) | is.na(Id)) # nolint

# log missing locations
if (nrow(missing_locations) > 0) {
  log_error(paste0("Missing locations found in the location table: ", missing_locations)) # nolint
} else {
  log_info("No missing locations found in the location table") # nolint
}

# romove missing locations
locations <- locations %>%
  filter(!is.na(Bolge) & !is.na(Sehir) & !is.na(Plaka) & !is.na(Istasyonlar_modified) & !is.na(Id)) # nolint

# loop through the locations
for (i in seq_len(nrow(locations))) { # nolint
  location <- locations[i, ]
  print(paste0("Processing location: ", location$Istasyonlar_modified)) # nolint

  # get the file paths
  daily_data_file <- file.path(raw_data_dir, location$Sehir, paste0(location$Istasyonlar_modified, "_gunluk_detay_2014-2024", ".xlsx")) # nolint
  daily_summary_file <- file.path(raw_data_dir, location$Sehir, paste0(location$Istasyonlar_modified, "_gunluk_ozet_2014-2024", ".xlsx")) # nolint
  hourly_data_file <- file.path(raw_data_dir, location$Sehir, paste0(location$Istasyonlar_modified, "_saatlik_detay_2014-2024", ".xlsx")) # nolint
  hourly_summary_file <- file.path(raw_data_dir, location$Sehir, paste0(location$Istasyonlar_modified, "_saatlik_ozet_2014-2024", ".xlsx")) # nolint

  # check if files exist (daily data, daily summary, hourly data, hourly summary) # nolint
  if (!file.exists(daily_data_file)) {
    log_error(paste0(location$Istasyonlar_modified, " | ", "Daily data file does not exist: ", daily_data_file)) # nolint

    # fuzzy search in directory for the file
    closest_file <- find_closest_file(basename(daily_data_file), dirname(daily_data_file)) # nolint
    if (nchar(closest_file) > 0) {
      log_warn(paste0(location$Istasyonlar_modified, " | ", "Closest daily data file found: ", closest_file)) # nolint
    }
  } else {
    report[report$station == location$Istasyonlar_modified, "daily_data_file"] <- daily_data_file # nolint

    # read the daily data file
    suppressMessages(daily_data <- read_excel(daily_data_file))

    # get second column name (it contains station name)
    columns <- colnames(daily_data)
    second_column <- columns[2]

    # check if the second column name is the same as the station name
    if (second_column != location$Istasyonlar) {
      log_error(paste0(location$Istasyonlar_modified, " | ", "Daily data file station name mismatch: ", daily_data_file)) # nolint
    } else {
      report[report$station == location$Istasyonlar_modified, "daily_data_file_station_name_match"] <- 1 # nolint
    }

    # print number of rows
    print(paste0("Number of rows in daily data file: ", nrow(daily_data))) # nolint
  }

  if (!file.exists(daily_summary_file)) {
    log_error(paste0(location$Istasyonlar_modified, " | ", "Daily summary file does not exist: ", daily_summary_file)) # nolint

    # fuzzy search in directory for the file
    closest_file <- find_closest_file(basename(daily_summary_file), dirname(daily_summary_file)) # nolint
    if (nchar(closest_file) > 0) {
      log_warn(paste0(location$Istasyonlar_modified, " | ", "Closest daily summary file found: ", closest_file)) # nolint
    }
  } else {
    report[report$station == location$Istasyonlar_modified, "daily_summary_file"] <- daily_summary_file # nolint

    # read the daily summary file
    suppressMessages(daily_summary <- read_excel(daily_summary_file))

    # get second element of the second row (it contains station name)
    second_row <- daily_summary[2, ]
    second_element <- second_row[2]

    # check if the second element of the second row is the same as the station name # nolint
    if (second_element != location$Istasyonlar) {
      log_error(paste0(location$Istasyonlar_modified, " | ", "Daily summary file station name mismatch: ", daily_summary_file)) # nolint
    } else {
      report[report$station == location$Istasyonlar_modified, "daily_summary_file_station_name_match"] <- 1 # nolint
    }

    # print number of rows
    print(paste0("Number of rows in daily summary file: ", nrow(daily_summary))) # nolint
  }

  if (!file.exists(hourly_data_file)) {
    log_error(paste0(location$Istasyonlar_modified, " | ", "Hourly data file does not exist: ", hourly_data_file)) # nolint

    # fuzzy search in directory for the file
    closest_file <- find_closest_file(basename(hourly_data_file), dirname(hourly_data_file)) # nolint
    if (nchar(closest_file) > 0) {
      log_warn(paste0(location$Istasyonlar_modified, " | ", "Closest hourly data file found: ", closest_file)) # nolint
    }
  } else {
    report[report$station == location$Istasyonlar_modified, "hourly_data_file"] <- hourly_data_file # nolint

    # read the hourly data file
    suppressMessages(hourly_data <- read_excel(hourly_data_file))

    # get second column name (it contains station name)
    columns <- colnames(hourly_data)
    second_column <- columns[2]

    # check if the second column name is the same as the station name
    if (second_column != location$Istasyonlar) {
      log_error(paste0(location$Istasyonlar_modified, " | ", "Hourly data file station name mismatch: ", hourly_data_file)) # nolint
    } else {
      report[report$station == location$Istasyonlar_modified, "hourly_data_file_station_name_match"] <- 1 # nolint
    }

    # print number of rows
    print(paste0("Number of rows in hourly data file: ", nrow(hourly_data))) # nolint
  }

  if (!file.exists(hourly_summary_file)) {
    log_error(paste0(location$Istasyonlar_modified, " | ", "Hourly summary file does not exist: ", hourly_summary_file)) # nolint

    # fuzzy search in directory for the file
    closest_file <- find_closest_file(basename(hourly_summary_file), dirname(hourly_summary_file)) # nolint
    if (nchar(closest_file) > 0) {
      log_warn(paste0(location$Istasyonlar_modified, " | ", "Closest hourly summary file found: ", closest_file)) # nolint
    }
  } else {
    report[report$station == location$Istasyonlar_modified, "hourly_summary_file"] <- hourly_summary_file # nolint

    # read the hourly summary file
    suppressMessages(hourly_summary <- read_excel(hourly_summary_file))

    # get second element of the second row (it contains station name)
    second_row <- hourly_summary[2, ]
    second_element <- second_row[2]

    # check if the second element of the second row is the same as the station name # nolint
    if (second_element != location$Istasyonlar) {
      log_error(paste0(location$Istasyonlar_modified, " | ", "Hourly summary file station name mismatch: ", hourly_summary_file)) # nolint
    } else {
      report[report$station == location$Istasyonlar_modified, "hourly_summary_file_station_name_match"] <- 1 # nolint
    }

    # print number of rows
    print(paste0("Number of rows in hourly summary file: ", nrow(hourly_summary))) # nolint
  }
}

# write the report to an excel file
write_xlsx(report, report_file)

# close the connection
dbDisconnect(con)