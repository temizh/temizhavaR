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

# define parameters to ckeck
parameters <- c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3") # nolint

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
message("Connecting to the database...")
con <- dbConnect(RSQLite::SQLite(), dbname = file.path(raw_data_dir, "temiz-hava.sqlite")) # nolint

# get daily data table
message("Reading daily data table...")
daily_detail <- dbGetQuery(con, "SELECT * FROM daily_detail")

# get hourly data table
message("Reading hourly data table...")
hourly_detail <- dbGetQuery(con, "SELECT * FROM hourly_detail")

# get the location table
message("Reading location table...")
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
    hourly_summary_file_station_name_match = 0,
    daily_data_file_row_count_match = 0,
    hourly_data_file_row_count_match = 0
  )

# add data count match columns for each parameter to report
for (parameter in parameters) {
  report <- report %>%
    mutate(
      !!paste0("daily_data_file_", parameter, "_data_count_match") := 0
    )
}

# get missing locations
message("Checking for missing locations...")
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
  message(paste0("Processing location: ", location$Istasyonlar_modified)) # nolint

  # get the file paths
  daily_data_file <- file.path(raw_data_dir, location$Sehir, paste0(location$Istasyonlar_modified, "_gunluk_detay_2014-2024", ".xlsx")) # nolint
  daily_summary_file <- file.path(raw_data_dir, location$Sehir, paste0(location$Istasyonlar_modified, "_gunluk_ozet_2014-2024", ".xlsx")) # nolint
  hourly_data_file <- file.path(raw_data_dir, location$Sehir, paste0(location$Istasyonlar_modified, "_saatlik_detay_2014-2024", ".xlsx")) # nolint
  hourly_summary_file <- file.path(raw_data_dir, location$Sehir, paste0(location$Istasyonlar_modified, "_saatlik_ozet_2014-2024", ".xlsx")) # nolint

  # check if files exist (daily data, daily summary, hourly data, hourly summary) and apply respective checks # nolint

  # DAILY SUMMARY --------------------------------------------------------------
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

      # get the total data count for the location
      total_daily_data_count <- daily_summary %>%
        filter(Parametre == parameters[1]) %>%
        select("Olması Gereken Veri") %>%
        pull()

      # get the daily summary metrics
      daily_summary_metrics <- daily_summary %>%
        filter(!is.na(Parametre)) %>%
        select("Parametre", "Veri Adeti") %>%
        # remove spaces from parameter names
        mutate(Parametre = gsub(" ", "", Parametre)) 
    }
  }

  # DAILY DATA --------------------------------------------------------------
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

      # remove NA date rows from daily data
      daily_data <- daily_data %>%
        filter(!is.na(Tarih))

      # get the daily data from the daily detail table for location
      daily_detail_for_location <- daily_detail %>%
        filter(location_id == location$Id)

      # check if the number of rows in the daily data file is the same as the daily detail table # nolint
      if (nrow(daily_data) != nrow(daily_detail_for_location) && (nrow(daily_data) != total_daily_data_count)) { # nolint
        log_error(paste0(location$Istasyonlar_modified, " | ", "Daily data file row count mismatch.")) # nolint
      } else {
        report[report$station == location$Istasyonlar_modified, "daily_data_file_row_count_match"] <- 1 # nolint
      }

      # for each parameter, check not NA data count
      for (parameter in parameters) {
        # get the daily data for the parameter
        daily_data_for_parameter <- daily_detail_for_location %>%
          select(parameter) %>%
          na.omit()

        parameter_data_count <- daily_summary_metrics %>%
          filter(Parametre == parameter) %>%
          select("Veri Adeti") %>%
          pull()

        # get the number of not NA data for the parameter
        if (nrow(daily_data_for_parameter) == 0) {
          avaliable_parameters <- 0
        } else {
          avaliable_parameters <- nrow(daily_data_for_parameter)
        }

        # check if the number of not NA data for the parameter is the same as the daily summary metrics # nolint
        if (avaliable_parameters != parameter_data_count) {
          log_warn(paste0(location$Istasyonlar_modified, " | ", "Daily data file parameter data count mismatch: ", parameter, ' | ', parameter_data_count, '/', avaliable_parameters)) # nolint
        } else {
          report[report$station == location$Istasyonlar_modified, paste0("daily_data_file_", parameter, "_data_count_match")] <- 1 # nolint
        }
      }
    }
  }

  # HOURLY SUMMARY -------------------------------------------------------------
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

      # get the hourly data count for the location
      total_hourly_data_count <- hourly_summary %>%
        filter(Parametre == parameters[1]) %>%
        select("Olması Gereken Veri") %>%
        pull()

      # get the hourly summary metrics
      hourly_summary_metrics <- hourly_summary %>%
        filter(!is.na(Parametre)) %>%
        select("Parametre", "Veri Adeti") %>%
        # remove spaces from parameter names
        mutate(Parametre = gsub(" ", "", Parametre))
    }
  }

  # HOURLY DATA -------------------------------------------------------------
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

      # remove NA date rows from hourly data
      hourly_data <- hourly_data %>%
        filter(!is.na(Tarih))

      # get the hourly data from the hourly detail table for location
      hourly_detail_for_location <- hourly_detail %>%
        filter(location_id == location$Id)

      # check if the number of rows in the hourly data file is the same as the hourly detail table # nolint
      if (nrow(hourly_data) != nrow(hourly_detail_for_location) && (nrow(hourly_data) != total_hourly_data_count)) { # nolint
        log_error(paste0(location$Istasyonlar_modified, " | ", "Hourly data file row count mismatch.")) # nolint
      } else {
        report[report$station == location$Istasyonlar_modified, "hourly_data_file_row_count_match"] <- 1 # nolint
      }

      # for each parameter, check not NA data count
      for (parameter in parameters) {
        # get the hourly data for the parameter
        hourly_data_for_parameter <- hourly_detail_for_location %>%
          select(parameter) %>%
          na.omit()

        parameter_data_count <- hourly_summary_metrics %>%
          filter(Parametre == parameter) %>%
          select("Veri Adeti") %>%
          pull()

        # get the number of not NA data for the parameter
        if (nrow(hourly_data_for_parameter) == 0) {
          avaliable_parameters <- 0
        } else {
          avaliable_parameters <- nrow(hourly_data_for_parameter)
        }

        # check if the number of not NA data for the parameter is the same as the hourly summary metrics # nolint
        if (avaliable_parameters != parameter_data_count) {
          log_warn(paste0(location$Istasyonlar_modified, " | ", "Hourly data file parameter data count mismatch: ", parameter, ' | ', parameter_data_count, '/', avaliable_parameters)) # nolint
        } else {
          report[report$station == location$Istasyonlar_modified, paste0("hourly_data_file_", parameter, "_data_count_match")] <- 1 # nolint
        }
      }
    }
  }
}

# write the report to an excel file
write_xlsx(report, report_file)

# close the connection
dbDisconnect(con)

# log the completion
log_info("Data checks completed successfully")

# message the completion
message("Data checks completed successfully")