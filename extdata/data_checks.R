# import required libraries
library(dplyr)
library(dbplyr)
library(DBI)
library(logger)
library(stringdist)
library(readxl)
library(writexl)
library(dotenv)
library(temizhavaR)

base_dir <- getOption("temizhavaR.base_dir")

env_file <- file.path(base_dir, ".env")
if (file.exists(env_file)) {
  dotenv::load_dot_env(file = env_file)
  cat(".env file loaded successfully.\n")
} else {
  stop(".env file not found at: ", env_file)
}



# function to find the closest file in a directory (fuzzy search) (only for warning) # nolint
find_closest_file <- function(target_file, search_dir, threshold = 5) {
  files <- list.files(search_dir, full.names = TRUE)

  if (length(files) == 0) {
    return("")  
  }

  distances <- stringdist::stringdist(target_file, basename(files), method = "lv")  # nolint

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
raw_data_dir <- options()$temizhavaR.base_dir

# Import log directory from options
log_dir <- file.path(raw_data_dir, "logs")

# Define log file name (daily timestamped log file), contains logs (info, warning, error) # nolint
log_file <- file.path(log_dir, paste0("data_check_", Sys.time(), ".log")) # nolint

# Define report file name (daily timestamped report file), contains data quality metrics # nolint
report_file <- file.path(log_dir, paste0("report_", Sys.time(), ".xlsx")) # nolint

# Configure both file and database logging
log_appender(appender_file(log_file))
log_layout(layout_glue_generator(format = "{time} | {level} | {msg}"))

# connect to the database
message("Connecting to the database...")
# con <- dbConnect(RSQLite::SQLite(), dbname = file.path(raw_data_dir, "temiz-hava.sqlite")) # nolint
con <- create_postgres_conn()


# Enhanced logging function
log_operation <- function(level, location, message, details = NULL) {
  # Standard file logging
  if (level == "ERROR") {
    log_error(sprintf("%s | %s", location, message))
  } else if (level == "WARN") {
    log_warn(sprintf("%s | %s", location, message))
  } else {
    log_info(sprintf("%s | %s", location, message))
  }
  

}

# Initialize logging session
session_id <- paste0("CHECK_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", random_string("", 6))

# Enhanced logging function with better NULL handling
log_to_db <- function(message, level, category, location_id = NULL, station_name = NULL, details = NULL) {
  if (is.null(message)) {
    message <- "No message provided"
  }
  
  # Handle NULL values for location_id and station_name
  location_id_sql <- if (is.null(location_id)) "NULL" else dbQuoteString(con, as.character(location_id))
  station_name_sql <- if (is.null(station_name)) "NULL" else dbQuoteString(con, as.character(station_name))
  
  # Convert details to JSON with error handling
  details_json <- tryCatch({
    if (is.null(details)) {
      "null"
    } else if (is.list(details)) {
      jsonlite::toJSON(details, auto_unbox = TRUE)
    } else {
      jsonlite::toJSON(list(value = details), auto_unbox = TRUE)
    }
  }, error = function(e) {
    warning("Failed to convert details to JSON: ", e$message)
    "null"
  })

  # Database logging with error handling
  tryCatch({
    sql <- sprintf("
      INSERT INTO data_cleaning_log 
        (session_id, script_name, log_level, category, location_id, station_name, message, details)
      VALUES 
        (%s, %s, %s, %s, %s, %s, %s, %s::jsonb)",
      dbQuoteString(con, session_id),
      dbQuoteString(con, "data_checks"),
      dbQuoteString(con, level),
      dbQuoteString(con, category),
      location_id_sql,
      station_name_sql,
      dbQuoteString(con, message),
      dbQuoteString(con, details_json)
    )
    dbExecute(con, sql)
  }, error = function(e) {
    warning("Failed to write to database log: ", e$message)
  })

  # Console logging always happens even if DB fails
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s | %s | %s\n", 
              timestamp, 
              level, 
              ifelse(is.null(station_name), category, station_name),
              message))
}

# Update logging calls to use new function
log_to_db("Starting data quality checks", "INFO", "system")

# get daily data table
message("Reading daily data table...")
daily_detail <- tbl(con, "daily_detail")

# get hourly data table
message("Reading hourly data table...")
hourly_detail <- tbl(con, "hourly_detail")

# get the location table
message("Reading location table...")
locations <- tbl(con, "location")

# instantiate report data frame
report <- locations %>%
  # if all columns are not NA, then the station data is available
  mutate(station_data = ifelse(!is.na(Sehir) & !is.na(Plaka) & !is.na(Bolge) & !is.na(Istasyon_original) & !is.na(Id) & !is.na(Istasyon_modified), 1, 0)) %>% # nolint
  select(Istasyon_modified, station_data) %>%
  rename(station = Istasyon_modified) %>%
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
    hourly_data_file_row_count_match = 0,
    # Add new columns for tracking cleaned data
    daily_negative_values = 0,
    daily_invalid_pm = 0,
    daily_invalid_nox = 0,
    hourly_negative_values = 0,
    hourly_invalid_pm = 0,
    hourly_invalid_nox = 0,
    hourly_wrong_time = 0
  ) %>%
  collect() %>%
  as.data.frame()

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
  filter(is.na(Bolge) | is.na(Sehir) | is.na(Plaka) | is.na(Istasyon_modified) | is.na(Id)) # nolint

# check if missing locations is empty
missing_locations_is_empty <- missing_locations %>% tally() %>% pull(n) == 0

# log missing locations
if (!missing_locations_is_empty) {
  log_to_db(
    paste0("Missing locations found in the location table: ", missing_locations),
    "ERROR", 
    "location_check"
  )
} else {
  log_to_db(
    "No missing locations found in the location table",
    "INFO", 
    "location_check"
  )
}

# romove missing locations
locations <- locations %>%
  filter(!is.na(Bolge) & !is.na(Sehir) & !is.na(Plaka) & !is.na(Istasyon_modified) & !is.na(Id)) # nolint

# get the number of locations
locations_count <- locations %>% tally() %>% pull(n)

# loop through the locations
for (i in seq_len(as.integer(locations_count))) {

  # get the location
  location <- locations %>%
    mutate(row_num = row_number()) %>%
    filter(row_num == i) %>%
    collect() %>%
    as.data.frame()

  message(paste0("Processing location: ", location$Istasyon_modified)) # nolint

  # get the file paths
  daily_data_file <- file.path(raw_data_dir, location$Sehir, paste0(location$Istasyon_modified, "_gunluk_detay_2014-2024", ".xlsx")) # nolint
  daily_summary_file <- file.path(raw_data_dir, location$Sehir, paste0(location$Istasyon_modified, "_gunluk_ozet_2014-2024", ".xlsx")) # nolint
  hourly_data_file <- file.path(raw_data_dir, location$Sehir, paste0(location$Istasyon_modified, "_saatlik_detay_2014-2024", ".xlsx")) # nolint
  hourly_summary_file <- file.path(raw_data_dir, location$Sehir, paste0(location$Istasyon_modified, "_saatlik_ozet_2014-2024", ".xlsx")) # nolint

  # check if files exist (daily data, daily summary, hourly data, hourly summary) and apply respective checks # nolint

  # DAILY SUMMARY --------------------------------------------------------------
  if (!file.exists(daily_summary_file)) {
    # log the error
    log_to_db("ERROR", location$Istasyon_modified, paste0("Daily summary file does not exist: ", daily_summary_file)) # nolint

    # fuzzy search in directory for the file
    closest_file <- find_closest_file(basename(daily_summary_file), dirname(daily_summary_file)) # nolint
    if (nchar(closest_file) > 0) {
      log_to_db("WARN", location$Istasyon_modified, paste0("Closest daily summary file found: ", closest_file)) # nolint
    }
  } else {
    # update the report with the daily summary file path
    report[report$station == location$Istasyon_modified, "daily_summary_file"] <- daily_summary_file # nolint

    # read the daily summary file
    suppressMessages(daily_summary <- read_excel(daily_summary_file))

    # get second element of the second row (it contains station name)
    second_row <- daily_summary[2, ]
    second_element <- second_row[2]

    # check if the second element of the second row is the same as the station name # nolint
    if (second_element != location$Istasyon_original) {
      log_to_db("ERROR", location$Istasyon_modified, paste0("Daily summary file station name mismatch: ", daily_summary_file)) # nolint
    } else {
      report[report$station == location$Istasyon_modified, "daily_summary_file_station_name_match"] <- 1 # nolint


      
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
        mutate(Parametre = gsub(" ", "", Parametre)) %>%
        # remove dots from parameter names
        mutate(Parametre = gsub("\\.", "", Parametre))
    }
  }

  # DAILY DATA --------------------------------------------------------------
  if (!file.exists(daily_data_file)) {
    log_to_db("ERROR", location$Istasyon_modified, paste0("Daily data file does not exist: ", daily_data_file)) # nolint

    # fuzzy search in directory for the file
    closest_file <- find_closest_file(basename(daily_data_file), dirname(daily_data_file)) # nolint
    if (nchar(closest_file) > 0) {
      log_to_db("WARN", location$Istasyon_modified, paste0("Closest daily data file found: ", closest_file)) # nolint
    }
  } else {
    report[report$station == location$Istasyon_modified, "daily_data_file"] <- daily_data_file # nolint

    # read the daily data file
    suppressMessages(daily_data <- read_excel(daily_data_file))

    # get second column name (it contains station name)
    columns <- colnames(daily_data)
    second_column <- columns[2]

    # check if the second column name is the same as the station name
    if (second_column != location$Istasyon_original) {
      log_to_db("ERROR", location$Istasyon_modified, paste0("Daily data file station name mismatch: ", daily_data_file)) # nolint
    } else {
      report[report$station == location$Istasyon_modified, "daily_data_file_station_name_match"] <- 1 # nolint

      # remove NA date rows from daily data
      daily_data <- daily_data %>%
        filter(!is.na(Tarih))

      # get the daily data from the daily detail table for location
      daily_detail_for_location <- daily_detail %>%
        filter(sql(sprintf('CAST(location_id AS TEXT) = %s', dbQuoteString(con, as.character(location$Id)))))

      # check if there are duplicate data
      daily_detail_for_location_duplicates <- daily_detail_for_location %>%
        collect() %>%
        as.data.frame() %>%
        filter(duplicated(Tarih))

      # log if there are duplicate data
      if (nrow(daily_detail_for_location_duplicates) > 0) {
        log_to_db("WARN", location$Istasyon_modified, "Daily data file contains duplicate data.") # nolint
      }

      daily_detail_for_location_count <- daily_detail_for_location %>% tally() %>% pull(n) # nolint

      # check if the number of rows in the daily data file is the same as the daily detail table # nolint
      if (nrow(daily_data) != daily_detail_for_location_count && (nrow(daily_data) != total_daily_data_count)) { # nolint
        log_to_db("ERROR", location$Istasyon_modified, "Daily data file row count mismatch.") # nolint
      } else {
        report[report$station == location$Istasyon_modified, "daily_data_file_row_count_match"] <- 1 # nolint
      }

      # Add data quality checks
      daily_detail_for_location_count <- daily_detail_for_location %>% tally() %>% pull(n)
      
      if (daily_detail_for_location_count == 0) {
        log_to_db("WARN", location$Istasyon_modified, sprintf("No daily data found in database"))
      } else {
        daily_quality_check <- tidy_air_quality_data(
          daily_detail_for_location, 
          "daily_detail", 
          FALSE, 
          con, 
          update_original = TRUE
        )
        
        # Update report with quality metrics
        report[report$station == location$Istasyon_modified, "daily_negative_values"] <- daily_quality_check$negative_values
        report[report$station == location$Istasyon_modified, "daily_invalid_pm"] <- daily_quality_check$invalid_pm
        report[report$station == location$Istasyon_modified, "daily_invalid_nox"] <- daily_quality_check$invalid_nox
        
        # Use daily_quality_check$cleaned_count instead of daily_detail_for_location_count
        log_to_db("INFO", location$Istasyon_modified, sprintf("Processing %.0f daily records", daily_quality_check$cleaned_count))
        
        if (daily_quality_check$negative_values > 0) {
          log_to_db("WARN", location$Istasyon_modified, sprintf("Daily data: %.0f records (%.2f%%) with negative values will be nullified", 
                          daily_quality_check$negative_values,
                          100 * daily_quality_check$negative_values / daily_quality_check$cleaned_count))
        }
        if (daily_quality_check$invalid_pm > 0) {
          log_to_db("WARN", location$Istasyon_modified, sprintf("Daily data: %.0f records (%.2f%%) with PM2.5 > PM10 will be nullified", 
                          daily_quality_check$invalid_pm,
                          100 * daily_quality_check$invalid_pm / daily_quality_check$cleaned_count))
        }
        if (daily_quality_check$invalid_nox > 0) {
          log_to_db("WARN", location$Istasyon_modified, sprintf("Daily data: %.0f records (%.2f%%) with invalid NOx relationships will be nullified", 
                          daily_quality_check$invalid_nox,
                          100 * daily_quality_check$invalid_nox / daily_quality_check$cleaned_count))
        }
      }

      # for each parameter, check not NA data count
      for (parameter in parameters) {
        # get the daily data for the parameter - count non-NA values in database
        daily_data_for_parameter <- daily_detail_for_location %>%
          summarise(count = sql(sprintf('COUNT(CASE WHEN "%s" IS NOT NULL THEN 1 END)', parameter))) %>%
          collect() %>%
          pull(count)

          
        parameter_data_count <- daily_summary_metrics %>%
          filter(Parametre == parameter) %>%
          select("Veri Adeti") %>%
          pull()

        # check if the number of not NA data for the parameter matches summary
        if (daily_data_for_parameter != parameter_data_count) {
          log_to_db("WARN", location$Istasyon_modified, paste0("Daily data file parameter data count mismatch: ", parameter, ' | ', parameter_data_count, '/', daily_data_for_parameter)) # nolint
        } else {
          report[report$station == location$Istasyon_modified, paste0("daily_data_file_", parameter, "_data_count_match")] <- 1 # nolint
        }
      }
      
      # Check if there were any previous cleanings even if current check found no issues
      previous_cleanings <- tryCatch({
        dbGetQuery(con, sprintf("
          SELECT 
            MAX(operation_time) as last_cleaned_at,
            COUNT(*)::integer as clean_operations,
            SUM(CASE WHEN field_name = 'PM10,PM25' THEN affected_rows ELSE 0 END)::integer as pm_fixes,
            SUM(CASE WHEN field_name = 'NO,NO2,NOX' THEN affected_rows ELSE 0 END)::integer as nox_fixes,
            SUM(CASE WHEN reason = 'negative_values' THEN affected_rows ELSE 0 END)::integer as negative_fixes,
            SUM(CASE WHEN reason = 'invalid_time_format' THEN affected_rows ELSE 0 END)::integer as time_fixes
          FROM data_quality_log
          WHERE location_id = %s 
          AND table_name = 'daily_detail'
          AND operation_type = 'nullify'
          GROUP BY location_id",
          dbQuoteString(con, as.character(location$Id))
        ))
      }, error = function(e) {
        log_to_db("WARN", location$Istasyon_modified, sprintf("Failed to query previous cleanings: %s", e$message))
        return(data.frame())
      })
      
      if (nrow(previous_cleanings) > 0 && previous_cleanings$clean_operations > 0) {
        tryCatch({
          # Format the last cleaned date
          last_cleaned <- format(previous_cleanings$last_cleaned_at, "%Y-%m-%d %H:%M:%S")
          
          # Log information about previously cleaned data using %i for integers
          log_to_db("INFO", location$Istasyon_modified, sprintf(
            "Previously cleaned daily data: %i total operations on %s (PM fixes: %i, NOx fixes: %i, Negative values: %i)",
            as.integer(previous_cleanings$clean_operations), 
            last_cleaned,
            as.integer(previous_cleanings$pm_fixes),
            as.integer(previous_cleanings$nox_fixes),
            as.integer(previous_cleanings$negative_fixes)
          ))
          
          # Add historical cleaning data to report
          report[report$station == location$Istasyon_modified, "daily_negative_values"] <- 
            report[report$station == location$Istasyon_modified, "daily_negative_values"] + as.integer(previous_cleanings$negative_fixes)
          report[report$station == location$Istasyon_modified, "daily_invalid_pm"] <- 
            report[report$station == location$Istasyon_modified, "daily_invalid_pm"] + as.integer(previous_cleanings$pm_fixes)
          report[report$station == location$Istasyon_modified, "daily_invalid_nox"] <- 
            report[report$station == location$Istasyon_modified, "daily_invalid_nox"] + as.integer(previous_cleanings$nox_fixes)
        }, error = function(e) {
          log_to_db("WARN", location$Istasyon_modified, sprintf("Error processing previous cleaning data: %s", e$message))
        })
      }
    }
  }

  # HOURLY SUMMARY -------------------------------------------------------------
  if (!file.exists(hourly_summary_file)) {
    log_to_db("ERROR", location$Istasyon_modified, paste0("Hourly summary file does not exist: ", hourly_summary_file)) # nolint

    # fuzzy search in directory for the file
    closest_file <- find_closest_file(basename(hourly_summary_file), dirname(hourly_summary_file)) # nolint
    if (nchar(closest_file) > 0) {
      log_to_db("WARN", location$Istasyon_modified, paste0("Closest hourly summary file found: ", closest_file)) # nolint
    }
  } else {
    report[report$station == location$Istasyon_modified, "hourly_summary_file"] <- hourly_summary_file # nolint

    # read the hourly summary file
    suppressMessages(hourly_summary <- read_excel(hourly_summary_file))

    # get second element of the second row (it contains station name)
    second_row <- hourly_summary[2, ]
    second_element <- second_row[2]

    # check if the second element of the second row is the same as the station name # nolint
    if (second_element != location$Istasyon_original) {
      log_to_db("ERROR", location$Istasyon_modified, paste0("Hourly summary file station name mismatch: ", hourly_summary_file)) # nolint
    } else {
      report[report$station == location$Istasyon_modified, "hourly_summary_file_station_name_match"] <- 1 # nolint

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
        mutate(Parametre = gsub(" ", "", Parametre)) %>%
        # remove dots from parameter names
        mutate(Parametre = gsub("\\.", "", Parametre))
    }
  }

  # HOURLY DATA -------------------------------------------------------------
  if (!file.exists(hourly_data_file)) {
    log_to_db("ERROR", location$Istasyon_modified, paste0("Hourly data file does not exist: ", hourly_data_file)) # nolint

    # fuzzy search in directory for the file
    closest_file <- find_closest_file(basename(hourly_data_file), dirname(hourly_data_file)) # nolint
    if (nchar(closest_file) > 0) {
      log_to_db("WARN", location$Istasyon_modified, paste0("Closest hourly data file found: ", closest_file)) # nolint
    }
  } else {
    report[report$station == location$Istasyon_modified, "hourly_data_file"] <- hourly_data_file # nolint

    # read the hourly data file
    suppressMessages(hourly_data <- read_excel(hourly_data_file))

    # get second column name (it contains station name)
    columns <- colnames(hourly_data)
    second_column <- columns[2]

    if (second_column != location$Istasyon_original) {
      log_to_db("ERROR", location$Istasyon_modified, paste0("Hourly data file station name mismatch: ", hourly_data_file)) # nolint
    } else {
      report[report$station == location$Istasyon_modified, "hourly_data_file_station_name_match"] <- 1 # nolint

      # remove NA date rows from hourly data
      hourly_data <- hourly_data %>%
        filter(!is.na(Tarih))

      # get the hourly data from the hourly detail table for location
      hourly_detail_for_location <- hourly_detail %>%
        filter(sql(sprintf('CAST(location_id AS TEXT) = %s', dbQuoteString(con, as.character(location$Id)))))

      hourly_detail_for_location_count <- hourly_detail_for_location %>% 
        tally() %>% 
        pull(n)

      # check if there are duplicate data
      hourly_detail_for_location_duplicates <- hourly_detail_for_location %>%
        collect() %>%
        as.data.frame() %>%
        filter(duplicated(Tarih))

      # log if there are duplicate data
      if (nrow(hourly_detail_for_location_duplicates) > 0) {
        log_to_db("WARN", location$Istasyon_modified, "Daily data file contains duplicate data.") # nolint
      }

      # check if the number of rows in the hourly data file is the same as the hourly detail table # nolint
      if (nrow(hourly_data) != hourly_detail_for_location_count && (nrow(hourly_data) != total_hourly_data_count)) { # nolint
        log_to_db("ERROR", location$Istasyon_modified, "Hourly data file row count mismatch.") # nolint
      } else {
        report[report$station == location$Istasyon_modified, "hourly_data_file_row_count_match"] <- 1 # nolint
      }

      hourly_detail_for_location_count <- hourly_detail_for_location %>% tally() %>% pull(n)
      
      if (hourly_detail_for_location_count == 0) {
        log_to_db("WARN", location$Istasyon_modified, sprintf("No hourly data found in database"))
      } else {
        hourly_quality_check <- tidy_air_quality_data(
          hourly_detail_for_location, 
          "hourly_detail", 
          TRUE, 
          con, 
          update_original = TRUE
        )
        
        report[report$station == location$Istasyon_modified, "hourly_negative_values"] <- hourly_quality_check$negative_values
        report[report$station == location$Istasyon_modified, "hourly_invalid_pm"] <- hourly_quality_check$invalid_pm
        report[report$station == location$Istasyon_modified, "hourly_invalid_nox"] <- hourly_quality_check$invalid_nox
        report[report$station == location$Istasyon_modified, "hourly_wrong_time"] <- hourly_quality_check$wrong_time
        
        # Use hourly_quality_check$cleaned_count instead of hourly_detail_for_location_count
        log_to_db("INFO", location$Istasyon_modified, sprintf("Processing %.0f hourly records", hourly_quality_check$cleaned_count))
        
        if (hourly_quality_check$negative_values > 0) {
          log_to_db("WARN", location$Istasyon_modified, sprintf("Hourly data: %.0f records (%.2f%%) with negative values will be nullified", 
                          hourly_quality_check$negative_values,
                          100 * hourly_quality_check$negative_values / hourly_quality_check$cleaned_count))
        }
        if (hourly_quality_check$invalid_pm > 0) {
          log_to_db("WARN", location$Istasyon_modified, sprintf("Hourly data: %.0f records (%.2f%%) with PM2.5 > PM10 will be nullified", 
                          hourly_quality_check$invalid_pm,
                          100 * hourly_quality_check$invalid_pm / hourly_quality_check$cleaned_count))
        }
        if (hourly_quality_check$invalid_nox > 0) {
          log_to_db("WARN", location$Istasyon_modified, sprintf("Hourly data: %.0f records (%.2f%%) with invalid NOx relationships will be nullified", 
                          hourly_quality_check$invalid_nox,
                          100 * hourly_quality_check$invalid_nox / hourly_quality_check$cleaned_count))
        }
        if (hourly_quality_check$wrong_time > 0) {
          log_to_db("WARN", location$Istasyon_modified, sprintf("Hourly data: %.0f records (%.2f%%) with incorrect time format will be removed", 
                          hourly_quality_check$wrong_time,
                          100 * hourly_quality_check$wrong_time / hourly_quality_check$cleaned_count))
        }
      }

      # for each parameter, check not NA data count
      for (parameter in parameters) {
        # get the hourly data for the parameter - count non-NA values in database
        hourly_data_for_parameter <- hourly_detail_for_location %>%
          summarise(count = sql(sprintf('COUNT(CASE WHEN "%s" IS NOT NULL THEN 1 END)', parameter))) %>%
          collect() %>%
          pull(count)

        parameter_data_count <- hourly_summary_metrics %>%
          filter(Parametre == parameter) %>%
          select("Veri Adeti") %>%
          pull()

        # check if the number of not NA data for the parameter matches summary
        if (hourly_data_for_parameter != parameter_data_count) {
          log_to_db("WARN", location$Istasyon_modified, paste0("Hourly data file parameter data count mismatch: ", parameter, ' | ', parameter_data_count, '/', hourly_data_for_parameter)) # nolint
        } else {
          report[report$station == location$Istasyon_modified, paste0("hourly_data_file_", parameter, "_data_count_match")] <- 1 # nolint
        }
      }
      
      # Check if there were any previous cleanings for hourly data
      previous_hourly_cleanings <- tryCatch({
        dbGetQuery(con, sprintf("
          SELECT 
            MAX(operation_time) as last_cleaned_at,
            COUNT(*)::integer as clean_operations,
            SUM(CASE WHEN field_name = 'PM10,PM25' THEN affected_rows ELSE 0 END)::integer as pm_fixes,
            SUM(CASE WHEN field_name = 'NO,NO2,NOX' THEN affected_rows ELSE 0 END)::integer as nox_fixes,
            SUM(CASE WHEN reason = 'negative_values' THEN affected_rows ELSE 0 END)::integer as negative_fixes,
            SUM(CASE WHEN field_name = 'all' AND reason = 'invalid_time_format' THEN affected_rows ELSE 0 END)::integer as time_fixes
          FROM data_quality_log
          WHERE location_id = %s 
          AND table_name = 'hourly_detail'
          AND operation_type = 'nullify'
          GROUP BY location_id",
          dbQuoteString(con, as.character(location$Id))
        ))
      }, error = function(e) {
        log_to_db("WARN", location$Istasyon_modified, sprintf("Failed to query previous hourly cleanings: %s", e$message))
        return(data.frame())
      })
      
      if (nrow(previous_hourly_cleanings) > 0 && previous_hourly_cleanings$clean_operations > 0) {
        tryCatch({
          # Format the last cleaned date
          last_cleaned <- format(previous_hourly_cleanings$last_cleaned_at, "%Y-%m-%d %H:%M:%S")
          
          # Log information about previously cleaned data using %i for integers
          log_to_db("INFO", location$Istasyon_modified, sprintf(
            "Previously cleaned hourly data: %i total operations on %s (PM fixes: %i, NOx fixes: %i, Negative values: %i, Time format fixes: %i)",
            as.integer(previous_hourly_cleanings$clean_operations), 
            last_cleaned,
            as.integer(previous_hourly_cleanings$pm_fixes),
            as.integer(previous_hourly_cleanings$nox_fixes),
            as.integer(previous_hourly_cleanings$negative_fixes),
            as.integer(previous_hourly_cleanings$time_fixes)
          ))
          
          # Add historical cleaning data to report
          report[report$station == location$Istasyon_modified, "hourly_negative_values"] <- 
            report[report$station == location$Istasyon_modified, "hourly_negative_values"] + as.integer(previous_hourly_cleanings$negative_fixes)
          report[report$station == location$Istasyon_modified, "hourly_invalid_pm"] <- 
            report[report$station == location$Istasyon_modified, "hourly_invalid_pm"] + as.integer(previous_hourly_cleanings$pm_fixes)
          report[report$station == location$Istasyon_modified, "hourly_invalid_nox"] <- 
            report[report$station == location$Istasyon_modified, "hourly_invalid_nox"] + as.integer(previous_hourly_cleanings$nox_fixes)
          report[report$station == location$Istasyon_modified, "hourly_wrong_time"] <- 
            report[report$station == location$Istasyon_modified, "hourly_wrong_time"] + as.integer(previous_hourly_cleanings$time_fixes)
        }, error = function(e) {
          log_to_db("WARN", location$Istasyon_modified, sprintf("Error processing previous hourly cleaning data: %s", e$message))
        })
      }
    }
  }
}

# write the report to an excel file
write_xlsx(report, report_file)

# close the connection
dbDisconnect(con)

# log the completion
log_to_db("INFO", "system", "Data checks completed successfully")

# message the completion
message("Data checks completed successfully")
