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


random_string <-function(chars, len) {
  set.seed(123)
  char_set <- c(letters, LETTERS, 0:9)
  if (nchar(chars) > 0) {
    char_set <- chars
  }
  paste0(sample(char_set, len, replace = TRUE), collapse = "")
}

find_closest_file <- function(target_file, search_dir, threshold = 5) {
  files <- list.files(search_dir, full.names = TRUE)

  if (length(files) == 0) {
    return("")  
  }

  distances <- stringdist::stringdist(target_file, basename(files), method = "lv")

  best_match_index <- which.min(distances)
  best_match <- files[best_match_index]
  best_distance <- distances[best_match_index]

  if (best_distance <= threshold) {
    return(best_match)
  } else {
    return("")
  }
}

find_best_file <- function(base_dir, city, station, file_type) {
  date_ranges <- c("2024-2025", "2014-2024", "2023-2024", "2022-2023", "2020-2024", "2010-2024")
  
  safe_station <- gsub("[^[:alnum:]-]", "_", station)
  
  for (date_range in date_ranges) {
    file_path <- file.path(base_dir, city, paste0(safe_station, "_", file_type, "_", date_range, ".xlsx"))
    if (file.exists(file_path)) {
      return(file_path)
    }
    
    orig_file_path <- file.path(base_dir, city, paste0(station, "_", file_type, "_", date_range, ".xlsx"))
    if (file.exists(orig_file_path)) {
      return(orig_file_path)
    }
  }
  
  city_dir <- file.path(base_dir, city)
  if (dir.exists(city_dir)) {
    pattern1 <- paste0("^", safe_station, "_", file_type, "_.*\\.xlsx$")
    pattern2 <- paste0("^", station, "_", file_type, "_.*\\.xlsx$")
    
    matching_files <- c(
      list.files(city_dir, pattern = pattern1, full.names = TRUE),
      list.files(city_dir, pattern = pattern2, full.names = TRUE)
    )
    
    if (length(matching_files) > 0) {
      if (length(matching_files) > 1) {
        base_names <- basename(matching_files)
        latest_idx <- which.max(sapply(base_names, function(name) {
          year_match <- regexpr("\\d{4}-\\d{4}", name)
          if (year_match > 0) {
            year_range <- regmatches(name, year_match)
            end_year <- as.numeric(substring(year_range, 6, 9))
            return(end_year)
          }
          return(0) 
        }))
        return(matching_files[latest_idx])
      } else {
        return(matching_files[1])
      }
    }
  }
  
  return(file.path(base_dir, city, paste0(safe_station, "_", file_type, "_2014-2024.xlsx")))
}

extract_date_range <- function(file_path) {
  basename_file <- basename(file_path)
  date_match <- regexpr("\\d{4}-\\d{4}", basename_file)
  
  if (date_match > 0) {
    date_range <- regmatches(basename_file, date_match)
    start_year <- as.numeric(substr(date_range, 1, 4))
    end_year <- as.numeric(substr(date_range, 6, 9))
    
    start_date <- as.Date(paste0(start_year, "-01-01"))
    end_date <- as.Date(paste0(end_year, "-12-31"))
    
    return(list(start_date = start_date, end_date = end_date))
  }
  
  return(list(start_date = as.Date("2000-01-01"), end_date = as.Date("2100-12-31")))
}

parameters <- c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3")

raw_data_dir <- options()$temizhavaR.base_dir  

log_dir <- file.path(raw_data_dir, "logs")

log_file <- file.path(log_dir, paste0("data_check_", Sys.time(), ".log"))

report_file <- file.path(log_dir, paste0("report_", Sys.time(), ".xlsx"))

log_appender(appender_file(log_file))
log_layout(layout_glue_generator(format = "{time} | {level} | {msg}"))

message("Connecting to the database...")
con <- temizhavaR:::create_postgres_conn()

dbExecute(con, "CREATE TABLE IF NOT EXISTS data_cleaning_log (
  id SERIAL PRIMARY KEY,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  session_id TEXT,
  timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  script_name TEXT,
  log_level TEXT,
  category TEXT,
  location_id TEXT,
  station_name TEXT,
  message TEXT,
  details JSONB
)")

log_operation <- function(level, location, message, details = NULL) {
  if (level == "ERROR") {
    log_error(sprintf("%s | %s", location, message))
  } else if (level == "WARN") {
    log_warn(sprintf("%s | %s", location, message))
  } else {
    log_info(sprintf("%s | %s", location, message))
  }
}

session_id <- paste0("CHECK_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", random_string("", 6))

log_to_db <- function(level, station_name, message, location_id = NULL, category = "check", details = NULL) {
  if (is.null(message)) {
    message <- "No message provided"
  }
  
  location_id_sql <- if (is.null(location_id)) "NULL" else dbQuoteString(con, as.character(location_id))
  station_name_sql <- if (is.null(station_name)) "NULL" else dbQuoteString(con, as.character(station_name))
  
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

  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s | %s | %s\n", 
              timestamp, 
              level, 
              ifelse(is.null(station_name), category, station_name),
              message))
}

log_to_db("INFO", "system", "Starting data quality checks")

message("Reading daily data table...")
daily_detail <- tbl(con, "daily_detail")

message("Reading hourly data table...")
hourly_detail <- tbl(con, "hourly_detail")

message("Reading location table...")
locations <- tbl(con, "location")

get_station_name <- function(loc_id) {
  tryCatch({
    result <- dbGetQuery(con, sprintf("SELECT \"Istasyon_modified\" FROM location WHERE \"Id\" = %s", 
                                     dbQuoteString(con, as.character(loc_id))))
    if (nrow(result) > 0) {
      return(result$Istasyon_modified[1])
    }
    return(paste0("Unknown_Station_", loc_id))
  }, error = function(e) {
    return(paste0("Unknown_Station_", loc_id))
  })
}

report <- locations %>%
  mutate(station_data = ifelse(!is.na(Sehir) & !is.na(Plaka) & !is.na(Bolge) & !is.na(Istasyon_original) & !is.na(Id) & !is.na(Istasyon_modified), 1, 0)) %>%
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
    daily_negative_values = 0,
    daily_invalid_pm = 0,
    hourly_negative_values = 0,
    hourly_invalid_pm = 0,
    hourly_wrong_time = 0
  ) %>%
  collect() %>%
  as.data.frame()

for (parameter in parameters) {
  report <- report %>%
    mutate(
      !!paste0("daily_data_file_", parameter, "_data_count_match") := 0
    )
}

message("Checking for missing locations...")
missing_locations <- locations %>%
  filter(is.na(Bolge) | is.na(Sehir) | is.na(Plaka) | is.na(Istasyon_modified) | is.na(Id))

missing_locations_is_empty <- missing_locations %>% tally() %>% pull(n) == 0

if (!missing_locations_is_empty) {
  log_to_db(
    "ERROR",
    "location_check",
    paste0("Missing locations found in the location table: ", missing_locations),
    NULL,  
    "location_check" 
  )
} else {
  log_to_db(
    "INFO",
    "location_check", 
    "No missing locations found in the location table",
    NULL,  
    "location_check" 
  )
}

locations <- locations %>%
  filter(!is.na(Bolge) & !is.na(Sehir) & !is.na(Plaka) & !is.na(Istasyon_modified) & !is.na(Id))

locations_count <- locations %>% tally() %>% pull(n)

for (i in seq_len(as.integer(locations_count))) {

  location <- locations %>%
    mutate(row_num = row_number()) %>%
    filter(row_num == i) %>%
    collect() %>%
    as.data.frame()

  message(paste0("Processing location: ", location$Istasyon_modified))

  daily_data_file <- find_best_file(raw_data_dir, location$Sehir, location$Istasyon_modified, "gunluk_detay")
  daily_summary_file <- find_best_file(raw_data_dir, location$Sehir, location$Istasyon_modified, "gunluk_ozet")
  hourly_data_file <- find_best_file(raw_data_dir, location$Sehir, location$Istasyon_modified, "saatlik_detay")
  hourly_summary_file <- find_best_file(raw_data_dir, location$Sehir, location$Istasyon_modified, "saatlik_ozet")
  
  if (!file.exists(daily_summary_file)) {
    log_to_db("ERROR", location$Istasyon_modified, paste0("Daily summary file does not exist: ", daily_summary_file))

    closest_file <- find_closest_file(basename(daily_summary_file), dirname(daily_summary_file))
    if (nchar(closest_file) > 0) {
      log_to_db("WARN", location$Istasyon_modified, paste0("Closest daily summary file found: ", closest_file))
    }
  } else {
    report[report$station == location$Istasyon_modified, "daily_summary_file"] <- daily_summary_file

    suppressMessages(daily_summary <- read_excel(daily_summary_file))

    second_row <- daily_summary[2, ]
    second_element <- second_row[2]

    if (second_element != location$Istasyon_original) {
      log_to_db("ERROR", location$Istasyon_modified, paste0("Daily summary file station name mismatch: ", daily_summary_file))
    } else {
      report[report$station == location$Istasyon_modified, "daily_summary_file_station_name_match"] <- 1

      total_daily_data_count <- daily_summary %>%
        filter(Parametre == parameters[1]) %>%
        select("Olması Gereken Veri") %>%
        pull()

      daily_summary_metrics <- daily_summary %>%
        filter(!is.na(Parametre)) %>%
        select("Parametre", "Veri Adeti") %>%
        mutate(Parametre = gsub(" ", "", Parametre)) %>%
        mutate(Parametre = gsub("\\.", "", Parametre))
    }
  }

  if (!file.exists(daily_data_file)) {
    log_to_db("ERROR", location$Istasyon_modified, paste0("Daily data file does not exist: ", daily_data_file))

    closest_file <- find_closest_file(basename(daily_data_file), dirname(daily_data_file))
    if (nchar(closest_file) > 0) {
      log_to_db("WARN", location$Istasyon_modified, paste0("Closest daily data file found: ", closest_file))
    }
  } else {
    report[report$station == location$Istasyon_modified, "daily_data_file"] <- daily_data_file

    date_range <- extract_date_range(daily_data_file)
    
    suppressMessages(daily_data <- read_excel(daily_data_file))

    columns <- colnames(daily_data)
    second_column <- columns[2]

    if (second_column != location$Istasyon_original) {
      log_to_db("ERROR", location$Istasyon_modified, paste0("Daily data file station name mismatch: ", daily_data_file))
    } else {
      report[report$station == location$Istasyon_modified, "daily_data_file_station_name_match"] <- 1
      daily_data <- daily_data %>% filter(!is.na(Tarih))
      daily_detail_for_location <- daily_detail %>% 
        filter(sql(sprintf('CAST(location_id AS TEXT) = %s', dbQuoteString(con, as.character(location$Id))))) %>%
        filter(sql(sprintf('"Tarih" >= %s AND "Tarih" <= %s', 
                           dbQuoteString(con, as.character(date_range$start_date)),
                           dbQuoteString(con, as.character(date_range$end_date)))))
      daily_detail_for_location_duplicates <- daily_detail_for_location %>% collect() %>% as.data.frame() %>% filter(duplicated(Tarih))
      if (nrow(daily_detail_for_location_duplicates) > 0) {
        log_to_db("WARN", location$Istasyon_modified, "Daily data file contains duplicate data.")
      }
      daily_detail_for_location_count <- daily_detail_for_location %>% tally() %>% pull(n)
      dq <- tidy_air_quality_data(daily_detail_for_location, "daily_detail", FALSE, con, TRUE)
      report[report$station == location$Istasyon_modified, "daily_negative_values"] <- dq$negative_values
      report[report$station == location$Istasyon_modified, "daily_invalid_pm"] <- dq$invalid_pm
      log_to_db("INFO", location$Istasyon_modified, sprintf("Processing %.0f daily records", dq$cleaned_count), 
                location$Id)  

      if (nrow(daily_data) != daily_detail_for_location_count && (nrow(daily_data) != total_daily_data_count)) {
        log_to_db("ERROR", location$Istasyon_modified, "Daily data file row count mismatch.")
      } else {
        report[report$station == location$Istasyon_modified, "daily_data_file_row_count_match"] <- 1
      }

      for (parameter in parameters) {
        daily_data_for_parameter <- daily_detail_for_location %>%
          summarise(count = sql(sprintf('COUNT(CASE WHEN "%s" IS NOT NULL THEN 1 END)', parameter))) %>%
          collect() %>%
          pull(count)

        parameter_data_count <- daily_summary_metrics %>%
          filter(Parametre == parameter) %>%
          select("Veri Adeti") %>%
          pull()

        if (daily_data_for_parameter != parameter_data_count) {
          log_to_db("WARN", location$Istasyon_modified, paste0("Daily data file parameter data count mismatch: ", parameter, ' | ', parameter_data_count, '/', daily_data_for_parameter))
        } else {
          report[report$station == location$Istasyon_modified, paste0("daily_data_file_", parameter, "_data_count_match")] <- 1
        }
      }
      
      previous_cleanings <- tryCatch({
        dbGetQuery(con, sprintf("
          SELECT 
            MAX(timestamp) as last_cleaned_at,
            COUNT(*)::integer as clean_operations,
            SUM(CASE WHEN CAST(details->>'field_name' AS TEXT) = 'PM10,PM25' THEN CAST(details->>'affected_rows' AS INTEGER) ELSE 0 END)::integer as pm_fixes,
            SUM(CASE WHEN CAST(details->>'reason' AS TEXT) = 'negative_values' THEN CAST(details->>'affected_rows' AS INTEGER) ELSE 0 END)::integer as negative_fixes,
            SUM(CASE WHEN CAST(details->>'reason' AS TEXT) = 'invalid_time_format' THEN CAST(details->>'affected_rows' AS INTEGER) ELSE 0 END)::integer as time_fixes
          FROM data_cleaning_log
          WHERE location_id = %s 
          AND category = 'DATA_QUALITY'
          AND CAST(details->>'table_name' AS TEXT) = 'daily_detail'
          AND CAST(details->>'operation_type' AS TEXT) = 'nullify'
          GROUP BY location_id",
          dbQuoteString(con, as.character(location$Id))
        ))
      }, error = function(e) {
        log_to_db("WARN", location$Istasyon_modified, sprintf("Failed to query previous cleanings: %s", e$message))
        return(data.frame())
      })
      
      if (nrow(previous_cleanings) > 0 && previous_cleanings$clean_operations > 0) {
        tryCatch({
          last_cleaned <- format(previous_cleanings$last_cleaned_at, "%Y-%m-%d %H:%M:%S")
          
          log_to_db("INFO", location$Istasyon_modified, sprintf(
            "Previously cleaned daily data: %i total operations on %s (PM fixes: %i, Negative values: %i)",
            as.integer(previous_cleanings$clean_operations), 
            last_cleaned,
            as.integer(previous_cleanings$pm_fixes),
            as.integer(previous_cleanings$negative_fixes)
          ), location$Id) 
          
          report[report$station == location$Istasyon_modified, "daily_negative_values"] <- 
            report[report$station == location$Istasyon_modified, "daily_negative_values"] + as.integer(previous_cleanings$negative_fixes)
          report[report$station == location$Istasyon_modified, "daily_invalid_pm"] <- 
            report[report$station == location$Istasyon_modified, "daily_invalid_pm"] + as.integer(previous_cleanings$pm_fixes)
        }, error = function(e) {
          log_to_db("WARN", location$Istasyon_modified, sprintf("Error processing previous cleaning data: %s", e$message))
        })
      }
    }
  }

  if (!file.exists(hourly_summary_file)) {
    log_to_db("ERROR", location$Istasyon_modified, paste0("Hourly summary file does not exist: ", hourly_summary_file))

    closest_file <- find_closest_file(basename(hourly_summary_file), dirname(hourly_summary_file))
    if (nchar(closest_file) > 0) {
      log_to_db("WARN", location$Istasyon_modified, paste0("Closest hourly summary file found: ", closest_file))
    }
  } else {
    report[report$station == location$Istasyon_modified, "hourly_summary_file"] <- hourly_summary_file

    suppressMessages(hourly_summary <- read_excel(hourly_summary_file))

    second_row <- hourly_summary[2, ]
    second_element <- second_row[2]

    if (second_element != location$Istasyon_original) {
      log_to_db("ERROR", location$Istasyon_modified, paste0("Hourly summary file station name mismatch: ", hourly_summary_file))
    } else {
      report[report$station == location$Istasyon_modified, "hourly_summary_file_station_name_match"] <- 1

      total_hourly_data_count <- hourly_summary %>%
        filter(Parametre == parameters[1]) %>%
        select("Olması Gereken Veri") %>%
        pull()

      hourly_summary_metrics <- hourly_summary %>%
        filter(!is.na(Parametre)) %>%
        select("Parametre", "Veri Adeti") %>%
        mutate(Parametre = gsub(" ", "", Parametre)) %>%
        mutate(Parametre = gsub("\\.", "", Parametre))
    }
  }

  if (!file.exists(hourly_data_file)) {
    log_to_db("ERROR", location$Istasyon_modified, paste0("Hourly data file does not exist: ", hourly_data_file))

    closest_file <- find_closest_file(basename(hourly_data_file), dirname(hourly_data_file))
    if (nchar(closest_file) > 0) {
      log_to_db("WARN", location$Istasyon_modified, paste0("Closest hourly data file found: ", closest_file))
    }
  } else {
    report[report$station == location$Istasyon_modified, "hourly_data_file"] <- hourly_data_file

    hourly_date_range <- extract_date_range(hourly_data_file)
    
    suppressMessages(hourly_data <- read_excel(hourly_data_file))

    columns <- colnames(hourly_data)
    second_column <- columns[2]

    if (second_column != location$Istasyon_original) {
      log_to_db("ERROR", location$Istasyon_modified, paste0("Hourly data file station name mismatch: ", hourly_data_file))
    } else {
      report[report$station == location$Istasyon_modified, "hourly_data_file_station_name_match"] <- 1

      hourly_data <- hourly_data %>%
        filter(!is.na(Tarih))

      hourly_detail_for_location <- hourly_detail %>%
        filter(sql(sprintf('CAST(location_id AS TEXT) = %s', dbQuoteString(con, as.character(location$Id))))) %>%
        filter(sql(sprintf('"Tarih" >= %s AND "Tarih" <= %s', 
                           dbQuoteString(con, as.character(hourly_date_range$start_date)),
                           dbQuoteString(con, as.character(hourly_date_range$end_date)))))

      hourly_detail_for_location_count <- hourly_detail_for_location %>% 
        tally() %>% 
        pull(n)

      hourly_detail_for_location_duplicates <- hourly_detail_for_location %>%
        collect() %>%
        as.data.frame() %>%
        filter(duplicated(Tarih))

      if (nrow(hourly_detail_for_location_duplicates) > 0) {
        log_to_db("WARN", location$Istasyon_modified, "Daily data file contains duplicate data.")
      }

      if (nrow(hourly_data) != hourly_detail_for_location_count && (nrow(hourly_data) != total_hourly_data_count)) {
        log_to_db("ERROR", location$Istasyon_modified, "Hourly data file row count mismatch.")
      } else {
        report[report$station == location$Istasyon_modified, "hourly_data_file_row_count_match"] <- 1
      }

      hourly_detail_for_location_count <- hourly_detail_for_location %>% tally() %>% pull(n)
      
      if (hourly_detail_for_location_count == 0) {
        log_to_db("WARN", location$Istasyon_modified, sprintf("No hourly data found in database"), location$Id)
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
        report[report$station == location$Istasyon_modified, "hourly_wrong_time"] <- hourly_quality_check$wrong_time
        
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
        if (hourly_quality_check$wrong_time > 0) {
          log_to_db("WARN", location$Istasyon_modified, sprintf("Hourly data: %.0f records (%.2f%%) with incorrect time format will be removed", 
                          hourly_quality_check$wrong_time,
                          100 * hourly_quality_check$wrong_time / hourly_quality_check$cleaned_count))
        }
      }

      for (parameter in parameters) {
        hourly_data_for_parameter <- hourly_detail_for_location %>%
          summarise(count = sql(sprintf('COUNT(CASE WHEN "%s" IS NOT NULL THEN 1 END)', parameter))) %>%
          collect() %>%
          pull(count)

        parameter_data_count <- hourly_summary_metrics %>%
          filter(Parametre == parameter) %>%
          select("Veri Adeti") %>%
          pull()

        if (hourly_data_for_parameter != parameter_data_count) {
          log_to_db("WARN", location$Istasyon_modified, paste0("Hourly data file parameter data count mismatch: ", parameter, ' | ', parameter_data_count, '/', hourly_data_for_parameter))
        } else {
          report[report$station == location$Istasyon_modified, paste0("hourly_data_file_", parameter, "_data_count_match")] <- 1
        }
      }
      
      previous_hourly_cleanings <- tryCatch({
        dbGetQuery(con, sprintf("
          SELECT 
            MAX(timestamp) as last_cleaned_at,
            COUNT(*)::integer as clean_operations,
            SUM(CASE WHEN CAST(details->>'field_name' AS TEXT) = 'PM10,PM25' THEN CAST(details->>'affected_rows' AS INTEGER) ELSE 0 END)::integer as pm_fixes,
            SUM(CASE WHEN CAST(details->>'reason' AS TEXT) = 'negative_values' THEN CAST(details->>'affected_rows' AS INTEGER) ELSE 0 END)::integer as negative_fixes,
            SUM(CASE WHEN CAST(details->>'field_name' AS TEXT) = 'all' AND CAST(details->>'reason' AS TEXT) = 'invalid_time_format' THEN CAST(details->>'affected_rows' AS INTEGER) ELSE 0 END)::integer as time_fixes
          FROM data_cleaning_log
          WHERE location_id = %s 
          AND category = 'DATA_QUALITY'
          AND CAST(details->>'table_name' AS TEXT) = 'hourly_detail'
          AND CAST(details->>'operation_type' AS TEXT) = 'nullify'
          GROUP BY location_id",
          dbQuoteString(con, as.character(location$Id))
        ))
      }, error = function(e) {
        log_to_db("WARN", location$Istasyon_modified, sprintf("Failed to query previous hourly cleanings: %s", e$message))
        return(data.frame())
      })
      
      if (nrow(previous_hourly_cleanings) > 0 && previous_hourly_cleanings$clean_operations > 0) {
        tryCatch({
          last_cleaned <- format(previous_hourly_cleanings$last_cleaned_at, "%Y-%m-%d %H:%M:%S")
          
          log_to_db("INFO", location$Istasyon_modified, sprintf(
            "Previously cleaned hourly data: %i total operations on %s (PM fixes: %i, Negative values: %i, Time format fixes: %i)",
            as.integer(previous_hourly_cleanings$clean_operations), 
            last_cleaned,
            as.integer(previous_hourly_cleanings$pm_fixes),
            as.integer(previous_hourly_cleanings$negative_fixes),
            as.integer(previous_hourly_cleanings$time_fixes)
          ))
          
          report[report$station == location$Istasyon_modified, "hourly_negative_values"] <- 
            report[report$station == location$Istasyon_modified, "hourly_negative_values"] + as.integer(previous_hourly_cleanings$negative_fixes)
          report[report$station == location$Istasyon_modified, "hourly_invalid_pm"] <- 
            report[report$station == location$Istasyon_modified, "hourly_invalid_pm"] + as.integer(previous_hourly_cleanings$pm_fixes)
          report[report$station == location$Istasyon_modified, "hourly_wrong_time"] <- 
            report[report$station == location$Istasyon_modified, "hourly_wrong_time"] + as.integer(previous_hourly_cleanings$time_fixes)
        }, error = function(e) {
          log_to_db("WARN", location$Istasyon_modified, sprintf("Error processing previous hourly cleaning data: %s", e$message))
        })
      }
    }
  }
}

write_xlsx(report, report_file)

dbDisconnect(con)

log_to_db("INFO", "system", "Data checks completed successfully", NULL, "system")

message("Data checks completed successfully")



