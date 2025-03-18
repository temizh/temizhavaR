#' Tidy air quality data
#' 
#' @param tbl_db A database table to clean.
#' @param table_name The name of the table to clean.
#' @param check_hourly Whether to check for hourly data.
#' @param con A database connection.
#' @param update_original Whether to update the original table.
#' @export
#' @examples
#' tidy_air_quality_data(tbl_db, "table_name")
tidy_air_quality_data <- function(tbl_db, table_name, check_hourly = FALSE, con = NULL, update_original = TRUE) {
  if (is.null(con)) {
    con <- get("con", envir = parent.frame())
  }
  
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
  
  tryCatch({
    data_exists <- as.numeric(tbl_db %>% summarise(n = sql('COUNT(1)')) %>% collect() %>% pull(n))
    if (data_exists == 0) {
      return(list(data = NULL, original_count = 0, cleaned_count = 0, exact_duplicates = 0, time_duplicates = 0, negative_values = 0, invalid_pm = 0, invalid_nox = 0, wrong_time = 0))
    }
    temp_table <- unique(random_string("temp_", 10))
    query <- dbplyr::sql_render(tbl_db)
    create_temp_sql <- sprintf('CREATE TEMPORARY TABLE %s AS (%s)', dbQuoteIdentifier(con, temp_table), query)
    tryCatch({
      dbExecute(con, create_temp_sql)
      dbExecute(con, sprintf('ALTER TABLE %s ADD COLUMN needs_cleaning BOOLEAN', dbQuoteIdentifier(con, temp_table)))
    }, error = function(e) { stop(paste("Failed to create temporary table:", e$message)) })
    original_count <- tryCatch({
      as.numeric(dbGetQuery(con, sprintf('SELECT COUNT(1) as n FROM %s', dbQuoteIdentifier(con, temp_table)))$n)
    }, error = function(e) {
      dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", dbQuoteIdentifier(con, temp_table)))
      stop(paste("Failed to query temporary table:", e$message))
    })
    if (original_count == 0) {
      dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", temp_table))
      return(list(data = NULL, original_count = 0, cleaned_count = 0, exact_duplicates = 0, time_duplicates = 0, negative_values = 0, invalid_pm = 0, invalid_nox = 0, wrong_time = 0))
    }
    needs_cleaning <- function(temp_table) {
      checks <- dbGetQuery(con, sprintf("SELECT (SELECT COUNT(*) FROM %1$s WHERE \"PM25\" > \"PM10\" AND \"PM25\" IS NOT NULL AND \"PM10\" IS NOT NULL) > 0 OR (SELECT COUNT(*) FROM %1$s WHERE (\"NO\" + \"NO2\") NOT BETWEEN \"NOX\" * 0.5 AND \"NOX\" * 1.5 AND \"NO\" IS NOT NULL AND \"NO2\" IS NOT NULL AND \"NOX\" IS NOT NULL) > 0 OR (SELECT COUNT(*) FROM %1$s WHERE (\"PM10\" < 0 AND \"PM10\" IS NOT NULL) OR (\"PM25\" < 0 AND \"PM25\" IS NOT NULL) OR (\"SO2\" < 0 AND \"SO2\" IS NOT NULL) OR (\"NO2\" < 0 AND \"NO2\" IS NOT NULL) OR (\"O3\" < 0 AND \"O3\" IS NOT NULL) OR (\"CO\" < 0 AND \"CO\" IS NOT NULL) OR (\"NO\" < 0 AND \"NO\" IS NOT NULL) OR (\"NOX\" < 0 AND \"NOX\" IS NOT NULL)) > 0 as needs_cleaning", temp_table))
      return(as.logical(checks$needs_cleaning))
    }
    
    should_clean <- needs_cleaning(temp_table)
    
    exact_dupes_sql <- sprintf("SELECT COUNT(*) as n FROM ( SELECT COUNT(*) as cnt FROM %s GROUP BY \"Id\", \"location_id\", \"Tarih\", \"PM10\", \"PM25\", \"SO2\", \"CO\", \"NO2\", \"NOX\", \"NO\", \"O3\" HAVING COUNT(*) > 1 ) t", temp_table)
    exact_duplicates <- as.numeric(dbGetQuery(con, exact_dupes_sql)$n)
    
    time_dupes_sql <- sprintf("WITH LocationTimeDupes AS ( SELECT \"Tarih\", \"location_id\", COUNT(*) as cnt FROM %s GROUP BY \"Tarih\", \"location_id\" HAVING COUNT(*) > 1 ) SELECT COUNT(*) as n FROM LocationTimeDupes", temp_table)
    time_duplicates <- as.numeric(dbGetQuery(con, time_dupes_sql)$n)
    
    dbExecute(con, sprintf("DELETE FROM %s WHERE ctid NOT IN ( SELECT MIN(ctid) FROM %s GROUP BY \"Tarih\", \"location_id\" )", temp_table, temp_table))
    
    negative_sql <- sprintf("SELECT COUNT(*) as n FROM %s WHERE (\"PM10\" < 0 AND \"PM10\" IS NOT NULL) OR (\"PM25\" < 0 AND \"PM25\" IS NOT NULL) OR (\"SO2\" < 0 AND \"SO2\" IS NOT NULL) OR (\"NO2\" < 0 AND \"NO2\" IS NOT NULL) OR (\"O3\" < 0 AND \"O3\" IS NOT NULL) OR (\"CO\" < 0 AND \"CO\" IS NOT NULL) OR (\"NO\" < 0 AND \"NO\" IS NOT NULL) OR (\"NOX\" < 0 AND \"NOX\" IS NOT NULL)", temp_table)
    negative_count_sql <- as.numeric(dbGetQuery(con, negative_sql)$n)
    
    invalid_pm_sql <- sprintf("SELECT COUNT(*) as n FROM %s WHERE \"PM25\" > \"PM10\" AND \"PM25\" IS NOT NULL AND \"PM10\" IS NOT NULL", temp_table)
    invalid_pm <- as.numeric(dbGetQuery(con, invalid_pm_sql)$n)
    
    invalid_nox_sql <- sprintf("SELECT COUNT(*) as n FROM %s WHERE \"NO\" IS NOT NULL AND \"NO2\" IS NOT NULL AND \"NOX\" IS NOT NULL AND (\"NO\" + \"NO2\") NOT BETWEEN \"NOX\" * 0.5 AND \"NOX\" * 1.5", temp_table)
    invalid_nox <- as.numeric(dbGetQuery(con, invalid_nox_sql)$n)
    
    negative_values <- negative_count_sql
    
    tryCatch({
      dbExecute(con, "CREATE TABLE IF NOT EXISTS data_cleaning_log ( id SERIAL PRIMARY KEY, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP, session_id TEXT, timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP, script_name TEXT, log_level TEXT, category TEXT, location_id TEXT, station_name TEXT, message TEXT, details JSONB )")
    }, error = function(e) { warning("Failed to create log table: ", e$message); return(NULL) })
    
    session_id <- paste0("CLEAN_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", random_string("", 6))
    
    log_to_db <- function(message, level, category, location_id = NULL, station_name = NULL, details = NULL) {
      if (is.null(message)) { message <- "No message provided" }
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
        warning("Failed to convert details to JSON: ", e$message); 
        "null" 
      })
      
      tryCatch({
        sql <- sprintf(" INSERT INTO data_cleaning_log (session_id, script_name, log_level, category, location_id, station_name, message, details) VALUES (%s, %s, %s, %s, %s, %s, %s, %s::jsonb)",
                      dbQuoteString(con, session_id),
                      dbQuoteString(con, "tidy_air_quality_data"),
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
      cat(sprintf("[%s] %s | %s | %s\n", timestamp, level, ifelse(is.null(station_name), category, station_name), message))
    }
    
    if (!should_clean) {
      location_id <- tryCatch({
        tbl_db %>% pull(location_id) %>% unique() %>% as.character()
      }, error = function(e) { "Unknown" })
      
      station_name <- tryCatch({
        tbl_db %>% pull(Istasyon_modified) %>% unique() %>% head(1) %>% as.character()
      }, error = function(e) { "Unknown_Station" })
      
      log_to_db("No data quality issues detected before cleaning modifications; proceeding for consistency.", 
               "INFO", "DATA_CHECK", 
               location_id, 
               station_name, 
               NULL)
    }
    
    if (negative_values > 0) {
      location_id <- tryCatch({
        tbl_db %>% pull(location_id) %>% unique() %>% as.character()
      }, error = function(e) {
        "Unknown"
      })
      
      station_name <- tryCatch({
        tbl_db %>% pull(Istasyon_modified) %>% unique() %>% head(1) %>% as.character()
      }, error = function(e) {
        "Unknown_Station"
      })
      
      log_to_db(sprintf("Found %d records with negative values", negative_values), 
                "WARN", 
                "QUALITY_CHECK", 
                location_id, 
                station_name,
                list(check_type = "negative_values", affected_count = negative_values, table_name = table_name))
    }
    
    log_quality_operation <- function(operation_type, field_name, affected_rows, reason, old_value = NULL, new_value = NULL) {
      location_id <- tryCatch({
        tbl_db %>% pull(location_id) %>% unique() %>% head(1) %>% as.character()
      }, error = function(e) {
        "Unknown"
      })
      
      station_name <- tryCatch({
        tbl_db %>% pull(Istasyon_modified) %>% unique() %>% head(1) %>% as.character()
      }, error = function(e) {
        "Unknown_Station"
      })
      
      details <- list(
        table_name = table_name,
        operation_type = operation_type,
        field_name = field_name,
        affected_rows = affected_rows,
        reason = reason,
        old_value = old_value,
        new_value = new_value
      )
      
      tryCatch({
        sql <- sprintf("
          INSERT INTO data_cleaning_log 
            (session_id, script_name, log_level, category, location_id, station_name, message, details)
          VALUES
            (%s, %s, %s, %s, %s, %s, %s, %s::jsonb)",
          dbQuoteString(con, session_id),
          dbQuoteString(con, "tidy_air_quality_data"),
          dbQuoteString(con, "INFO"),
          dbQuoteString(con, "DATA_QUALITY"),
          dbQuoteString(con, location_id),
          dbQuoteString(con, station_name),
          dbQuoteString(con, sprintf("Quality check: %s %s records affected", operation_type, affected_rows)),
          dbQuoteString(con, jsonlite::toJSON(details, auto_unbox = TRUE))
        )
        dbExecute(con, sql)
        
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        cat(sprintf("[%s] %s | %s | %s\n", 
                    timestamp, 
                    "INFO", 
                    ifelse(is.null(station_name), "DATA_QUALITY", station_name), 
                    sprintf("Quality check: %s %s records affected", operation_type, affected_rows)))
      }, error = function(e) { 
        warning("Failed to write to database log: ", e$message) 
      })
    }
    
    for (field in c("PM10","PM25","SO2","NO2","O3","CO","NO","NOX")) {
      affected_rows <- as.numeric(dbGetQuery(con, sprintf(" SELECT COUNT(*) as count FROM %s WHERE \"%s\" < 0 AND \"%s\" IS NOT NULL", temp_table, field, field))$count)
      if (affected_rows > 0) {
        log_quality_operation("nullify", field, affected_rows, "negative_values", "< 0", "NULL")
        dbExecute(con, sprintf(" UPDATE %s SET \"%s\" = NULL WHERE \"%s\" < 0", temp_table, field, field))
      }
    }
    
    affected_rows <- invalid_pm
    if (affected_rows > 0) {
      log_quality_operation("nullify", "PM10,PM25", affected_rows, "invalid_pm_relationship", "PM2.5 > PM10", "NULL")
      dbExecute(con, sprintf(" UPDATE %s SET \"PM10\" = NULL, \"PM25\" = NULL WHERE \"PM25\" > \"PM10\" AND \"PM25\" IS NOT NULL AND \"PM10\" IS NOT NULL", temp_table))
    }
    
    affected_rows <- invalid_nox
    if (affected_rows > 0) {
      log_quality_operation("nullify", "NO,NO2,NOX", affected_rows, "invalid_nox_relationship", "NO+NO2 outside NOx bounds", "NULL")
      dbExecute(con, sprintf(" UPDATE %s SET \"NO\" = NULL, \"NO2\" = NULL, \"NOX\" = NULL WHERE \"NO\" IS NOT NULL AND \"NO2\" IS NOT NULL AND \"NOX\" IS NOT NULL AND (\"NO\" + \"NO2\") NOT BETWEEN \"NOX\" * 0.5 AND \"NOX\" * 1.5", temp_table))
    }
    
    wrong_time <- 0
    if (check_hourly) {
      wrong_time_sql <- sprintf(" SELECT COUNT(*) as n FROM %s WHERE EXTRACT(MINUTE FROM \"Tarih\") != 0 OR EXTRACT(SECOND FROM \"Tarih\") != 56", temp_table)
      wrong_time <- as.numeric(dbGetQuery(con, wrong_time_sql)$n)
      affected_rows <- wrong_time
      if (affected_rows > 0) {
        log_quality_operation("nullify", "all", affected_rows, "invalid_time_format", "incorrect minutes/seconds", "NULL")
        dbExecute(con, sprintf(" UPDATE %s SET \"PM10\" = NULL, \"PM25\" = NULL, \"SO2\" = NULL, \"NO2\" = NULL, \"O3\" = NULL, \"CO\" = NULL, \"NO\" = NULL, \"NOX\" = NULL WHERE EXTRACT(MINUTE FROM \"Tarih\") != 0 OR EXTRACT(SECOND FROM \"Tarih\") != 0", temp_table))
      }
    }
    
    cleaned_count <- as.numeric(dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM %s", temp_table))$n)
    
    if (update_original) {
      location_id <- tbl_db %>% select(location_id) %>% distinct() %>% collect() %>% pull(location_id)
      issues_found <- negative_values + invalid_pm + invalid_nox + wrong_time
      
      existing_backups <- dbGetQuery(con, sprintf(" SELECT table_name, created_at FROM ( SELECT table_name, (regexp_matches(table_name, '%s_backup_(\\d{8}_\\d{6})'))::text[] AS backup_date, to_timestamp(regexp_replace(table_name, '.*_backup_(\\d{8}_\\d{6}).*', '\\1'), 'YYYYMMDD_HH24MISS') as created_at FROM information_schema.tables WHERE table_name LIKE '%s_backup_%%' AND table_schema = 'public' ) AS backups WHERE table_name LIKE '%s_backup_%%' ORDER BY created_at DESC", table_name, table_name, table_name))
      
      has_recent_backup <- FALSE
      backup_table <- NULL
      
      if (nrow(existing_backups) > 0) {
        most_recent_backup <- existing_backups$table_name[1]
        backup_date <- existing_backups$created_at[1]
        today <- as.Date(Sys.time())
        backup_day <- as.Date(backup_date)
        has_recent_backup <- (today == backup_day)
        if (has_recent_backup) { backup_table <- most_recent_backup }
      }
      
      if (issues_found > 0) {
        if (!has_recent_backup) {
          backup_table <- paste0(table_name, "_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
          all_old_backups <- dbGetQuery(con, sprintf(" SELECT table_name FROM information_schema.tables WHERE table_name LIKE '%s_backup_%%' AND table_schema = 'public' ORDER BY table_name", table_name))
          dbExecute(con, sprintf(" CREATE TABLE %s AS SELECT *, %s::boolean as needs_cleaning FROM %s WHERE CAST(location_id AS TEXT) = %s", dbQuoteIdentifier(con, backup_table), dbQuoteString(con, "TRUE"), dbQuoteIdentifier(con, table_name), dbQuoteString(con, as.character(location_id))))
          
          if (nrow(all_old_backups) > 0) {
            for (old_backup in all_old_backups$table_name) {
              tryCatch({
                dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", dbQuoteIdentifier(con, old_backup)))
                station_name <- get_station_name(location_id)
                log_to_db(sprintf("Removed old backup table %s for location %s", old_backup, location_id), 
                         "INFO", "DATA_BACKUP", location_id, station_name, 
                         list(location_id = location_id, removed_backup = old_backup))
              }, error = function(e) {
                station_name <- get_station_name(location_id)
                log_to_db(sprintf("Failed to remove old backup table %s: %s", old_backup, e$message), 
                         "WARN", "DATA_BACKUP", location_id, station_name, 
                         list(location_id = location_id, backup_error = e$message))
              })
            }
          }
          
          log_to_db(sprintf("Created backup of original data for location %s as %s", location_id, backup_table), 
                    "INFO", 
                    "DATA_BACKUP", 
                    location_id, 
                    get_station_name(location_id), 
                    list(location_id = location_id, backup_table = backup_table))
        } else {
          log_to_db(sprintf("Using existing backup %s for location %s from today", backup_table, location_id), 
                    "INFO", 
                    "DATA_BACKUP", 
                    location_id, 
                    get_station_name(location_id), 
                    list(location_id = location_id, backup_table = backup_table))
        }
      } else {
        dbExecute(con, sprintf("ALTER TABLE %s DROP COLUMN IF EXISTS needs_cleaning", dbQuoteIdentifier(con, temp_table)))
        
        previous_cleanings <- dbGetQuery(con, sprintf("
        SELECT 
          MAX(timestamp) as last_cleaned_at,
          COUNT(*) as clean_operations,
          SUM(CASE WHEN CAST(details->>'field_name' AS TEXT) = 'PM10,PM25' THEN CAST(details->>'affected_rows' AS INTEGER) ELSE 0 END) as pm_fixes, 
          SUM(CASE WHEN CAST(details->>'field_name' AS TEXT) = 'NO,NO2,NOX' THEN CAST(details->>'affected_rows' AS INTEGER) ELSE 0 END) as nox_fixes,
          SUM(CASE WHEN CAST(details->>'reason' AS TEXT) = 'negative_values' THEN CAST(details->>'affected_rows' AS INTEGER) ELSE 0 END) as negative_fixes
        FROM data_cleaning_log 
        WHERE location_id = %s 
        AND category = 'DATA_QUALITY' 
        AND CAST(details->>'table_name' AS TEXT) = %s 
        AND CAST(details->>'operation_type' AS TEXT) = 'nullify'
        GROUP BY location_id", 
        dbQuoteString(con, as.character(location_id)), 
        dbQuoteString(con, table_name))
        )
        
        if (nrow(previous_cleanings) > 0 && previous_cleanings$clean_operations[1] > 0) {
          last_cleaned <- format(previous_cleanings$last_cleaned_at[1], "%Y-%m-%d %H:%M:%S")
          station_name <- get_station_name(location_id)
          log_to_db(sprintf("No new issues found for location %s, but previously cleaned data exists. Last cleaned: %s", location_id, last_cleaned), 
                   "INFO", "DATA_HISTORY", location_id, station_name, 
                   list(location_id = location_id, last_cleaned = last_cleaned, pm_fixes = previous_cleanings$pm_fixes[1], 
                        nox_fixes = previous_cleanings$nox_fixes[1], negative_fixes = previous_cleanings$negative_fixes[1], 
                        total_operations = previous_cleanings$clean_operations[1]))
          
          if (previous_cleanings$pm_fixes[1] > 0) {
            log_to_db(sprintf("Previously fixed %.0f records with PM2.5 > PM10 relationship issues", previous_cleanings$pm_fixes[1]), 
                     "INFO", "HISTORICAL_FIXES", location_id, station_name, NULL)
          }
          
          if (previous_cleanings$nox_fixes[1] > 0) {
            log_to_db(sprintf("Previously fixed %.0f records with invalid NOx relationships", previous_cleanings$nox_fixes[1]), 
                     "INFO", "HISTORICAL_FIXES", location_id, station_name, NULL)
          }
          
          if (previous_cleanings$negative_fixes[1] > 0) {
            log_to_db(sprintf("Previously fixed %.0f records with negative values", previous_cleanings$negative_fixes[1]), 
                     "INFO", "HISTORICAL_FIXES", location_id, station_name, NULL)
          }
        } else {
          log_to_db(sprintf("No data quality issues found for location %s. No previous cleaning records found.", location_id), "INFO", "DATA_CHECK", location_id, NULL, NULL)
        }
      }
    }
    
    cleaned_data <- tbl(con, sprintf("%s", temp_table))
    on.exit({ dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", temp_table)) })
    return(list(data = cleaned_data, original_count = original_count, cleaned_count = cleaned_count, exact_duplicates = exact_duplicates, time_duplicates = time_duplicates, negative_values = negative_values, invalid_pm = invalid_pm, invalid_nox = invalid_nox, wrong_time = wrong_time))
  }, error = function(e) { stop(paste("Error in tidy_air_quality_data:", e$message)) })
}

random_string <- function(prefix = "", n = 10) {
  paste0(prefix, paste0(sample(c(letters, 0:9), n, replace = TRUE), collapse = ""))
}
