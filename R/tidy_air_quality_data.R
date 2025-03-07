#' Clean air quality data by applying various quality checks
#' 
#' @param tbl_db Database table reference
#' @param table_name Name of the table
#' @param check_hourly Whether to check hourly data format
#' @param con Database connection
#' @return List containing cleaning results and statistics
#' @export
tidy_air_quality_data <- function(tbl_db, table_name, check_hourly = FALSE, con = NULL) {
  if (is.null(con)) {
    con <- get("con", envir = parent.frame())
  }

  tryCatch({
    # Check if there's any data for this query first
    data_exists <- as.numeric(tbl_db %>% 
      summarise(n = sql('COUNT(1)')) %>% 
      collect() %>% 
      pull(n))
    
    if (data_exists == 0) {
      # Return early with zeros
      return(list(
        data = NULL,
        original_count = 0,
        cleaned_count = 0,
        exact_duplicates = 0,
        time_duplicates = 0,
        negative_values = 0,
        invalid_pm = 0,
        invalid_nox = 0,
        wrong_time = 0
      ))
    }

    # Create temporary table with filtered data
    temp_table <- unique(random_string("temp_", 10))
    
    # Convert dplyr query to SQL properly
    query <- dbplyr::sql_render(tbl_db)
    
    # Create the temporary table safely
    create_temp_sql <- sprintf('CREATE TEMPORARY TABLE %s AS %s',
                             dbQuoteIdentifier(con, temp_table),
                             query)
    
    tryCatch({
      dbExecute(con, create_temp_sql)
    }, error = function(e) {
      stop(paste("Failed to create temporary table:", e$message))
    })

    # Verify table creation and get count
    original_count <- tryCatch({
      as.numeric(dbGetQuery(con, sprintf('SELECT COUNT(1) as n FROM %s', dbQuoteIdentifier(con, temp_table)))$n)
    }, error = function(e) {
      dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", dbQuoteIdentifier(con, temp_table)))
      stop(paste("Failed to query temporary table:", e$message))
    })

    # Only continue with checks if we have data in the temp table
    if (original_count == 0) {
      # Clean up and return zeros
      dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", temp_table))
      return(list(
        data = NULL,
        original_count = 0,
        cleaned_count = 0,
        exact_duplicates = 0,
        time_duplicates = 0,
        negative_values = 0,
        invalid_pm = 0,
        invalid_nox = 0,
        wrong_time = 0
      ))
    }

    # Count exact duplicates
    exact_dupes_sql <- sprintf("
      SELECT COUNT(*) as n FROM (
        SELECT COUNT(*) as cnt
        FROM %s
        GROUP BY \"Id\", \"location_id\", \"Tarih\", \"PM10\", \"PM25\", \"SO2\", \"CO\", \"NO2\", \"NOX\", \"NO\", \"O3\"
        HAVING COUNT(*) > 1
      ) t", temp_table)
    
    exact_duplicates <- as.numeric(dbGetQuery(con, exact_dupes_sql)$n)
    
    # Count timestamp duplicates
    time_dupes_sql <- sprintf("
      WITH LocationTimeDupes AS (
        SELECT \"Tarih\", \"location_id\", COUNT(*) as cnt
        FROM %s
        GROUP BY \"Tarih\", \"location_id\"
        HAVING COUNT(*) > 1
      )
      SELECT COUNT(*) as n FROM LocationTimeDupes", temp_table)
    
    time_duplicates <- as.numeric(dbGetQuery(con, time_dupes_sql)$n)
    
    # Remove only duplicates
    dbExecute(con, sprintf("
      DELETE FROM %s
      WHERE ctid NOT IN (
        SELECT MIN(ctid)
        FROM %s
        GROUP BY \"Tarih\", \"location_id\"
      )", temp_table, temp_table))
    
    # Count negative values
    negative_sql <- sprintf("
      SELECT COUNT(*) as n
      FROM %s
      WHERE (\"PM10\" < 0 AND \"PM10\" IS NOT NULL)
         OR (\"PM25\" < 0 AND \"PM25\" IS NOT NULL)
         OR (\"SO2\" < 0 AND \"SO2\" IS NOT NULL)
         OR (\"NO2\" < 0 AND \"NO2\" IS NOT NULL)
         OR (\"O3\" < 0 AND \"O3\" IS NOT NULL)
         OR (\"CO\" < 0 AND \"CO\" IS NOT NULL)
         OR (\"NO\" < 0 AND \"NO\" IS NOT NULL)
         OR (\"NOX\" < 0 AND \"NOX\" IS NOT NULL)", temp_table)
    
    negative_count_sql <- as.numeric(dbGetQuery(con, negative_sql)$n)
    
    # Check PM relationships
    invalid_pm_sql <- sprintf("
      SELECT COUNT(*) as n
      FROM %s
      WHERE \"PM25\" > \"PM10\" 
        AND \"PM25\" IS NOT NULL 
        AND \"PM10\" IS NOT NULL", temp_table)
    
    invalid_pm <- as.numeric(dbGetQuery(con, invalid_pm_sql)$n)
    
    # Check NOx relationships
    invalid_nox_sql <- sprintf("
      SELECT COUNT(*) as n
      FROM %s
      WHERE \"NO\" IS NOT NULL 
        AND \"NO2\" IS NOT NULL 
        AND \"NOX\" IS NOT NULL
        AND (\"NO\" + \"NO2\") NOT BETWEEN \"NOX\" * 0.5 AND \"NOX\" * 1.5", temp_table)
    
    invalid_nox <- as.numeric(dbGetQuery(con, invalid_nox_sql)$n)
    
    # Get counts before creating logs
    negative_values <- negative_count_sql
    
    # Create enhanced log tables - moved before any logging attempts
    tryCatch({
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS data_cleaning_log (
          id SERIAL PRIMARY KEY,
          session_id TEXT,
          timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          script_name TEXT,
          log_level TEXT,
          category TEXT,
          location_id TEXT,
          station_name TEXT,
          message TEXT,
          details JSONB,
          created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )")
      
      dbExecute(con, "
        CREATE TABLE IF NOT EXISTS data_quality_log (
          id SERIAL PRIMARY KEY,
          table_name TEXT,
          location_id TEXT,
          operation_type TEXT,
          field_name TEXT,
          affected_rows INTEGER,
          reason TEXT,
          old_value TEXT,
          new_value TEXT,
          operation_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )")
      
    }, error = function(e) {
      warning("Failed to create log tables: ", e$message)
      return(NULL)
    })

    # Generate unique session ID for this cleaning run
    session_id <- paste0("CLEAN_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", random_string("", 6))

    # Enhanced logging function
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

      # Database logging
      tryCatch({
        sql <- sprintf("
          INSERT INTO data_cleaning_log 
            (session_id, script_name, log_level, category, location_id, station_name, message, details)
          VALUES 
            (%s, %s, %s, %s, %s, %s, %s, %s::jsonb)",
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

      # Console output
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      cat(sprintf("[%s] %s | %s | %s\n", 
                timestamp, 
                level, 
                ifelse(is.null(station_name), category, station_name),
                message))
    }

    # Quality check logging with correct argument order and variable scoping
    if (negative_values > 0) {
      log_to_db(
        sprintf("Found %d records with negative values", negative_values),
        "WARN",
        "QUALITY_CHECK",
        location_id = tbl_db %>% pull(location_id) %>% unique() %>% as.character(),
        station_name = tbl_db %>% pull(Istasyon_modified) %>% unique() %>% as.character(),
        details = list(
          check_type = "negative_values",
          affected_count = negative_values,
          table_name = table_name
        )
      )
    }

    log_quality_operation <- function(operation_type, field_name, affected_rows, reason, old_value = NULL, new_value = NULL) {
      dbExecute(con, sprintf("
        INSERT INTO data_quality_log (table_name, location_id, operation_type, field_name, affected_rows, reason, old_value, new_value)
        VALUES (%s, %s, %s, %s, %d, %s, %s, %s)",
        dbQuoteString(con, table_name),
        dbQuoteString(con, tbl_db %>% pull(location_id) %>% unique() %>% as.character()),
        dbQuoteString(con, operation_type),
        dbQuoteString(con, field_name),
        affected_rows,
        dbQuoteString(con, reason),
        ifelse(is.null(old_value), "NULL", dbQuoteString(con, old_value)),
        ifelse(is.null(new_value), "NULL", dbQuoteString(con, new_value))
      ))
      
      # Use log_to_db instead of log_operation
      log_to_db(
        sprintf("Quality check: %s %s records affected", operation_type, affected_rows),
        "INFO",
        "QUALITY_CHECK",
        tbl_db %>% pull(location_id) %>% unique() %>% as.character(),
        NULL,
        list(
          field = field_name,
          reason = reason,
          affected_rows = affected_rows
        )
      )
    }

    for (field in c("PM10", "PM25", "SO2", "NO2", "O3", "CO", "NO", "NOX")) {
      affected_rows <- as.numeric(dbGetQuery(con, sprintf("
        SELECT COUNT(*) as count FROM %s WHERE \"%s\" < 0 AND \"%s\" IS NOT NULL",
        temp_table, field, field))$count)
      
      if (affected_rows > 0) {
        log_quality_operation(
          "nullify", 
          field, 
          affected_rows,
          "negative_values",
          "< 0",
          "NULL"
        )
        
        dbExecute(con, sprintf("
          UPDATE %s SET \"%s\" = NULL WHERE \"%s\" < 0",
          temp_table, field, field))
      }
    }

    affected_rows <- invalid_pm
    if (affected_rows > 0) {
      log_quality_operation(
        "nullify",
        "PM10,PM25",
        affected_rows,
        "invalid_pm_relationship",
        "PM2.5 > PM10",
        "NULL"
      )
      
      dbExecute(con, sprintf("
        UPDATE %s 
        SET \"PM10\" = NULL, \"PM25\" = NULL 
        WHERE \"PM25\" > \"PM10\" AND \"PM25\" IS NOT NULL AND \"PM10\" IS NOT NULL",
        temp_table))
    }

    affected_rows <- invalid_nox
    if (affected_rows > 0) {
      log_quality_operation(
        "nullify",
        "NO,NO2,NOX",
        affected_rows,
        "invalid_nox_relationship",
        "NO+NO2 outside NOx bounds",
        "NULL"
      )
      
      dbExecute(con, sprintf("
        UPDATE %s 
        SET \"NO\" = NULL, \"NO2\" = NULL, \"NOX\" = NULL
        WHERE \"NO\" IS NOT NULL AND \"NO2\" IS NOT NULL AND \"NOX\" IS NOT NULL
        AND (\"NO\" + \"NO2\") NOT BETWEEN \"NOX\" * 0.5 AND \"NOX\" * 1.5",
        temp_table))
    }

    wrong_time <- 0
    if (check_hourly) {
      wrong_time_sql <- sprintf("
        SELECT COUNT(*) as n
        FROM %s
        WHERE EXTRACT(MINUTE FROM \"Tarih\") != 0 
           OR EXTRACT(SECOND FROM \"Tarih\") != 56", temp_table)
      
      wrong_time <- as.numeric(dbGetQuery(con, wrong_time_sql)$n)
      
      affected_rows <- wrong_time
      if (affected_rows > 0) {
        log_quality_operation(
          "nullify",
          "all",
          affected_rows,
          "invalid_time_format",
          "incorrect minutes/seconds",
          "NULL"
        )
        
        dbExecute(con, sprintf("
          UPDATE %s 
          SET \"PM10\" = NULL, \"PM25\" = NULL, \"SO2\" = NULL, \"NO2\" = NULL, 
              \"O3\" = NULL, \"CO\" = NULL, \"NO\" = NULL, \"NOX\" = NULL
          WHERE EXTRACT(MINUTE FROM \"Tarih\") != 0 
             OR EXTRACT(SECOND FROM \"Tarih\") != 56",
          temp_table))
      }
    }

    cleaned_data <- tbl(con, sprintf("%s", temp_table))
    cleaned_count <- as.numeric(dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM %s", temp_table))$n)
    
    on.exit({
      dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", temp_table))
    })
    
    return(list(
      data = cleaned_data,
      original_count = original_count,
      cleaned_count = cleaned_count,
      exact_duplicates = exact_duplicates,
      time_duplicates = time_duplicates,
      negative_values = negative_count_sql,
      invalid_pm = invalid_pm,
      invalid_nox = invalid_nox,
      wrong_time = wrong_time
    ))
    
  }, error = function(e) {
    stop(paste("Error in tidy_air_quality_data:", e$message))
  })
}

#' Generate a random string
#' @param prefix Prefix for the string
#' @param n Length of random part
#' @return Random string
#' @export
random_string <- function(prefix = "", n = 10) {
  paste0(prefix, paste0(sample(c(letters, 0:9), n, replace = TRUE), collapse = ""))
}
