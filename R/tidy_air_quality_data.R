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
  
  # Enhanced error handling function
  log_error <- function(message, error = NULL) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    error_msg <- if(!is.null(error)) paste(error$message, "Call:", error$call) else ""
    cat(sprintf("[%s] ERROR: %s %s\n", timestamp, message, error_msg))
  }
  
  tryCatch({
    # Get initial count using simpler query
    original_count <- as.numeric(tbl_db %>% 
      summarise(n = sql('COUNT(1)')) %>% 
      collect() %>% 
      pull(n))
    
    cat("Original record count:", original_count, "\n")
    
    if (original_count == 0) {
      return(list(
        data = tbl_db,
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
    
    # Create a temporary table for step-by-step cleaning
    temp_table <- paste0("temp_", random_string("", 10))
    
    cat("Creating temporary table for cleaning...\n")
    dbExecute(con, sprintf('CREATE TEMPORARY TABLE %s AS SELECT * FROM %s', 
                          temp_table, 
                          table_name))
    
    # Step 1: Check and count duplicates BEFORE removing them
    cat("Step 1: Checking for duplicates...\n")
    exact_dupes_sql <- sprintf("
      SELECT COUNT(*) as n FROM (
        SELECT COUNT(*) as cnt
        FROM %s
        GROUP BY \"Id\", \"location_id\", \"Tarih\", \"PM10\", \"PM25\", \"SO2\", \"CO\", \"NO2\", \"NOX\", \"NO\", \"O3\"
        HAVING COUNT(*) > 1
      ) t", temp_table)
    
    exact_duplicates <- as.numeric(dbGetQuery(con, exact_dupes_sql)$n)
    
    time_dupes_sql <- sprintf("
      WITH LocationTimeDupes AS (
        SELECT \"Tarih\", \"location_id\", COUNT(*) as cnt
        FROM %s
        GROUP BY \"Tarih\", \"location_id\"
        HAVING COUNT(*) > 1
      )
      SELECT COUNT(*) as n FROM LocationTimeDupes", temp_table)
    
    time_duplicates <- as.numeric(dbGetQuery(con, time_dupes_sql)$n)
    
    # Remove duplicates - keep the record with the smallest Id
    if (time_duplicates > 0) {
      cat("Removing", time_duplicates, "duplicate records...\n")
      dedup_sql <- sprintf("
        DELETE FROM %s
        WHERE ctid NOT IN (
          SELECT MIN(ctid)
          FROM %s
          GROUP BY \"Tarih\", \"location_id\"
        )", temp_table, temp_table)
      
      dbExecute(con, dedup_sql)
    }
    
    # Step 2: Count and fix negative values
    cat("Step 2: Checking for negative values...\n")
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
    
    negative_values <- as.numeric(dbGetQuery(con, negative_sql)$n)
    
    # Fix negative values by setting them to NULL
    if (negative_values > 0) {
      cat("Setting", negative_values, "negative values to NULL...\n")
      filter_sql_negative <- sprintf("
        UPDATE %s
        SET 
          \"PM10\" = CASE WHEN \"PM10\" < 0 THEN NULL ELSE \"PM10\" END,
          \"PM25\" = CASE WHEN \"PM25\" < 0 THEN NULL ELSE \"PM25\" END,
          \"SO2\" = CASE WHEN \"SO2\" < 0 THEN NULL ELSE \"SO2\" END,
          \"NO2\" = CASE WHEN \"NO2\" < 0 THEN NULL ELSE \"NO2\" END,
          \"O3\" = CASE WHEN \"O3\" < 0 THEN NULL ELSE \"O3\" END,
          \"CO\" = CASE WHEN \"CO\" < 0 THEN NULL ELSE \"CO\" END,
          \"NO\" = CASE WHEN \"NO\" < 0 THEN NULL ELSE \"NO\" END,
          \"NOX\" = CASE WHEN \"NOX\" < 0 THEN NULL ELSE \"NOX\" END", 
        temp_table)
      
      dbExecute(con, filter_sql_negative)
    }
    
    # Step 3: Check and fix PM relationships
    cat("Step 3: Checking for invalid PM relationships...\n")
    invalid_pm_sql <- sprintf("
      SELECT COUNT(*) as n
      FROM %s
      WHERE \"PM25\" > \"PM10\" 
        AND \"PM25\" IS NOT NULL 
        AND \"PM10\" IS NOT NULL", temp_table)
    
    invalid_pm <- as.numeric(dbGetQuery(con, invalid_pm_sql)$n)
    
    # Fix invalid PM relationships
    if (invalid_pm > 0) {
      cat("Setting", invalid_pm, "invalid PM relationships to NULL...\n")
      filter_sql_pm <- sprintf("
        UPDATE %s
        SET 
          \"PM10\" = NULL,
          \"PM25\" = NULL
        WHERE \"PM25\" > \"PM10\" 
          AND \"PM25\" IS NOT NULL 
          AND \"PM10\" IS NOT NULL", 
        temp_table)
      
      dbExecute(con, filter_sql_pm)
    }
    
    # Step 4: Check and fix NOx relationships  
    cat("Step 4: Checking for invalid NOx relationships...\n")
    invalid_nox_sql <- sprintf("
      SELECT COUNT(*) as n
      FROM %s
      WHERE \"NO\" IS NOT NULL 
        AND \"NO2\" IS NOT NULL 
        AND \"NOX\" IS NOT NULL
        AND (\"NO\" + \"NO2\") NOT BETWEEN \"NOX\" * 0.5 AND \"NOX\" * 1.5", 
      temp_table)
    
    invalid_nox <- as.numeric(dbGetQuery(con, invalid_nox_sql)$n)
    
    # Fix invalid NOx relationships
    if (invalid_nox > 0) {
      cat("Setting", invalid_nox, "invalid NOx relationships to NULL...\n")
      filter_sql_nox <- sprintf("
        UPDATE %s
        SET 
          \"NO\" = NULL,
          \"NO2\" = NULL,
          \"NOX\" = NULL
        WHERE \"NO\" IS NOT NULL 
          AND \"NO2\" IS NOT NULL 
          AND \"NOX\" IS NOT NULL
          AND (\"NO\" + \"NO2\") NOT BETWEEN \"NOX\" * 0.5 AND \"NOX\" * 1.5", 
        temp_table)
      
      dbExecute(con, filter_sql_nox)
    }
    
    # Step 5: Handle hourly data time format if needed
    wrong_time <- 0
    if (check_hourly) {
      cat("Step 5: Checking hourly data time format...\n")
      wrong_time_sql <- sprintf("
        SELECT COUNT(*) as n
        FROM %s
        WHERE EXTRACT(MINUTE FROM \"Tarih\") != 0 
           OR EXTRACT(SECOND FROM \"Tarih\") != 56", 
        temp_table)
      
      wrong_time <- as.numeric(dbGetQuery(con, wrong_time_sql)$n)
      
      # Remove records with wrong time format for hourly data
      if (wrong_time > 0) {
        cat("Removing", wrong_time, "records with incorrect time format...\n")
        dbExecute(con, sprintf("
          DELETE FROM %s
          WHERE EXTRACT(MINUTE FROM \"Tarih\") != 0 
             OR EXTRACT(SECOND FROM \"Tarih\") != 56", 
          temp_table))
      }
    }
    
    # Get final cleaned data count
    cleaned_count_sql <- sprintf("SELECT COUNT(*) as n FROM %s", temp_table)
    cleaned_count <- as.numeric(dbGetQuery(con, cleaned_count_sql)$n)
    
    # Return the temp table name instead of dplyr object to avoid collection issues
    cleaned_data <- temp_table
    
    cleanup_temp_table <- function() {
      tryCatch({
        dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", temp_table))
      }, error = function(e) {
        warning("Failed to clean up temporary table:", e$message)
      })
    }
    
    return(list(
      data = cleaned_data,
      temp_table_name = temp_table,  
      original_count = original_count,
      cleaned_count = cleaned_count,
      exact_duplicates = exact_duplicates,
      time_duplicates = time_duplicates,
      negative_values = negative_values,
      invalid_pm = invalid_pm,
      invalid_nox = invalid_nox,
      wrong_time = wrong_time,
      cleanup_function = cleanup_temp_table
    ))
    
  }, error = function(e) {
    log_error("Error in tidy_air_quality_data", e)
    stop(e$message)
  })
}

random_string <- function(prefix = "", n = 10) {
  paste0(prefix, paste0(sample(c(letters, 0:9), n, replace = TRUE), collapse = ""))
}
