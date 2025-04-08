library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(temizhavaR)

# Enhanced error handling function
log_error <- function(message, error = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  error_msg <- if(!is.null(error)) paste(error$message, "Call:", error$call) else ""
  cat(sprintf("[%s] ERROR: %s %s\n", timestamp, message, error_msg))
}

# Function to safely check if column exists
column_exists <- function(tbl_db, col_name) {
  tryCatch({
    cols <- colnames(tbl_db %>% head(1) %>% collect())
    return(col_name %in% cols)
  }, error = function(e) {
    log_error(sprintf("Failed to check if column '%s' exists", col_name), e)
    return(FALSE)
  })
}


# tidy_air_quality_data <- function(tbl_db, table_name, check_hourly = FALSE) {
#   tryCatch({
#     # Get initial count using simpler query
#     original_count <- as.numeric(tbl_db %>% 
#       summarise(n = sql('COUNT(1)')) %>% 
#       collect() %>% 
#       pull(n))
    
#     cat("Original record count:", original_count, "\n")
    
#     # Create a temporary table with better NULL handling
#     temp_table <- unique(random_string("temp_", 10))
#     table_identifier <- dbQuoteIdentifier(con, table_name)
#     dbExecute(con, sprintf('CREATE TEMPORARY TABLE %s AS SELECT *, location_id::text as location_id_text FROM %s', 
#                           dbQuoteIdentifier(con, temp_table), 
#                           table_identifier))
    
#     # Optimize duplicate checks with direct SQL - count only exact duplicates
#     cat("Checking for duplicates...\n")
#     exact_dupes_sql <- sprintf("
#       SELECT COUNT(*) as n FROM (
#         SELECT COUNT(*) as cnt
#         FROM %s
#         GROUP BY \"Id\", \"location_id\", \"Tarih\", \"PM10\", \"PM25\", \"SO2\", \"CO\", \"NO2\", \"NOX\", \"NO\", \"O3\"
#         HAVING COUNT(*) > 1
#       ) t", temp_table)
    
#     exact_duplicates <- as.numeric(dbGetQuery(con, exact_dupes_sql)$n)
    
#     # Modified timestamp duplicate check - count duplicates per location
#     time_dupes_sql <- sprintf("
#       WITH LocationTimeDupes AS (
#         SELECT \"Tarih\", \"location_id\", COUNT(*) as cnt
#         FROM %s
#         GROUP BY \"Tarih\", \"location_id\"
#         HAVING COUNT(*) > 1
#       )
#       SELECT COUNT(*) as n FROM LocationTimeDupes", temp_table)
    
   
    
#     time_duplicates <- as.numeric(dbGetQuery(con, time_dupes_sql)$n)
    
    
#     # Remove only exact duplicates, keep best quality data per timestamp
#     dedup_sql <- sprintf("
#       DELETE FROM %s
#       WHERE ctid NOT IN (
#         SELECT MIN(ctid)
#         FROM %s
#         GROUP BY \"Tarih\", \"location_id\"
#       )", temp_table, temp_table)
    
#     dbExecute(con, dedup_sql)
    
#     # Count negative values - only check non-NULL values
#     cat("Checking for negative values...\n")
#     negative_sql <- sprintf("
#       SELECT COUNT(*) as n
#       FROM %s
#       WHERE (\"PM10\" < 0 AND \"PM10\" IS NOT NULL)
#          OR (\"PM25\" < 0 AND \"PM25\" IS NOT NULL)
#          OR (\"SO2\" < 0 AND \"SO2\" IS NOT NULL)
#          OR (\"NO2\" < 0 AND \"NO2\" IS NOT NULL)
#          OR (\"O3\" < 0 AND \"O3\" IS NOT NULL)
#          OR (\"CO\" < 0 AND \"CO\" IS NOT NULL)
#          OR (\"NO\" < 0 AND \"NO\" IS NOT NULL)
#          OR (\"NOX\" < 0 AND \"NOX\" IS NOT NULL)", temp_table)
    
#     negative_count_sql <- as.numeric(dbGetQuery(con, negative_sql)$n)
    
#     # Check PM relationships - only when both values exist
#     cat("Checking for invalid PM relationships...\n")
#     invalid_pm_sql <- sprintf("
#       SELECT COUNT(*) as n
#       FROM %s
#       WHERE \"PM25\" > \"PM10\" 
#         AND \"PM25\" IS NOT NULL 
#         AND \"PM10\" IS NOT NULL", temp_table)
    
#     invalid_pm <- as.numeric(dbGetQuery(con, invalid_pm_sql)$n)
    
#     # Get samples BEFORE applying any filters
#     cat("\nSample of records with negative values:\n")
#     negative_samples_sql <- sprintf("
#       SELECT \"Tarih\", location_id_text as location_id, \"Istasyon_modified\",
#              \"PM10\", \"PM25\", \"SO2\", \"NO2\", \"O3\", \"CO\", \"NO\", \"NOX\"
#       FROM %s
#       WHERE (\"PM10\" < 0 AND \"PM10\" IS NOT NULL)
#          OR (\"PM25\" < 0 AND \"PM25\" IS NOT NULL)
#          OR (\"SO2\" < 0 AND \"SO2\" IS NOT NULL)
#          OR (\"NO2\" < 0 AND \"NO2\" IS NOT NULL)
#          OR (\"O3\" < 0 AND \"O3\" IS NOT NULL)
#          OR (\"CO\" < 0 AND \"CO\" IS NOT NULL)
#          OR (\"NO\" < 0 AND \"NO\" IS NOT NULL)
#          OR (\"NOX\" < 0 AND \"NOX\" IS NOT NULL)
#       ORDER BY \"Tarih\" DESC
#       LIMIT 5", temp_table)
#     print(dbGetQuery(con, negative_samples_sql))

#     cat("\nSample of records where PM2.5 > PM10:\n")
#     invalid_pm_samples_sql <- sprintf("
#       SELECT \"Tarih\", location_id_text as location_id, \"Istasyon_modified\",
#              \"PM10\", \"PM25\",
#              ROUND((\"PM25\" - \"PM10\")::numeric, 2) as difference
#       FROM %s
#       WHERE \"PM25\" > \"PM10\" 
#         AND \"PM25\" IS NOT NULL 
#         AND \"PM10\" IS NOT NULL
#       ORDER BY (\"PM25\" - \"PM10\") DESC
#       LIMIT 5", temp_table)
#     print(dbGetQuery(con, invalid_pm_samples_sql))

#     # cat("\nSample of records with invalid NOx relationships:\n")
#     # invalid_nox_samples_sql <- sprintf("
#     #   SELECT \"Tarih\", location_id_text as location_id, \"Istasyon_modified\",
#     #          \"NO\", \"NO2\", \"NOX\", 
#     #          ROUND((\"NO\" + \"NO2\")::numeric, 2) as sum_no_no2,
#     #          ROUND((\"NOX\" * 0.5)::numeric, 2) as min_expected,
#     #          ROUND((\"NOX\" * 1.5)::numeric, 2) as max_expected
#     #   FROM %s
#     #   WHERE \"NO\" IS NOT NULL 
#     #     AND \"NO2\" IS NOT NULL 
#     #     AND \"NOX\" IS NOT NULL
#     #     AND (\"NO\" + \"NO2\") NOT BETWEEN \"NOX\" * 0.5 AND \"NOX\" * 1.5
#     #   ORDER BY ABS((\"NO\" + \"NO2\") - \"NOX\") DESC
#     #   LIMIT 5", temp_table)
#     # print(dbGetQuery(con, invalid_nox_samples_sql))

#     # If checking hourly data, also show samples with wrong time format
#     if (check_hourly) {
#       cat("\nSample of records with incorrect time format:\n")
#       wrong_time_samples_sql <- sprintf("
#         SELECT \"Tarih\", location_id_text as location_id, \"Istasyon_modified\",
#                EXTRACT(HOUR FROM \"Tarih\") as hour,
#                EXTRACT(MINUTE FROM \"Tarih\") as minute,
#                EXTRACT(SECOND FROM \"Tarih\") as second
#         FROM %s
#         WHERE EXTRACT(MINUTE FROM \"Tarih\") != 0 
#            OR EXTRACT(SECOND FROM \"Tarih\") != 56
#         ORDER BY \"Tarih\" DESC
#         LIMIT 5", temp_table)
#       print(dbGetQuery(con, wrong_time_samples_sql))
#     }

#     # Apply filters - separate UPDATE statements for negative values and PM relationship
#     cat("Applying data quality filters...\n")
    
#     # First UPDATE: Handle negative values
#     filter_sql_negative <- sprintf("
#       UPDATE %s
#       SET 
#         \"PM10\" = CASE WHEN \"PM10\" < 0 THEN NULL ELSE \"PM10\" END,
#         \"PM25\" = CASE WHEN \"PM25\" < 0 THEN NULL ELSE \"PM25\" END,
#         \"SO2\" = CASE WHEN \"SO2\" < 0 THEN NULL ELSE \"SO2\" END,
#         \"NO2\" = CASE WHEN \"NO2\" < 0 THEN NULL ELSE \"NO2\" END,
#         \"O3\" = CASE WHEN \"O3\" < 0 THEN NULL ELSE \"O3\" END,
#         \"CO\" = CASE WHEN \"CO\" < 0 THEN NULL ELSE \"CO\" END,
#         \"NO\" = CASE WHEN \"NO\" < 0 THEN NULL ELSE \"NO\" END,
#         \"NOX\" = CASE WHEN \"NOX\" < 0 THEN NULL ELSE \"NOX\" END", 
#       temp_table)
    
#     dbExecute(con, filter_sql_negative)
    
#     # Second UPDATE: Handle PM2.5 > PM10 cases
#     filter_sql_pm <- sprintf("
#       UPDATE %s
#       SET 
#         \"PM10\" = NULL,
#         \"PM25\" = NULL
#       WHERE \"PM25\" > \"PM10\" 
#         AND \"PM25\" IS NOT NULL 
#         AND \"PM10\" IS NOT NULL", 
#       temp_table)
    
#     dbExecute(con, filter_sql_pm)
    
#     # # After PM relationship check, add NOx relationship check
#     # cat("Checking for invalid NOx relationships...\n")
#     # invalid_nox_sql <- sprintf("
#     #   SELECT COUNT(*) as n
#     #   FROM %s
#     #   WHERE \"NO\" IS NOT NULL 
#     #     AND \"NO2\" IS NOT NULL 
#     #     AND \"NOX\" IS NOT NULL
#     #     AND (\"NO\" + \"NO2\") NOT BETWEEN \"NOX\" * 0.5 AND \"NOX\" * 1.5", temp_table)
    
#     # invalid_nox <- as.numeric(dbGetQuery(con, invalid_nox_sql)$n)
    
#     # # Add NOx validation to the filters
#     # filter_sql_nox <- sprintf("
#     #   UPDATE %s
#     #   SET 
#     #     \"NO\" = NULL,
#     #     \"NO2\" = NULL,
#     #     \"NOX\" = NULL
#     #   WHERE \"NO\" IS NOT NULL 
#     #     AND \"NO2\" IS NOT NULL 
#     #     AND \"NOX\" IS NOT NULL
#     #     AND (\"NO\" + \"NO2\") NOT BETWEEN \"NOX\" * 0.5 AND \"NOX\" * 1.5", 
#     #   temp_table)
    
#     # dbExecute(con, filter_sql_nox)
    
#     # Create cleaned table after all updates
#     create_cleaned_sql <- sprintf("CREATE TEMPORARY TABLE %s_cleaned AS SELECT * FROM %s", 
#                                 temp_table, temp_table)
    
#     dbExecute(con, create_cleaned_sql)

#     # Handle hourly data check if needed
#     wrong_time <- 0
#     if (check_hourly) {
#       wrong_time_sql <- sprintf("
#         SELECT COUNT(*) as n
#         FROM %s_cleaned
#         WHERE EXTRACT(MINUTE FROM \"Tarih\") != 0 
#            OR EXTRACT(SECOND FROM \"Tarih\") != 56", temp_table)
      
#       wrong_time <- as.numeric(dbGetQuery(con, wrong_time_sql)$n)
      
#       # Filter for hourly data
#       dbExecute(con, sprintf("
#         DELETE FROM %s_cleaned
#         WHERE EXTRACT(MINUTE FROM \"Tarih\") != 0 
#            OR EXTRACT(SECOND FROM \"Tarih\") != 56", temp_table))
#     }
    
#     # Get final cleaned data
#     cleaned_data <- tbl(con, sprintf("%s_cleaned", temp_table))
#     cleaned_count <- as.numeric(dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM %s_cleaned", temp_table))$n)
    
#     # Clean up temporary tables
#     on.exit({
#       dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", temp_table))
#       dbExecute(con, sprintf("DROP TABLE IF EXISTS %s_cleaned", temp_table))
#     })
    
#     return(list(
#       data = cleaned_data,
#       original_count = original_count,
#       cleaned_count = cleaned_count,
#       exact_duplicates = exact_duplicates,
#       time_duplicates = time_duplicates,
#       negative_values = negative_count_sql,
#       invalid_pm = invalid_pm,
#       # invalid_nox = invalid_nox,
#       wrong_time = wrong_time
#     ))
    
#   }, error = function(e) {
#     log_error("Error in tidy_air_quality_data", e)
#     stop(e$message)
#   })
# }

# Helper function to generate random string
random_string <- function(prefix = "", n = 10) {
  paste0(prefix, paste0(sample(c(letters, 0:9), n, replace = TRUE), collapse = ""))
}


# Function to clean and analyze tables
process_table <- function(con, table_name, check_hourly = FALSE) {
  tryCatch({
    cat(sprintf("\nProcessing %s...\n", table_name))
    
    # Start timer for performance measurement
    start_time <- Sys.time()
    
    # First check if table exists
    table_exists <- dbExistsTable(con, table_name)
    if(!table_exists) {
      stop(paste("Table", table_name, "does not exist in the database"))
    }
    
    # Get table reference
    tbl_db <- tbl(con, table_name)
    
    # Print sample of the data for debugging
    cat("Sample data (first 3 rows):\n")
    print(tbl_db %>% head(3) %>% collect())
    
    result <- tidy_air_quality_data(tbl_db, table_name, check_hourly)
    
    # Calculate execution time
    end_time <- Sys.time()
    execution_time <- difftime(end_time, start_time, units = "secs")
    
    cat(sprintf("\nAnalysis Report for %s:\n", table_name))
    cat("----------------------------------------\n")
    cat("Original records:", result$original_count, "\n")
    cat("Records that remain:", result$cleaned_count, "\n")
    cat("Records removed:", result$original_count - result$cleaned_count, "\n")
    cat("Exact duplicates found:", result$exact_duplicates, "\n")
    cat("Timestamp duplicates found:", result$time_duplicates, "\n")
    cat("Records with negative values:", result$negative_values, "\n")
    cat("Records where PM2.5 > PM10:", result$invalid_pm, "\n")
    # cat("Records where NO + NO2 not within ±50% of NOx:", result$invalid_nox, "\n")
    
    if (check_hourly) {
      cat("Records with incorrect time format (not :00:56):", result$wrong_time, "\n")
    }
    
    cat("Processing completed in:", round(execution_time, 2), "seconds\n")
    cat("----------------------------------------\n\n")
    
    # Commented out: Code that would update the database
    # cleaned_data <- result$data %>% collect()
    # dbWriteTable(con, paste0(table_name, "_cleaned"), cleaned_data, overwrite = TRUE)
    # cat("Created cleaned table:", paste0(table_name, "_cleaned"), "\n")
    
  }, error = function(e) {
    log_error(sprintf("Error processing %s", table_name), e)
  })
}

# Main execution with error handling
tryCatch({
  cat("Connecting to database...\n")
    con <- temizhavaR:::create_postgres_conn()

  
  
  process_table(con, "hourly_detail", check_hourly = TRUE)
  process_table(con, "daily_detail", check_hourly = FALSE)
}, error = function(e) {
  log_error("Main execution error", e)
}, finally = {
  if (exists("con") && dbIsValid(con)) {
    dbDisconnect(con)
    cat("Database connection closed.\n")
  }
})
