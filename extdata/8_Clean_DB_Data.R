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

# Function to clean and analyze tables
process_table <- function(con, table_name, check_hourly = FALSE) {
  tryCatch({
    cat(sprintf("\nProcessing %s...\n", table_name))
    cat("========================================\n")
    
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
    sample_data <- tbl_db %>% head(3) %>% collect()
    print(sample_data)
    cat("\n")
    
    # Call the optimized tidy function
    result <- tidy_air_quality_data(tbl_db, table_name, check_hourly, con, update_original = FALSE)
    
    # Calculate execution time
    end_time <- Sys.time()
    execution_time <- difftime(end_time, start_time, units = "secs")
    
    cat(sprintf("\nCleaning Results for %s:\n", table_name))
    cat("========================================\n")
    cat("Original records:", result$original_count, "\n")
    cat("Records that remain:", result$cleaned_count, "\n")
    cat("Records removed:", result$original_count - result$cleaned_count, "\n")
    cat("----------------------------------------\n")
    cat("Issues found and fixed:\n")
    cat("  • Exact duplicates:", result$exact_duplicates, "\n")
    cat("  • Timestamp duplicates:", result$time_duplicates, "\n")
    cat("  • Negative values:", result$negative_values, "\n")
    cat("  • PM2.5 > PM10 cases:", result$invalid_pm, "\n")
    cat("  • Invalid NOx relationships:", result$invalid_nox, "\n")
    
    if (check_hourly) {
      cat("  • Incorrect time format (not :00:56):", result$wrong_time, "\n")
    }
    
    cat("----------------------------------------\n")
    cat("Processing completed in:", round(execution_time, 2), "seconds\n")
    
    # Persist cleaned data back to your database using SQL directly
    if (result$cleaned_count > 0) {
      cleaned_table_name <- paste0(table_name, "_cleaned")
      temp_table_name <- result$data  # This is now the temp table name
      
      # Drop existing cleaned table if it exists
      if (dbExistsTable(con, cleaned_table_name)) {
        dbExecute(con, sprintf("DROP TABLE %s", cleaned_table_name))
      }
      
      # Create the cleaned table directly from the temporary table
      cat("Creating cleaned table from temporary table...\n")
      dbExecute(con, sprintf("CREATE TABLE %s AS SELECT * FROM %s", 
                            cleaned_table_name, temp_table_name))
      cat("✅ Created cleaned table:", cleaned_table_name, "\n")
      
      # Quick sanity check: show sample of cleaned data
      cat("\nSample of cleaned data (first 5 rows):\n")
      sample_cleaned <- dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 5", 
                                                cleaned_table_name))
      print(sample_cleaned)
      
      # Now clean up the temporary table
      if (!is.null(result$cleanup_function)) {
        result$cleanup_function()
        cat("🧹 Cleaned up temporary table\n")
      }
    } else {
      cat("⚠️  No data to persist - all records were filtered out\n")
    }
    
    cat("========================================\n\n")
    
  }, error = function(e) {
    log_error(sprintf("Error processing %s", table_name), e)
    cat(sprintf("❌ Failed to process table %s: %s\n", table_name, e$message))
  })
}

# Main execution with error handling
tryCatch({
  cat("Starting database cleaning process...\n")
  cat("====================================\n")
  
  cat("Connecting to database...\n")
  con <- temizhavaR:::create_postgres_conn()
  
  if (!dbIsValid(con)) {
    stop("Failed to establish database connection")
  }
  
  cat("✅ Database connection established successfully\n\n")
  
  # Process both tables with step-by-step cleaning
  process_table(con, "hourly_detail", check_hourly = TRUE)
  process_table(con, "daily_detail", check_hourly = FALSE)
  
  cat("🎉 All tables processed successfully!\n")
  
}, error = function(e) {
  log_error("Main execution error", e)
  cat(sprintf("❌ Process failed: %s\n", e$message))
}, finally = {
  if (exists("con") && dbIsValid(con)) {
    dbDisconnect(con)
    cat("✅ Database connection closed.\n")
  }
})
