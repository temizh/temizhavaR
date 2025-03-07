library(DBI)
library(dplyr)
library(dbplyr)
library(temizhavaR)

convert_timestamp_format <- function(force_tables = NULL) {
  db <- create_postgres_conn()
  on.exit(dbDisconnect(db))
  
  tables <- c("daily_detail", "hourly_detail", "location")
  
  for (table in tables) {
    tryCatch({
      check_sql <- sprintf("
        SELECT 
          \"Tarih\"::text as date_text,
          CASE 
            WHEN \"Tarih\" < '2000-01-01'::timestamp OR
                 \"Tarih\" > '2100-01-01'::timestamp
            THEN true
            ELSE false
          END as needs_conversion
        FROM %s 
        LIMIT 5", table)
      
      sample_data <- dbGetQuery(db, check_sql)
      
      cat(sprintf("\nTable: %s\nSample dates:\n", table))
      print(sample_data)
      
      if (!table %in% force_tables && !any(sample_data$needs_conversion)) {
        cat("Table appears to have valid dates - skipping conversion\n")
        next
      }
      
      cat(sprintf("\nConverting %s...\n", table))
      
      backup_table <- paste0(table, "_backup")
      dbExecute(db, sprintf("DROP TABLE IF EXISTS %s", backup_table))
      dbExecute(db, sprintf("CREATE TABLE %s AS SELECT * FROM %s", backup_table, table))
      
      sql_convert <- sprintf('
        ALTER TABLE %s 
        ALTER COLUMN "Tarih" TYPE TIMESTAMPTZ 
        USING (CASE 
          WHEN "Tarih" < \'2000-01-01\'::timestamp 
          THEN timestamp \'1900-01-01\' + ("Tarih"::text::numeric - 2) * interval \'1 day\'
          ELSE "Tarih"
        END)', table)
      
      dbExecute(db, sql_convert)
      
      cat(sprintf("Completed conversion for %s\n", table))
      
    }, error = function(e) {
      cat(sprintf("Error processing %s: %s\n", table, e$message))
      tryCatch({
        if (exists("backup_table")) {
          dbExecute(db, sprintf("DROP TABLE IF EXISTS %s", table))
          dbExecute(db, sprintf("ALTER TABLE %s RENAME TO %s", backup_table, table))
        }
      }, error = function(e) {})
    })
  }
}

convert_timestamp_format()
