library(DBI)
library(RSQLite)
library(temizhavaR)

rename_pm_columns <- function() {
  db <- create_postgres_conn()
  
  tables <- c("daily_detail", "hourly_detail")
  
  for (table in tables) {
    tryCatch({
      sql1 <- sprintf('ALTER TABLE %s RENAME COLUMN "PM2.5" TO "PM25"', table)
      dbExecute(db, sql1)

      
      cat(sprintf("Renamed PM2.5/pm25 to PM25 in table %s\n", table))
    }, error = function(e) {
      cat(sprintf("Note: Column rename failed in %s: %s\n", table, e$message))
    })

    tryCatch({
            
      sql2 <- sprintf('ALTER TABLE %s RENAME COLUMN pm25 TO "PM25"', table)
      dbExecute(db, sql2)
      
      cat(sprintf("Renamed pm25 to PM25 in table %s\n", table))
    }, error = function(e) {
      cat(sprintf("Note: Column rename failed in %s: %s\n", table, e$message))
    })

  }
  
  dbDisconnect(db)
}

rename_pm_columns()
