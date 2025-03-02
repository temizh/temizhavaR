library(DBI)
library(RSQLite)

alter_tables <- function() {
  db <- create_postgres_conn()
  
  new_columns <- c(
    "station_type TEXT",
    "Sampling_Point_Id TEXT",
    "Longitude REAL",
    "Latitude REAL",
    "Altitude REAL",
    "LONGTD REAL",
    "LATTD REAL",
    "Air_Quality_Station_Area TEXT"
  )
  
  tables <- c("daily_detail", "hourly_detail", "location")
  
  for (table in tables) {
    for (col in new_columns) {
      tryCatch({
        sql <- sprintf("ALTER TABLE %s ADD COLUMN %s", table, col)
        dbExecute(db, sql)
        cat(sprintf("Added column %s to table %s\n", col, table))
      }, error = function(e) {
        cat(sprintf("Note: Column %s might already exist in %s\n", col, table))
      })
    }
  }
  
  dbDisconnect(db)
}

alter_tables()
