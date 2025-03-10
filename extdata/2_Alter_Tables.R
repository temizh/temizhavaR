library(DBI)
library(RSQLite)

alter_tables <- function() {
  db <- create_postgres_conn()
  
  new_columns <- c(
    '"Sampling_Point_Id" TEXT',
    '"Longitude" REAL',
    '"Latitude" REAL',
    '"Altitude" REAL',
    '"LONGTD" REAL',
    '"LATTD" REAL',
    '"Air_Quality_Station_Area" TEXT',
    '"PM10ISTASYON" TEXT',
  )
  
  tryCatch({
    sql <- 'ALTER TABLE location ALTER COLUMN "Tarih" TYPE TIMESTAMP WITH TIME ZONE 
            USING "Tarih"::TIMESTAMP WITH TIME ZONE'
    dbExecute(db, sql)
    cat("Modified Tarih column type to TIMESTAMP WITH TIME ZONE\n")
  }, error = function(e) {
    cat("Note: Could not modify Tarih column type\n")
  })
  
  tables <- c("location")
  
  for (table in tables) {
    for (col in new_columns) {
      tryCatch({
        sql <- sprintf('ALTER TABLE %s ADD COLUMN %s', table, col)
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
