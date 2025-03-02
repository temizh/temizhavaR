library(DBI)
library(temizhavaR)

test_timestamp_conversion <- function() {
  excel_date <- 41640.00064814815
  
  r_date <- as.POSIXct("1900-01-01", tz="UTC") + ((excel_date - 2) * 86400)
  cat("R conversion result:", format(r_date, "%Y-%m-%d %H:%M:%S"), "\n")
  
  db <- create_postgres_conn()
  on.exit(dbDisconnect(db))
  
  dbExecute(db, "DROP TABLE IF EXISTS test_dates")
  dbExecute(db, "CREATE TABLE test_dates (date_col DOUBLE PRECISION)")
  dbExecute(db, sprintf("INSERT INTO test_dates VALUES (%f)", excel_date))
  
  sql <- "
    SELECT 
      date_col as original_value,
      (timestamp '1900-01-01' + ((date_col - 2) * interval '1 day')) as converted_date
    FROM test_dates"
  
  result <- dbGetQuery(db, sql)
  print(result)
  
  dbExecute(db, "DROP TABLE test_dates")
}

test_timestamp_conversion()
