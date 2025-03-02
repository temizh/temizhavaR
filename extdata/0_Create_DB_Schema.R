suppressMessages({
  library(uuid)
  library(dplyr)
  library(readxl)
  library(writexl)

  library(DBI)
  library(temizhavaR)
})

create_location_table <- function(mydb) {
  location_sql <- "
    CREATE TABLE IF NOT EXISTS location (
      Bolge TEXT,
      Sehir TEXT,
      Plaka TEXT,
      Istasyonlar TEXT,
      Istasyonlar_modified TEXT,
      Id TEXT PRIMARY KEY
    );"
  
  dbExecute(mydb, location_sql)
}

create_detail_tables <- function(mydb) {
  dbExecute(mydb, "DROP TABLE IF EXISTS hourly_detail")
  dbExecute(mydb, "DROP TABLE IF EXISTS daily_detail")
  
  detail_table_sql <- "
    CREATE TABLE %s (
      Istasyon TEXT,
      location_id TEXT,
      Tarih TIMESTAMP,
      PM10 DOUBLE PRECISION,
      PM25 DOUBLE PRECISION,
      SO2 DOUBLE PRECISION,
      CO DOUBLE PRECISION,
      NO2 DOUBLE PRECISION,
      NOX DOUBLE PRECISION,
      NO DOUBLE PRECISION,
      O3 DOUBLE PRECISION,
      Istasyon_modified TEXT
    );"
  
  dbExecute(mydb, sprintf(detail_table_sql, "hourly_detail"))
  dbExecute(mydb, sprintf(detail_table_sql, "daily_detail"))
}

create_SQL_schema <- function() {
  init.temizhavaR()
  
  mydb <- create_postgres_conn()
  on.exit({
    if (!is.null(mydb) && dbIsValid(mydb)) dbDisconnect(mydb)
  })
  
  create_location_table(mydb)
  create_detail_tables(mydb)
  
  message("Schema created successfully")
}

create_SQL_schema()
