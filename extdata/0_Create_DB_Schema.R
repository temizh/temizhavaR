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
  tryCatch({
    dbExecute(mydb, 'DROP TABLE IF EXISTS "hourly_detail"')
    dbExecute(mydb, 'DROP TABLE IF EXISTS "daily_detail"')
  }, error = function(e) {
    message("Error dropping tables: ", e$message)
  })
  
  hourly_detail_sql <- '
    CREATE TABLE "hourly_detail" (
      "Id" SERIAL PRIMARY KEY,
      "Istasyon" TEXT,
      "location_id" TEXT,
      "Tarih" TIMESTAMPTZ,
      "PM10" DOUBLE PRECISION,
      "PM25" DOUBLE PRECISION,
      "SO2" DOUBLE PRECISION,
      "CO" DOUBLE PRECISION,
      "NO2" DOUBLE PRECISION,
      "NOX" DOUBLE PRECISION,
      "NO" DOUBLE PRECISION,
      "O3" DOUBLE PRECISION,
      "Istasyon_modified" TEXT
    );'
    
  daily_detail_sql <- '
    CREATE TABLE "daily_detail" (
      "Id" SERIAL PRIMARY KEY,
      "Istasyon" TEXT,
      "location_id" TEXT,
      "Tarih" TIMESTAMPTZ,
      "PM10" DOUBLE PRECISION,
      "PM25" DOUBLE PRECISION,
      "SO2" DOUBLE PRECISION,
      "CO" DOUBLE PRECISION,
      "NO2" DOUBLE PRECISION,
      "NOX" DOUBLE PRECISION,
      "NO" DOUBLE PRECISION,
      "O3" DOUBLE PRECISION,
      "Istasyon_modified" TEXT
    );'
  
  tryCatch({
    dbExecute(mydb, hourly_detail_sql)
    message("hourly_detail table created successfully")
  }, error = function(e) {
    message("Error creating hourly_detail table: ", e$message)
  })
  
  tryCatch({
    dbExecute(mydb, daily_detail_sql)
    message("daily_detail table created successfully")
  }, error = function(e) {
    message("Error creating daily_detail table: ", e$message)
  })
}

create_SQL_schema <- function() {
  init.temizhavaR()
  
  mydb <- create_postgres_conn()
  if(is.null(mydb) || !dbIsValid(mydb)) {
    stop("Could not create database connection.")
  }
  
  on.exit({
    if (!is.null(mydb) && dbIsValid(mydb)) dbDisconnect(mydb)
  })
  
  create_location_table(mydb)
  create_detail_tables(mydb)
  
  message("Schema creation process completed")
}

create_SQL_schema()
