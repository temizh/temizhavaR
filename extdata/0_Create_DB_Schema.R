suppressMessages({
  library(dplyr)
  library(readxl)
  library(writexl)
  library(DBI)
  library(temizhavaR)
})

create_location_table <- function(mydb) {
  location_sql <- "
    CREATE TABLE IF NOT EXISTS location (
      \"Id\" SERIAL PRIMARY KEY,
      \"Bolge\" TEXT,
      \"Sehir\" TEXT,
      \"Plaka\" TEXT,
      \"Istasyon_original\" TEXT,
      \"Istasyon_modified\" TEXT UNIQUE
    );"
  
  dbExecute(mydb, location_sql)
}

create_detail_tables <- function(mydb) {
  hourly_detail_sql <- '
    CREATE TABLE IF NOT EXISTS "hourly_detail" (
      "Id" SERIAL PRIMARY KEY,
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
      "Istasyon_modified" TEXT REFERENCES location("Istasyon_modified") ON DELETE CASCADE
    );'
    
  daily_detail_sql <- '
    CREATE TABLE IF NOT EXISTS "daily_detail" (
      "Id" SERIAL PRIMARY KEY,
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
      "Istasyon_modified" TEXT REFERENCES location("Istasyon_modified") ON DELETE CASCADE
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

create_indexes <- function(mydb) {
  index_statements <- c(
    # Time-based indexes
    "CREATE INDEX IF NOT EXISTS idx_hourly_detail_tarih ON hourly_detail(\"Tarih\")",
    "CREATE INDEX IF NOT EXISTS idx_daily_detail_tarih ON daily_detail(\"Tarih\")",
    
    # Location indexes
    "CREATE INDEX IF NOT EXISTS idx_hourly_detail_location ON hourly_detail(\"location_id\")",
    "CREATE INDEX IF NOT EXISTS idx_daily_detail_location ON daily_detail(\"location_id\")",
    "CREATE INDEX IF NOT EXISTS idx_location_sehir ON location(\"Sehir\")",
    "CREATE INDEX IF NOT EXISTS idx_location_bolge ON location(\"Bolge\")",
    
    # Composite indexes
    "CREATE INDEX IF NOT EXISTS idx_hourly_detail_loc_time ON hourly_detail(\"location_id\", \"Tarih\")",
    "CREATE INDEX IF NOT EXISTS idx_daily_detail_loc_time ON daily_detail(\"location_id\", \"Tarih\")",
    
    # Pollutant indexes - daily
    "CREATE INDEX IF NOT EXISTS idx_daily_detail_pm10 ON daily_detail(\"PM10\")",
    "CREATE INDEX IF NOT EXISTS idx_daily_detail_pm25 ON daily_detail(\"PM25\")",
    "CREATE INDEX IF NOT EXISTS idx_daily_detail_so2 ON daily_detail(\"SO2\")",
    "CREATE INDEX IF NOT EXISTS idx_daily_detail_co ON daily_detail(\"CO\")",
    "CREATE INDEX IF NOT EXISTS idx_daily_detail_no2 ON daily_detail(\"NO2\")",
    "CREATE INDEX IF NOT EXISTS idx_daily_detail_nox ON daily_detail(\"NOX\")",
    "CREATE INDEX IF NOT EXISTS idx_daily_detail_no ON daily_detail(\"NO\")",
    "CREATE INDEX IF NOT EXISTS idx_daily_detail_o3 ON daily_detail(\"O3\")",
    
    # Pollutant indexes - hourly
    "CREATE INDEX IF NOT EXISTS idx_hourly_detail_pm10 ON hourly_detail(\"PM10\")",
    "CREATE INDEX IF NOT EXISTS idx_hourly_detail_pm25 ON hourly_detail(\"PM25\")",
    "CREATE INDEX IF NOT EXISTS idx_hourly_detail_so2 ON hourly_detail(\"SO2\")",
    "CREATE INDEX IF NOT EXISTS idx_hourly_detail_co ON hourly_detail(\"CO\")",
    "CREATE INDEX IF NOT EXISTS idx_hourly_detail_no2 ON hourly_detail(\"NO2\")",
    "CREATE INDEX IF NOT EXISTS idx_hourly_detail_nox ON hourly_detail(\"NOX\")",
    "CREATE INDEX IF NOT EXISTS idx_hourly_detail_no ON hourly_detail(\"NO\")",
    "CREATE INDEX IF NOT EXISTS idx_hourly_detail_o3 ON hourly_detail(\"O3\")"
  )
  
  for(sql in index_statements) {
    tryCatch({
      dbExecute(mydb, sql)
    }, error = function(e) {
      message("Error creating index: ", sql, "\nError message: ", e$message)
    })
  }
  
  message("Database indexes creation completed")
}

create_SQL_schema <- function() {
  init.temizhavaR()
  
  mydb <- temizhavaR:::create_postgres_conn()
  if(is.null(mydb) || !dbIsValid(mydb)) {
    stop("Could not create database connection.")
  }
  
  on.exit({
    if (!is.null(mydb) && dbIsValid(mydb)) dbDisconnect(mydb)
  })
  
  create_location_table(mydb)
  create_detail_tables(mydb)
  create_indexes(mydb)  
  
  message("Schema creation process completed")
}

create_SQL_schema()
