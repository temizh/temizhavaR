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
  
  alter_sql <- "
    DO $$ 
    BEGIN
      -- Backup existing data
      CREATE TEMP TABLE location_backup AS 
      SELECT \"Bolge\", \"Sehir\", \"Plaka\", \"Istasyon_modified\"
      FROM location;
      
      -- Drop existing table
      DROP TABLE location;
      
      -- Recreate table with proper structure
      CREATE TABLE location (
        \"Id\" SERIAL PRIMARY KEY,
        \"Bolge\" TEXT,
        \"Sehir\" TEXT,
        \"Plaka\" TEXT,
        \"Istasyon_modified\" TEXT UNIQUE
      );
      
      -- Restore data
      INSERT INTO location (\"Bolge\", \"Sehir\", \"Plaka\",\"Istasyon_modified\")
      SELECT \"Bolge\", \"Sehir\", \"Plaka\", \"Istasyon_modified\"
      FROM location_backup;
      
      -- Clean up
      DROP TABLE location_backup;
    END $$;"
  
  dbExecute(mydb, alter_sql)
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
