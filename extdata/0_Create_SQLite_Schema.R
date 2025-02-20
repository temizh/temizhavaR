suppressMessages({
  library(uuid)
  library(dplyr)
  library(readxl)
  library(writexl)
  library(RSQLite)
  library(DBI)
  library(temizhavaR)
})

get_db_connection <- function(db_path = "../temiz-hava.sqlite") {
  db_path <- normalizePath(db_path, mustWork = FALSE)
  dbConnect(RSQLite::SQLite(), db_path)
}

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
      Tarih DATETIME,
      PM10 DOUBLE,
      `PM2.5` DOUBLE,
      SO2 DOUBLE,
      CO DOUBLE,
      NO2 DOUBLE,
      NOX DOUBLE,
      NO DOUBLE,
      O3 DOUBLE,
      Istasyon_modified TEXT
    );"
  
  dbExecute(mydb, sprintf(detail_table_sql, "hourly_detail"))
  dbExecute(mydb, sprintf(detail_table_sql, "daily_detail"))
}

show_db_info <- function(db_path = "../temiz-hava.sqlite") {
  abs_path <- normalizePath(db_path, mustWork = TRUE)
  message("Database created at: ", abs_path)
  message("Database size: ", file.size(abs_path) / 1024 / 1024, " MB")
}

create_SQL_schema <- function() {
  init.temizhavaR()
  
  db_path <- "../temiz-hava.sqlite"
  mydb <- NULL
  tryCatch({
    mydb <- get_db_connection(db_path)
    on.exit(if (!is.null(mydb) && dbIsValid(mydb)) dbDisconnect(mydb))
    
    create_location_table(mydb)
    create_detail_tables(mydb)
    
    message("Schema created successfully")
    show_db_info(db_path)
  }, error = function(e) {
    stop(paste("Error creating schema:", e$message))
  })
}

create_SQL_schema()
