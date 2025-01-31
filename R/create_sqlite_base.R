library(uuid)
library(dplyr)
library(readxl)
library(writexl)
library(DBI)
library(RSQLite)

#' Create SQLite schema for temiz-hava database
#' 
#' This function creates the necessary tables in the SQLite database for storing hourly and daily detail data.
#'
#' @param db_path Path to the SQLite database file
#' @return NULL
#' @export
#' @examples
#' create_sqlite_base()
#' create_sqlite_base("./temiz-hava.sqlite")


create_sqlite_base <- function(db_path = "./temiz-hava.sqlite") {
  
  if (!file.exists(db_path)) {
    file.create(db_path)
  }
  
  mydb <- dbConnect(SQLite(), db_path)
  
  on.exit(dbDisconnect(mydb), add = TRUE)  
  
  create_tables <- function(db_conn) {
    table_queries <- list(
          hourly_detail = "CREATE TABLE IF NOT EXISTS hourly_detail (
        Istasyon TEXT,
        Istasyon_modified TEXT,
        location_id TEXT,
        Tarih DATETIME,
        PM10 DOUBLE,
        \"PM2.5\" DOUBLE,
        SO2 DOUBLE,
        CO DOUBLE,
        NO2 DOUBLE,
        NOX DOUBLE,
        NO DOUBLE,
        O3 DOUBLE
      );",
      daily_detail = "CREATE TABLE IF NOT EXISTS daily_detail (
        Istasyon TEXT,
        Istasyon_modified TEXT,
        location_id TEXT,
        Tarih DATETIME,
        PM10 DOUBLE,
        \"PM2.5\" DOUBLE,
        SO2 DOUBLE,
        CO DOUBLE,
        NO2 DOUBLE,
        NOX DOUBLE,
        NO DOUBLE,
        O3 DOUBLE
      );",
      location = "CREATE TABLE IF NOT EXISTS location (
        Bolge TEXT,
        Sehir TEXT,
        Plaka TEXT,
        Istasyonlar TEXT,
        Istasyonlar_modified TEXT,
        Id TEXT PRIMARY KEY
      );"
    )
    
    lapply(table_queries, function(query) {
      dbExecute(db_conn, query)
    })
  }
  
  create_tables(mydb)
  
  ensure_column_exists <- function(db_conn, table, column) {
    tryCatch({
      dbExecute(db_conn, paste0("ALTER TABLE ", table, " ADD COLUMN ", column, " TEXT"))
    }, error = function(e) {
      if (!grepl("duplicate column name", e$message, ignore.case = TRUE)) {
        warning("Error altering table: ", e$message)
      }
    })
  }
  
  ensure_column_exists(mydb, "location", "Istasyonlar_modified")
  
  message("Database schema created/updated successfully.")
}
