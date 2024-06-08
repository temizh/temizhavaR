library(DBI)
library(uuid)
library(dplyr)
library(readxl)
library(writexl)

create_SQL_schema <- function() {

  init.temizhavaR()

  YEAR <- options()$temizhavaR.YEAR

  # create a connection to database
  mydb <- dbConnect(RSQLite::SQLite(), file.path(raw_dir,"temiz-hava.sqlite"))

  mydb_hourly_detail <- "
   CREATE TABLE hourly_detail (
    Istasyon TEXT,
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

);
"

  mydb_daily_detail <- "
CREATE TABLE daily_detail(
   Istasyon TEXT,
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
);
"

  mydb_location <- paste0("
CREATE TABLE IF NOT EXISTS location_", YEAR, "(
    Bolge TEXT,
    Sehir TEXT,
    Plaka TEXT,
    Istasyonlar TEXT,
    Id TEXT PRIMARY KEY,
    gunluk_files2022 TEXT,
    saatlik_files2022 TEXT
);
")

  # CREATE TABLE
  dbExecute(mydb, mydb_location)
  dbExecute(mydb, mydb_hourly_detail)

  dbExecute(mydb, mydb_daily_detail)
}

#location_data <- read_excel("C:/Users/Hp/Desktop/location_veri_ayri.xlsx")
#location_data$Id <- sapply(1:nrow(all_stations2023), function(i) as.character(UUIDgenerate()))
#dbWriteTable(mydb, "location", location_data, append = TRUE, row.names = FALSE)
#query_result1 <- dbGetQuery(mydb, "SELECT * FROM hourly_detail LIMIT 10")

create_SQL_schema()
source("make_location_table_2022.R")
location_data <- create_location_table_2022()

daily_detail_save_to_database()








# hourly_detail <- dbReadTable(mydb, "hourly_detail")
# daily_detail <- dbReadTable(mydb, "daily_detail")
# location_tbl <- dbReadTable(mydb, paste0("location_", YEAR))

#dbExecute(mydb, "ALTER TABLE location_deneme RENAME TO location_2022;")
# delete table
#dbExecute(mydb, "DROP TABLE IF EXISTS daily_detail")




# query_step2 <- "
# UPDATE hourly_detail
# SET location_id = (
#     SELECT Id FROM location WHERE location.Istasyonlar = hourly_detail.Istasyon
# );"
# dbExecute(mydb, query_step2)
#
# # Adım 3: location tablosundan istasyonun diğer bilgilerini çekmek için sorgu
# query_step3 <- "
# SELECT hourly_detail.Istasyon, hourly_detail.Tarih, hourly_detail.PM10, hourly_detail.\"PM2.5\", hourly_detail.SO2, hourly_detail.NO2, hourly_detail.NOX, hourly_detail.NO, hourly_detail.O3, location.Sehir, location.Plaka
# FROM hourly_detail
# JOIN location ON hourly_detail.location_id = location.Id;"
# dbGetQuery(mydb, query_step3)
