library(DBI)
library(RSQLite)

db_file <- "temiz-hava.sqlite"

db_conn <- dbConnect(RSQLite::SQLite(), dbname = db_file)

tabless <- dbListTables(db_conn)
print(tabless)


all_hourly_detail_data <- get_all_station_hourly_detail_data(db_conn)


dbDisconnect(db_conn)


print(head(all_hourly_detail_data))
