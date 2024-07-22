#' Hourly detail load from database by station name
#'
#' @param station_name The name of the station to filter the data.
#' @export

daily_detail_load_from_database <- function(station_name) {

  init.temizhavaR()

  mydb <- dbConnect(RSQLite::SQLite(), file.path(raw_dir, "temiz-hava.sqlite"))

  query <- paste0("SELECT * FROM daily_detail WHERE Istasyon = '", station_name, "'")
  query_result <- dbGetQuery(mydb, query)

  query_result$Tarih <- as.POSIXct(query_result$Tarih, format = "%Y-%m-%d")

  dbDisconnect(mydb)

  query_result
}
