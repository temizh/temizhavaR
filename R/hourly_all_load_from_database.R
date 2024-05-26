#' Hourly detail load from database
#'
#' @param station_name The name of the station to filter the data.
#' @export




all_hourly_detail_load_from_database <- function() {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")


  query <- "SELECT * FROM hourly_detail"
  query_result <- dbGetQuery(mydb, query)


  query_result$Tarih <- as.POSIXct(query_result$Tarih, format = "%Y-%m-%d %H:%M:%S")


  dbDisconnect(mydb)


  return(query_result)
}
