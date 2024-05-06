#' Hourly detail load from database by station name
#'
#' @param station_name The name of the station to filter the data.
#' @export


daily_detail_load_from_database <- function(processed_data, station_name) {
  data <- processed_data$data
  param_names <- processed_data$param_names
  station_name <- processed_data$station_name


  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  query <- paste0("SELECT * FROM daily_detail WHERE Istasyon = '", station_name, "'")
  query_result <- dbGetQuery(mydb, query)

  query_result$Tarih <- as.POSIXct(query_result$Tarih, format = "%Y-%m-%d")


  dbDisconnect(mydb)

  return(query_result)
}
