#' Sum detail save to database
#'
#' @param processed_data processed data.
#' @export

daily_detail_save_to_database <- function(processed_data) {
  data <- processed_data$data
  param_names <- processed_data$param_names
  station_name <- processed_data$station_name

  mydb <- dbConnect(RSQLite::SQLite(), dbname = "temiz-hava.sqlite", encoding = "UTF-8")

  # data$Tarih <- format(data$Tarih, "%Y-%m-%d")
  data$Tarih <- format(data$Tarih, "%Y-%m-%d %H:%M:%S")
  col_mapping <- c("Istasyon" = "Istasyon" ,"location_id"= "location_id","Tarih" = "Tarih", "PM10" = "PM10", "PM2.5" = "\"PM2.5\"", "SO2" = "SO2","CO" = "CO", "NO2" = "NO2", "NOX" = "NOX", "NO" = "NO", "O3" = "O3")
  data$Istasyon <- station_name

  station_count <- dbGetQuery(mydb, paste0("SELECT COUNT(*) FROM daily_detail WHERE Istasyon = '", station_name, "'"))

  if (station_count > 0) {
    dbExecute(mydb, paste0("DELETE FROM daily_detail WHERE Istasyon = '", station_name, "'"))
  }

  qry <- paste0("SELECT Id FROM location_2023 WHERE Istasyonlar = '", station_name, "'")
  location_ids <- dbGetQuery(mydb, qry)
  if (nrow(location_ids) > 0) {
    data$location_id <- rep(location_ids$Id, nrow(data))
  }
  head(data$location_id)
  print(paste("location_ids : ", location_ids))


  dbWriteTable(mydb, "daily_detail", data, col.names = col_mapping, append = TRUE, fileEncoding = "UTF-8")

  query <- paste0("SELECT * FROM daily_detail WHERE Istasyon = '", station_name, "'")
  query_result <- dbGetQuery(mydb, query)


  dbDisconnect(mydb)
  return(head(query_result))
}
