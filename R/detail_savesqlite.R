#' save detail data to database
#'
#' @param processed_data processed data.
#' @export

detail_save_to_database <- function(processed_data, table_name, verbose = FALSE) {
#browser()
  YEAR <- options()$temizhavaR.YEAR

  data <- processed_data$data
  param_names <- processed_data$param_names
  station_name <- processed_data$station_name

  mydb <- dbConnect(RSQLite::SQLite(), dbname = file.path(base_dir, "temiz-hava.sqlite"), encoding = "UTF-8")

  # data$Tarih <- format(data$Tarih, "%Y-%m-%d")
  data$Tarih <- format(data$Tarih, "%Y-%m-%d %H:%M:%S")
  col_mapping <- c("Istasyon_modified" = "Istasyon_modified" ,"location_id"= "location_id","Tarih" = "Tarih", "PM10" = "PM10", "PM
                   25" = "PM25", "SO2" = "SO2","CO" = "CO", "NO2" = "NO2", "NOX" = "NOX", "NO" = "NO", "O3" = "O3")
  data$Istasyon_modified <- station_name

  station_count <- dbGetQuery(mydb, paste0("SELECT COUNT(*) FROM ", table_name,  " WHERE Istasyon_modified = '", station_name, "'"))

  if (station_count > 0) {
    dbExecute(mydb, paste0("DELETE FROM ", table_name, " WHERE Istasyon_modified = '", station_name, "'"))
  }

  qry <- paste0("SELECT Id FROM location_", YEAR, " WHERE Istasyon_modified = '", station_name, "'")
  location_ids <- dbGetQuery(mydb, qry)
  if (nrow(location_ids) > 0) {
    data$location_id <- rep(location_ids$Id, nrow(data))
  }
  head(data$location_id)
  print(paste("location_ids : ", location_ids))


  dbWriteTable(mydb, table_name, data, col.names = col_mapping, append = TRUE, fileEncoding = "UTF-8")

  query <- paste0("SELECT * FROM ", table_name, " WHERE Istasyon_modified = '", station_name, "'")
  query_result <- dbGetQuery(mydb, query)
  dbDisconnect(mydb)

  if (verbose)
    print(head(query_result))

}
