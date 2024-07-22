#' Hourly detail load from database
#'
#' @param parameter The name of the parameter to return. All parameters are returned if not provided.
#' @export


all_daily_detail_load_from_database <- function(parm) {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  query <- "SELECT * FROM daily_detail"
  query_result <- dbGetQuery(mydb, query)

  query_result$Tarih <- as.POSIXct(query_result$Tarih, format = "%Y-%m-%d %H:%M:%S")

  dbDisconnect(mydb)

  if (!missing(parm))
    query_result <- query_result %>%
    select(Istasyon, Tarih, any_of(parm))

  return(query_result)
}
