#' List Stations for specified parameter from daily_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @export

daily_list_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season) {

  init.temizhavaR()

  if (0) {
    mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

    query <- paste0("SELECT Istasyon, (SUM(CASE WHEN ", parameter_name, " IS NOT NULL THEN 1 ELSE 0 END) * 100 / 365) AS veri_mevcudiyet_yuzdesi
                   FROM daily_detail
                   GROUP BY Istasyon
                   HAVING veri_mevcudiyet_yuzdesi >= ", threshold)

    query_result <- dbGetQuery(mydb, query)
    dbDisconnect(mydb)
  }
  else {
    query_result <- all_daily_detail_load_from_database(parameter_name) %>%
      select(Istasyon, Tarih, rlang::sym(parameter_name)) %>%
      group_by(Istasyon) %>%
      summarise(veri_mevcudiyet_yuzdesi = round(length(which(!is.na(.data[[parameter_name]]))) / 365 * 100)) %>%
      arrange(desc(veri_mevcudiyet_yuzdesi), Istasyon)
  }


  return(query_result)
}




