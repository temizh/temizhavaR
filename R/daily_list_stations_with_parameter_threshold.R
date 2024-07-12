#' List Stations for specified parameter from daily_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The season for which the data availability is calculated (default is "all").
#' @export

daily_list_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season = NULL) {

  if (0) {
    mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

    if (!is.null(season) && season == "summer") {
      query <- paste0("SELECT Istasyon, (SUM(CASE WHEN ", parameter_name, " IS NOT NULL AND strftime('%m', Tarih) IN ('04', '05', '06', '07', '08', '09') THEN 1 ELSE 0 END) * 100 / 183) AS veri_mevcudiyet_yuzdesi
                      FROM daily_detail
                      GROUP BY Istasyon
                      HAVING veri_mevcudiyet_yuzdesi >= ", threshold)
    } else if (!is.null(season) && season == "winter") {
      query <- paste0("SELECT Istasyon, (SUM(CASE WHEN ", parameter_name, " IS NOT NULL AND strftime('%m', Tarih) IN ('01', '02', '03', '10', '11', '12') THEN 1 ELSE 0 END) * 100 / 182) AS veri_mevcudiyet_yuzdesi
                      FROM daily_detail
                      GROUP BY Istasyon
                      HAVING veri_mevcudiyet_yuzdesi >= ", threshold)
    } else {
      query <- paste0("SELECT Istasyon, (SUM(CASE WHEN ", parameter_name, " IS NOT NULL THEN 1 ELSE 0 END) * 100 / 365) AS veri_mevcudiyet_yuzdesi
                      FROM daily_detail
                      GROUP BY Istasyon
                      HAVING veri_mevcudiyet_yuzdesi >= ", threshold)
    }

    query_result <- dbGetQuery(mydb, query)
    dbDisconnect(mydb)
  } else {
    parameter_name <- gsub('\\"', "", parameter_name)

    query_result <- all_daily_detail_load_from_database(parameter_name) %>%
      select(Istasyon, Tarih, rlang::sym(parameter_name))

    if (!is.null(season) && season == "summer") {
      query_result <- query_result %>%
        filter(month(Tarih) %in% 4:9)
      day_count <- 183
    } else if (!is.null(season) && season == "winter") {
      query_result <- query_result %>%
        filter(month(Tarih) %in% c(1, 2, 3, 10, 11, 12))
      day_count <- 182
    } else {
      day_count <- 365
    }

    query_result <- query_result %>%
      group_by(Istasyon) %>%
      summarise(veri_mevcudiyet_yuzdesi = round(length(which(!is.na(.data[[parameter_name]]))) / day_count * 100)) %>%
      arrange(desc(veri_mevcudiyet_yuzdesi), Istasyon) %>%
      filter(veri_mevcudiyet_yuzdesi >= threshold)
  }

  return(query_result)
}

