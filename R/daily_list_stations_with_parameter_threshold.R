#' List Stations for Specified Parameter from daily_detail
#'
#' This function lists the stations that have data availability above a specified threshold for a given parameter.
#' You can filter the results for a specific season or for the entire year.
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The specific season to consider (e.g., "summer" for April to September). If NULL, considers the entire year.
#' @return A data frame with stations that meet the threshold criteria, sorted by data availability percentage in descending order.
#' @export
#'
#' @examples
#' daily_list_stations_with_parameter_threshold("PM10", 90, "summer")
#' daily_list_stations_with_parameter_threshold("SO2", 85)

daily_list_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season = NULL) {
  if (!is.null(season)) {
    mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

    if (season == "summer") {
      # Yaz ayları için filtreleme yapar
      season_filter <- "AND strftime('%m', Tarih) IN ('04', '05', '06', '07', '08', '09')"
      days_in_season <- 183  # Yaz sezonundaki gün sayısı (Nisan - Eylül)
    } else if (!is.null(season)&& season == "winter"){
      # Kış ayları için filtreleme yapar
      season_filter <- "AND strftime('%m', Tarih) IN ('01', '02', '03', '10', '11', '12')"
      days_in_season <- 182  # Kış sezonundaki gün sayısı (Ocak, Şubat, Mart, Ekim, Kasım, Aralık)
    }

    else {
      # Diğer sezonlar için uygun filtreleme yapılabilir
      stop("Geçersiz sezon parametresi.")
    }

    query <- paste0("SELECT Istasyon, (SUM(CASE WHEN ", parameter_name, " IS NOT NULL THEN 1 ELSE 0 END) * 100 / ", days_in_season, ") AS veri_mevcudiyet_yuzdesi
                    FROM daily_detail
                    WHERE 1=1 ", season_filter, "
                    GROUP BY Istasyon
                    HAVING veri_mevcudiyet_yuzdesi >= ", threshold, "
                    ORDER BY veri_mevcudiyet_yuzdesi DESC")

    query_result <- dbGetQuery(mydb, query)
    dbDisconnect(mydb)
  } else {
    parameter_name <- gsub('\\"', "", parameter_name)
    query_result <- all_daily_detail_load_from_database(parameter_name) %>%
      select(Istasyon, Tarih, rlang::sym(parameter_name)) %>%
      group_by(Istasyon) %>%
      summarise(veri_mevcudiyet_yuzdesi = round(length(which(!is.na(.data[[parameter_name]]))) / 365 * 100)) %>%
      filter(veri_mevcudiyet_yuzdesi >= threshold) %>%
      arrange(desc(veri_mevcudiyet_yuzdesi), Istasyon)
  }

  return(query_result)
}
