#' List Stations for specified parameter from daily_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The specific season to consider (e.g., "summer" for nisan, mayıs, haziran, temmuz, ağustos, eylül).
#' @export

daily_list_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season = NULL) {

  if (1) {
    mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

    # Sezon filtresini belirler
    if (!is.null(season) && season == "summer") {
      # Yaz ayları için filtreleme yapar
      season_filter <- "AND strftime('%m', Tarih) IN ('04', '05', '06', '07', '08', '09')"
      days_in_season <- 183  # Yaz sezonundaki gün sayısı (Nisan - Eylül)
    } else {
      # Tüm yıl için
      season_filter <- ""
      days_in_season <- 365  # Yıldaki toplam gün sayısı
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
