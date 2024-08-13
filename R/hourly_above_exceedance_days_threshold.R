#' Calculate Exceedance Days for specified parameter from hourly_detail for all stations
#'
#' This function calculates the number of days exceeding a specified threshold for a given parameter from daily_detail data for all stations.
#'
#' @param daily_data The daily detail data containing parameter values for each day.
#' @param parameter The parameter for which the exceedance days are calculated.
#' @param threshold The threshold value for the parameter.
#' @return A data frame containing the number of days exceeding the specified threshold for the parameter for each station.
#' @export

hourly_above_exceedance_days_threshold <- function(parameter, threshold) {
  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  # Sadece yeterli veri olan istasyonları seç
  stations_query <- paste0("SELECT Istasyon
                            FROM hourly_detail
                            GROUP BY Istasyon
                            HAVING (SUM(CASE WHEN ", parameter, " IS NOT NULL THEN 1 ELSE 0 END) * 100 / 8761) >= 90")
  stations <- dbGetQuery(mydb, stations_query)

  # Eşik değeri aşıp aşmadığını kontrol eden sorgu
  query <- paste0("SELECT Istasyon, DATE(Tarih) AS Date, ", parameter, " > ", threshold, " AS ExceedsThreshold
                   FROM hourly_detail
                   WHERE Istasyon IN ('", paste(stations$Istasyon, collapse = "','"), "')")

  query_result <- dbGetQuery(mydb, query)

  dbDisconnect(mydb)

  # Aşım günlerini hesapla
  exceedance_days <- query_result %>%
    group_by(Istasyon, Date) %>%
    summarise(ExceedsThreshold = any(ExceedsThreshold)) %>%
    group_by(Istasyon) %>%
    summarise(ExceedanceDays = sum(ExceedsThreshold, na.rm = TRUE)) %>%
    as.data.frame()

  return(exceedance_days)
}
