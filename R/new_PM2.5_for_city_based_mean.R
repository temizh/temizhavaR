#' Calculate PM2.5 Values from PM10 Yearly Averages for Stations with Less Than 75% Data
#'
#' This function calculates the PM2.5 values from the PM10 yearly averages for stations with less than 75% data availability.
#'
#' @param parameter_name The name of the parameter (e.g., "PM10") for which the PM2.5 values are calculated.
#' @return A data frame containing the calculated PM2.5 values for stations with less than 75% data availability.
#' @export

new_pm25_for_city_based_mean <- function() {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  threshold <- 75

  threshold_query <- paste0("SELECT Istasyon FROM (SELECT Istasyon, (SUM(CASE WHEN \"PM2.5\" IS NOT NULL THEN 1 ELSE 0 END) * 100 / 365) AS data_percentage FROM daily_detail GROUP BY Istasyon) WHERE data_percentage < ", threshold)

  threshold_stations <- dbGetQuery(mydb, threshold_query)$Istasyon

  stations_string <- paste0("'", threshold_stations, "'", collapse = ", ")

  # PM10 verilerinin yüzde doksanından fazlasını kontrol et
  pm10_threshold_query <- paste0("SELECT Istasyon FROM (SELECT Istasyon, (SUM(CASE WHEN PM10 IS NOT NULL THEN 1 ELSE 0 END) * 100 / COUNT(*)) AS pm10_percentage FROM daily_detail WHERE Istasyon IN (", stations_string, ") GROUP BY Istasyon) WHERE pm10_percentage >= 90")

  pm10_threshold_stations <- dbGetQuery(mydb, pm10_threshold_query)$Istasyon

  if(length(pm10_threshold_stations) > 0) {
    stations_string <- paste0("'", pm10_threshold_stations, "'", collapse = ", ")

    pm10_query <- paste0("SELECT Istasyon, AVG(PM10) AS Yillik_Ortalama FROM daily_detail WHERE Istasyon IN (", stations_string, ") GROUP BY Istasyon")

    pm10_query_result <- dbGetQuery(mydb, pm10_query)

    pm25_values <- pm10_query_result$Yillik_Ortalama * 0.6667

    pm25_result <- data.frame(Istasyon = pm10_query_result$Istasyon, PM25 = pm25_values)

    dbDisconnect(mydb)

    return(pm25_result)
  } else {
    dbDisconnect(mydb)
    cat("Yüzde doksan ve üzeri PM10 verisi sağlanmadığı için PM25 hesaplanamıyor.")
    return(NULL)
  }
}
