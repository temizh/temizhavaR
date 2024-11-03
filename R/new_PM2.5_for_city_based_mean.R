#' Calculate PM2.5 Values from PM10 Yearly Averages for Stations with Less Than 75% Data
#'
#' This function calculates the PM2.5 values from the PM10 yearly averages for stations with less than 75% data availability.
#'
#' @param parameter_name The name of the parameter (e.g., "PM10") for which the PM2.5 values are calculated.
#' @return A data frame containing the calculated PM2.5 values for stations with less than 75% data availability.
#' @export

new_pm25_for_city_based_mean <- function(station, verbose = FALSE) {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  pm25_threshold <- 75
  pm10_threshold <- 90

  threshold_query <- paste0("SELECT Istasyon, data_percentage FROM (SELECT Istasyon, (SUM(CASE WHEN \"PM2.5\" IS NOT NULL THEN 1 ELSE 0 END) * 100 / 365) AS data_percentage FROM daily_detail GROUP BY Istasyon) WHERE data_percentage < ", pm25_threshold)
  threshold_stations <- dbGetQuery(mydb, threshold_query)

  # PM10 verilerinin yüzde doksanından fazlasını kontrol et
  #stations_string <- paste0("'", threshold_stations, "'", collapse = ", ")
  #pm10_threshold_query <- paste0("SELECT Istasyon, pm10_percentage FROM (SELECT Istasyon, (SUM(CASE WHEN PM10 IS NOT NULL THEN 1 ELSE 0 END) * 100 / COUNT(*)) AS pm10_percentage FROM daily_detail WHERE Istasyon IN (", stations_string, ") GROUP BY Istasyon) WHERE pm10_percentage >= 90")
  #pm10_threshold_stations <- dbGetQuery(mydb, pm10_threshold_query)
  #pm10_threshold_stations <- pm10_threshold_stations$Istasyon
  pm10_threshold_stations <- daily_list_stations_with_parameter_threshold("PM10", threshold = pm10_threshold, verbose = FALSE)

  #if (!grepl(station, threshold_stations$Istasyon)) {
  #  threshold_stations$Istasyon <- NA
  #}
  pm25_threshold_stations <- threshold_stations %>% dplyr::filter(Istasyon==station)
  pm10_threshold_stations <- pm10_threshold_stations %>% dplyr::filter(Istasyon==station)

  if (pm10_threshold_stations$veri_mevcudiyet_yuzdesi < pm10_threshold) {
    if (verbose)
      print(paste(station, ": Yüzde doksan ve üzeri PM10 verisi sağlanmadığı için PM25 hesaplanamıyor."))

    pm10_values <- NA
  } else {
    pm10_query <- paste0("SELECT Istasyon, AVG(PM10) AS Yillik_Ortalama FROM daily_detail GROUP BY Istasyon")
    pm10_query_result <- dbGetQuery(mydb, pm10_query)
    pm10_query_result <- pm10_query_result %>% dplyr::filter(grepl(station, Istasyon))
    pm10_values <- pm10_query_result$Yillik_Ortalama
  }

  pm25_values <-pm10_values * 0.6667
  pm25_result <- data.frame(Istasyon = station, PM25 = pm25_values,
                            pm25_veri_mevcudiyet_yuzdesi = pm25_threshold_stations$data_percentage,
                            pm10_veri_mevcudiyet_yuzdesi = pm10_threshold_stations$veri_mevcudiyet_yuzdesi,
                            pm10_value = pm10_values)

  #stations_string <- paste0("'", pm10_threshold_stations, "'", collapse = ", ")
  #pm10_query <- paste0("SELECT Istasyon, AVG(PM10) AS Yillik_Ortalama FROM daily_detail WHERE Istasyon IN (", stations_string, ") GROUP BY Istasyon")
  #pm10_query_result <- dbGetQuery(mydb, pm10_query)

  dbDisconnect(mydb)

  return(pm25_result)
}
