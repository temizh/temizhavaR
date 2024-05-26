#' Calculate Overall Average for specified parameter from daily_detail for all stations in a city
#'
#' This function calculates the overall average value of a specified parameter from daily_detail data for all stations in a city.
#'
#' @param city_name The name of the city.
#' @param parameter The parameter for which the overall average is calculated.
#' @return The overall average value of the parameter for all stations in the city.
#' @export


calculate_overall_average_by_city <- function(parameter) {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

 city_query <- dbGetQuery(mydb, "SELECT DISTINCT Sehir FROM location_2023")

 cities <- city_query$Sehir

 overall_avgs <- sapply(cities, function(city_name) {
  station_query <- dbGetQuery(mydb, paste0("SELECT Istasyonlar FROM location_2023 WHERE Sehir='", city_name, "'"))
  stations <- station_query$Istasyonlar

  station_avgs <- sapply(stations, function(station) {
    pm25_data_percentage_query <- paste0("SELECT (SUM(CASE WHEN \"PM2.5\" IS NOT NULL THEN 1 ELSE 0 END) * 100 / 365) AS data_percentage FROM daily_detail WHERE Istasyon='", station, "'")
    pm25_data_percentage <- dbGetQuery(mydb, pm25_data_percentage_query)$data_percentage

    if (!is.na(pm25_data_percentage) && pm25_data_percentage >= 75) {
      pm25_query <- paste0("SELECT \"PM2.5\" FROM daily_detail WHERE Istasyon='", station, "'")
      pm25_values <- dbGetQuery(mydb, pm25_query)$PM2.5
      return(pm25_values)
    } else {
      pm25_from_pm10_df <- new_pm25_from_pm10_yearly_average()
      pm25_from_pm10_value <- pm25_from_pm10_df$PM25[pm25_from_pm10_df$Istasyon == station]
      if (length(pm25_from_pm10_value) > 0) {
        return(pm25_from_pm10_value)
      } else {
        return(NA)
      }
    }
  }, USE.NAMES = FALSE)

  station_avgs <- unlist(station_avgs)
  station_avgs <- station_avgs[!is.na(station_avgs)]

  overall_avg <- mean(station_avgs, na.rm = TRUE)
  return(overall_avg)
})

dbDisconnect(mydb)

result <- data.frame(Sehir = cities, Ortalama = overall_avgs)
result <- result[!is.na(result$Ortalama), ]
row.names(result) <- NULL

return(result)
}

















# overall_avgs <- sapply(cities, function(city_name) {
#
#   station_query <- dbGetQuery(mydb, paste0("SELECT Istasyonlar FROM location_2023 WHERE Sehir='", city_name, "'"))
#
#   stations <- station_query$Istasyonlar
#
#   stations_string <- paste0("'", stations, "'", collapse = ", ")
#
#   query <- paste0("SELECT ", parameter, " FROM daily_detail WHERE Istasyon IN (", stations_string, ")")
#
#   daily_data <- dbGetQuery(mydb, query)
#
#   parameter <- gsub('"', '' , parameter)
#
#   overall_avg <- mean(daily_data[[parameter]], na.rm = TRUE)
#
#   return(overall_avg)
# })
#
# dbDisconnect(mydb)
#
# result <- data.frame(Sehir = cities, Ortalama = overall_avgs)
# result <- result[!is.na(result$Ortalama), ]
# row.names(result) <- NULL
#
# return(result)
# }




















# calculate_overall_average_by_city <- function(parameter) {
#
#   mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")
#
#   city_query <- dbGetQuery(mydb, "SELECT DISTINCT Sehir FROM location")
#
#   cities <- city_query$Sehir
#
#   overall_avgs <- sapply(cities, function(city_name) {
#
#     station_query <- dbGetQuery(mydb, paste0("SELECT Sehir_Istasyon FROM location WHERE Sehir='", city_name, "'"))
#
#     stations <- station_query$Sehir_Istasyon
#
#     stations_string <- paste0("'", stations, "'", collapse = ", ")
#
#     query <- paste0("SELECT ", parameter, " FROM daily_detail WHERE Istasyon IN (", stations_string, ")")
#
#     daily_data <- dbGetQuery(mydb, query)
#
#     parameter <- gsub('"', '' , parameter)
#
#     overall_avg <- mean(daily_data[[parameter]], na.rm = TRUE)
#
#     return(overall_avg)
#   })
#
#   dbDisconnect(mydb)
#
#   result <- data.frame(Sehir = cities, Ortalama = overall_avgs)
#   result <- result[!is.na(result$Ortalama), ]
#   row.names(result) <- NULL
#
#   return(result)
# }
