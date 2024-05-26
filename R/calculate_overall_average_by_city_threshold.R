#' Calculate Overall Average for specified parameter from daily_detail for all stations in a city
#'
#' This function calculates the overall average value of a specified parameter from daily_detail data for all stations in a city.
#'
#' @param city_name The name of the city.
#' @param parameter The parameter for which the overall average is calculated.
#' @return The overall average value of the parameter for all stations in the city.
#' @export


calculate_overall_average_by_city_threshold <- function(parameter) {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  city_query <- dbGetQuery(mydb, "SELECT DISTINCT Sehir FROM location_2023")
  cities <- city_query$Sehir

  overall_avgs <- sapply(cities, function(city_name) {
    station_query <- dbGetQuery(mydb, paste0("SELECT Istasyonlar FROM location_2023 WHERE Sehir='", city_name, "'"))
    stations <- unlist(strsplit(station_query$Istasyonlar, ","))

    station_avgs <- sapply(stations, function(station) {
      data_percentage_query <- paste0("SELECT (SUM(CASE WHEN \"", parameter, "\" IS NOT NULL THEN 1 ELSE 0 END) * 100 / 365) AS data_percentage FROM daily_detail WHERE Istasyon='", station, "'")
      data_percentage <- dbGetQuery(mydb, data_percentage_query)$data_percentage

      if (!is.na(data_percentage) && data_percentage >= 90) {
        parameter_query <- paste0("SELECT \"", parameter, "\" FROM daily_detail WHERE Istasyon='", station, "'")
        parameter_values <- dbGetQuery(mydb, parameter_query)[[parameter]]
        return(parameter_values)
      } else {
        return(NA)
      }
    }, USE.NAMES = FALSE)

    station_avgs <- unlist(station_avgs)
    station_avgs <- station_avgs[!is.na(station_avgs)]

    if (length(station_avgs) > 0) {
      overall_avg <- mean(station_avgs, na.rm = TRUE)
      return(overall_avg)
    } else {
      return(NA)
    }
  })

  dbDisconnect(mydb)

  result <- data.frame(Sehir = cities, Ortalama = overall_avgs)
  result <- result[!is.na(result$Ortalama), ]
  row.names(result) <- NULL

  return(result)
}

