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

  city_query <- dbGetQuery(mydb, "SELECT DISTINCT Sehir FROM location")

  cities <- city_query$Sehir

  overall_avgs <- sapply(cities, function(city_name) {

    station_query <- dbGetQuery(mydb, paste0("SELECT Sehir_Istasyon FROM location WHERE Sehir='", city_name, "'"))

    stations <- station_query$Sehir_Istasyon

    stations_string <- paste0("'", stations, "'", collapse = ", ")

    query <- paste0("SELECT ", parameter, " FROM daily_detail WHERE Istasyon IN (", stations_string, ")")

    daily_data <- dbGetQuery(mydb, query)

    parameter <- gsub('"', '' , parameter)

    overall_avg <- mean(daily_data[[parameter]], na.rm = TRUE)

    return(overall_avg)
  })

  dbDisconnect(mydb)

  result <- data.frame(Sehir = cities, Ortalama = overall_avgs)
  result <- result[!is.na(result$Ortalama), ]
  row.names(result) <- NULL

  return(result)
}
