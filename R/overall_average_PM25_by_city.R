#' Calculate Overall Average PM2.5 for specified parameter from daily_detail for all stations in a city
#'
#' This function calculates the overall average value of a specified parameter from daily_detail data for all stations in a city.
#'
#' @param city_name The name of the city.
#' @param parameter The parameter for which the overall average is calculated.
#' @return The overall average value of the parameter for all stations in the city.
#' @export

calculate_overall_PM25_average_by_city <- function() {

  parameter =  init.temizhavaR()

  YEAR <- options()$temizhavaR.YEAR

  mydb <- dbConnect(RSQLite::SQLite(), file.path(raw_dir, "temiz-hava.sqlite"))

  city_query <- dbGetQuery(mydb, paste0("SELECT DISTINCT Sehir FROM location_", YEAR))
  cities <- city_query$Sehir

  overall_avgs <- lapply(cities, function(city_name) {
    station_query <- dbGetQuery(mydb, paste0("SELECT Istasyonlar FROM location_", YEAR, " WHERE Sehir='", city_name, "'"))
    stations <- station_query$Istasyonlar

    station_data <- lapply(stations, function(station) {
      pm25_data_percentage_query <- paste0("SELECT (SUM(CASE WHEN \"PM2.5\" IS NOT NULL THEN 1 ELSE 0 END) * 100 / 365) AS data_percentage FROM daily_detail WHERE Istasyon='", station, "'")
      pm25_data_percentage <- dbGetQuery(mydb, pm25_data_percentage_query)$data_percentage

      if (!is.na(pm25_data_percentage) && pm25_data_percentage >= 75) {
        #Take the PM2.5 measurement
        pm25_query <- paste0("SELECT AVG(\"PM2.5\") AS Yillik_Ortalama FROM daily_detail WHERE Istasyon='", station, "'")
        pm25 <- dbGetQuery(mydb, pm25_query)

        pm25_values <- data.frame(Istasyon = station, PM25 = pm25$Yillik_Ortalama,
                                  pm25_veri_mevcudiyet_yuzdesi = pm25_data_percentage,
                                  pm10_veri_mevcudiyet_yuzdesi = NA,
                                  pm10_value = NA)

      } else {

        #Estimate PM2.5 from PM10 measurements if PM10 satisfies 90 availability treshold
        pm25_values <- new_pm25_for_city_based_mean(station, verbose = TRUE)
      }

      pm25_values
    })

    station_data <- do.call("rbind", station_data)
    #print(paste(city_name, ", class : ", class(station_data), ", dim : ", dim(station_data) ))
    station_data <- cbind(data.frame(Sehir = NA), station_data)
    station_data$Sehir <- city_name

    #if (station == "Erzurum - Palandöken") browser()
    #if (city_name  == "Erzurum") browser()

    station_data
  })

  dbDisconnect(mydb)

  result <- do.call("rbind", overall_avgs) %>%
    #arrange(desc(PM25)) %>%
    mutate(PM25 = round(PM25, 5)) %>%
    mutate(pm10_value = round(pm10_value, 5))
}
