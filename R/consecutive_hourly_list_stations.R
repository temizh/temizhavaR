#' List of stations consecutively exceeding a specified parameter from hourly_detail
#'
#' This function finds stations that consecutively exceed a specified threshold value for a given parameter (e.g., PM10 level) for three consecutive hours.
#' It retrieves data from the hourly details table in the database to return a list of stations and the total duration of exceeding the threshold.
#'
#' @param parameter_name The name of the desired parameter. For example, "PM10".
#' @param threshold The threshold value. Default value is 350 µg/m^3.
#' @return List of stations consecutively exceeding the specified parameter for three hours and the total duration of exceedance.
#' @export

consecutive_hourly_list_stations <- function(parameter_name, threshold) {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  query <- paste0("SELECT Istasyon, ", parameter_name, " FROM hourly_detail WHERE ", parameter_name, " IS NOT NULL")
  #query <- paste0("SELECT Istasyon, ", parameter_name, " FROM hourly_detail WHERE Istasyon = 'Adana - Çatalan' AND ", parameter_name, " IS NOT NULL LIMIT 20")

  hourly_stations <- dbGetQuery(mydb, query)

  data <- hourly_stations %>%
    mutate(threshold_exceeded = ifelse(hourly_stations[[parameter_name]] >= threshold, 1, 0))

  consecutive_stations <- data %>%
    mutate(lead1 = lead(threshold_exceeded),
           lead2 = lead(threshold_exceeded, 2)) %>%
    filter(threshold_exceeded == 1 & lead1 == 1 & lead2 == 1) %>%
    select(Istasyon) %>%
    distinct() %>%
    left_join(data %>% filter(threshold_exceeded == 1) %>% count(Istasyon), by = "Istasyon")

  return(consecutive_stations)

  }






