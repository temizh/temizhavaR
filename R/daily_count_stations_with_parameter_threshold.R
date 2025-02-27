#' Count Stations for specified parameter from daily_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The season for which data availability should be calculated (Default is NULL). Use "summer" or "winter".
#' @export

daily_count_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season = NULL, verbose = FALSE) {
  tryCatch({
    parameter_name <- gsub('\\"', "", parameter_name)
    data <- all_daily_detail_load_from_database(parameter_name)

    if (verbose) {
      print("Parameter name:")
      print(parameter_name)
      print("Initial data dimensions:")
      print(dim(data))
    }

    if (is.null(data) || nrow(data) == 0) {
      return(data.frame(
        count = 0,
        stations_checked = 0,
        threshold = threshold,
        days_in_period = 0,
        message = "No data available"
      ))
    }

    data$Tarih <- as.Date(data$Tarih)

    if (!is.null(season)) {
      if (season == "summer") {
        summer_months <- c(4, 5, 6, 7, 8, 9)
        data <- data %>% filter(month(Tarih) %in% summer_months)
      } else if (season == "winter") {
        winter_months <- c(1, 2, 3, 10, 11, 12)
        data <- data %>% filter(month(Tarih) %in% winter_months)
      }
    }

    days_in_season <- length(unique(data$Tarih))

    if (days_in_season == 0) {
      return(data.frame(
        count = 0,
        stations_checked = 0,
        threshold = threshold,
        days_in_period = 0,
        message = "No days in season"
      ))
    }

    if (verbose) {
      print("Sezon filtrelemesi sonrası veri boyutu:")
      print(dim(data))

      print("Sezondaki gün sayısı:")
      print(days_in_season)
    }

    station_counts <- data %>%
      group_by(Istasyon) %>%
      summarise(
        total_days = n(),
        non_na_days = sum(!is.na(.data[[parameter_name]])),
        veri_mevcudiyet_yuzdesi = round(non_na_days / days_in_season * 100, 2)
      ) %>%
      filter(veri_mevcudiyet_yuzdesi >= threshold)

    if (verbose) {
      print("Station counts before filtering:")
      print(station_counts)
    }

    result <- nrow(station_counts)

    return(data.frame(
      count = result,
      stations_checked = n_distinct(data$Istasyon),
      threshold = threshold,
      days_in_period = days_in_season,
      message = if(result == 0) "No stations meet threshold" else "OK"
    ))

  }, error = function(e) {
    warning("Error in count calculation: ", e$message)
    return(data.frame(
      count = 0,
      stations_checked = 0,
      threshold = threshold,
      days_in_period = 0,
      message = paste("Error:", e$message)
    ))
  })
}
