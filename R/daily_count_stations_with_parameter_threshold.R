#' Count Stations for specified parameter from daily_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The season for which data availability should be calculated (Default is NULL). Use "summer" or "winter".
#' @export

daily_count_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season = NULL, verbose = FALSE) {

  parameter_name <- gsub('\\"', "", parameter_name)
  data <- all_daily_detail_load_from_database(parameter_name)

  if (verbose) {
    print("Orijinal veri boyutu:")
    print(dim(data))
    print("Veri özeti:")
    print(summary(data))
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

  result <- nrow(station_counts)

  if (verbose) {
    print("İstasyon sayımı sonuçları:")
    print(station_counts)

    print("Sonuç:")
    print(result)
  }


  return(data.frame(count = result))
}
