#' Station Average for daily data
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The season for which data availability is calculated (default is NULL). Use "summer" or "winter".
#' @return A data frame with station names, their data availability percentages, and parameter averages, sorted by data availability.
#' @export

daily_station_average <- function(parameter_name, threshold = 90, season = NULL) {
  parameter_name <- gsub('\\"', "", parameter_name)
  data <- all_daily_detail_load_from_database(parameter_name)

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

  query_result <- data %>%
    group_by(Istasyon_modified) %>%
    summarize(
      total_days = n(),
      non_na_days = sum(!is.na(.data[[parameter_name]])),
      veri_mevcudiyet_yuzdesi = round(non_na_days / days_in_season * 100, 2),
      average = mean(.data[[parameter_name]], na.rm = TRUE)
    ) %>%
    filter(veri_mevcudiyet_yuzdesi >= threshold) %>%
    arrange(desc(veri_mevcudiyet_yuzdesi), desc(average)) %>%
    as.data.frame()

  return(query_result)
}
