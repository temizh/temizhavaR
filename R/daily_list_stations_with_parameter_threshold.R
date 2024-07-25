#' List Stations for Specified Parameter and Season from daily_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The season for which data availability is calculated (default is NULL). Use "summer" or "winter".
#' @return A data frame with stations and their data availability percentage.
#' @export

daily_list_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season = NULL) {

  parameter_name <- gsub('\\"', "", parameter_name)
  data <- all_daily_detail_load_from_database(parameter_name)
  data$Tarih <- as.Date(data$Tarih)

  if (!is.null(season) && season == "summer") {

    summer_months <- c(4, 5, 6, 7, 8, 9)
    data <- data %>% filter(month(Tarih) %in% summer_months)
    days_in_season <- length(unique(data$Tarih))
  }

  else {
    days_in_season <- length(unique(data$Tarih))
  }

  query_result <- data %>%
    select(Istasyon, Tarih, parameter_value = .data[[parameter_name]]) %>%
    group_by(Istasyon) %>%
    summarise(veri_mevcudiyet_yuzdesi = round(length(which(!is.na(parameter_value))) / days_in_season * 100)) %>%
    arrange(desc(veri_mevcudiyet_yuzdesi), Istasyon)

  return(as.data.frame(query_result))

}

