#' Count Stations for specified parameter from daily_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The season for which data availability should be calculated (Default is NULL). Use "summer" or "winter".
#' @export

daily_count_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season = NULL) {

  daily_data <- all_daily_detail_load_from_database(parameter_name)

  daily_data <- daily_data %>%
    mutate(Tarih = ymd_hms(Tarih),
           year = year(Tarih),
           month = month(Tarih),
           day = day(Tarih))

  if (!is.null(season) && season == "summer") {
    daily_data <- daily_data %>%
      filter(month %in% c(4, 5, 6, 7, 8, 9))
  }


  station_counts <- daily_data %>%
    group_by(Istasyon) %>%
    summarize(total_days = n_distinct(Tarih),
              available_days = sum(!is.na(.data[[parameter_name]]))) %>%
    ungroup() %>%
    mutate(data_percentage = (available_days / total_days) * 100) %>%
    filter(data_percentage >= threshold)

  return(data.frame(nrow(station_counts)))
}

