#' Count Stations for specified parameter from daily_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The season for which data availability should be calculated (Default is NULL). Use "summer" or "winter".
#' @export

daily_count_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season = NULL) {
  daily_data <- all_daily_detail_load_from_database(parameter_name)

  # Sütun adlarını ve veri türlerini yazdırır
  print("Sütun adları:")
  print(names(daily_data))
  print("Sütun veri türleri:")
  print(sapply(daily_data, class))

  # İlk birkaç satırı yazdırır
  print("İlk birkaç satır:")
  print(head(daily_data))

  # Sütun adlarındaki fazladan tırnak işaretlerini ve boşlukları kaldırır
  names(daily_data) <- gsub("^\"|\"$", "", names(daily_data))
  names(daily_data) <- trimws(names(daily_data))

  # parameter_name'deki tırnak işaretlerini ve boşlukları kaldırır
  parameter_name <- gsub("^\"|\"$", "", parameter_name)
  parameter_name <- trimws(parameter_name)

  # Parametre adının sütun adları arasında olup olmadığını kontrol eder
  if (!(parameter_name %in% names(daily_data))) {
    stop(paste("Parametre '", parameter_name, "' veri setinde bulunamadı. Mevcut sütunlar:", paste(names(daily_data), collapse=", ")))
  }

  daily_data <- daily_data %>%
    mutate(Tarih = as.Date(Tarih),
           year = year(Tarih),
           month = month(Tarih),
           day = day(Tarih))

  if (!is.null(season)) {
    if (season == "summer") {
      daily_data <- daily_data %>%
        filter(month %in% c(4, 5, 6, 7, 8, 9))
    } else if (season == "winter") {
      daily_data <- daily_data %>%
        filter(month %in% c(1, 2, 3, 10, 11, 12))
    }
  }

  station_counts <- daily_data %>%
    group_by(Istasyon) %>%
    summarize(total_days = n_distinct(Tarih),
              available_days = sum(!is.na(.data[[parameter_name]]))) %>%
    ungroup() %>%
    mutate(data_percentage = (available_days / total_days) * 100) %>%
    filter(data_percentage >= threshold)

  return(data.frame(count = nrow(station_counts)))
}
