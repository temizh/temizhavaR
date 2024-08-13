#' Calculate exceedance days for a specified parameter
#'
#' This function calculates the number of days a specified parameter exceeds a given threshold value
#' more than a specified number of times. The parameter can be any valid pollutant column in the dataset.
#'
#'@param parameter_name A character string specifying the parameter to check (e.g., "SO2", "NO2", etc.).
#'@param threshold A numeric value representing the threshold value (default is 125 µg/m³).
#'@param exceedance_count An integer representing the number of times the threshold should be exceeded (default is 3).
#'@export

exceedance_days_3_threshold <- function(parameter_name, threshold = 125, exceedance_count = 3) {
  # SQLite veritabanına bağlan
  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  # daily_detail tablosunu oku
  daily_data <- dbReadTable(mydb, "daily_detail")

  # Bağlantıyı kapat
  dbDisconnect(mydb)

  # Sütun adlarındaki fazladan tırnak işaretlerini ve boşlukları kaldır
  names(daily_data) <- gsub("^\"|\"$", "", names(daily_data))
  names(daily_data) <- trimws(names(daily_data))

  # Parametre adının sütun adları arasında olup olmadığını kontrol et
  if (!(parameter_name %in% names(daily_data))) {
    stop(paste("Parametre '", parameter_name, "' veri setinde bulunamadı. Mevcut sütunlar:", paste(names(daily_data), collapse=", ")))
  }

  # Verileri filtrele ve aşım günlerini hesapla
  exceedance_days <- daily_data %>%
    filter(!is.na(.data[[parameter_name]])) %>%
    mutate(ExceedsThreshold = .data[[parameter_name]] > threshold) %>%
    group_by(Istasyon) %>%
    summarise(ExceedanceCount = sum(ExceedsThreshold, na.rm = TRUE)) %>%
    filter(ExceedanceCount > exceedance_count) %>%
    as.data.frame()

  return(exceedance_days)
}
