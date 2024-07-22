#' List Stations with Counts Above Threshold
#'
#' @param data A pair of numbers.
#' @export

list_stations_counts_above_threshold <- function(processed_data, param_name) {
  data <- processed_data$data
  station_name <- processed_data$station_name
  param_names <- processed_data$param_names

  if (!is.null(station_name)) {
    if (param_name %in% param_names) {
      if (param_name %in% colnames(data)) {
        param_data <- data[[param_name]]

        # Verinin yüzde 90'dan fazla olduğu durumu kontrol et
        if (mean(!is.na(param_data)) >= 0.9) {
          num_stations <- length(station_name)
          cat(paste0("Belirtilen parametre için yüzde 90 ve üstü veri alan ", num_stations, " istasyon bulunmaktadır.\n"))
          return(num_stations)
        } else {
          cat("Belirtilen parametre için yüzde 90 ve üstü veri alan herhangi bir istasyon bulunamadi.\n")
        }
      } else {
        cat(paste0(param_name, " parametresi bulunamadi.\n"))
      }
    } else {
      cat(paste0(param_name, " parametresi gecersiz.\n"))
    }
  } else {
    cat("İstasyon adlari bulunamadi.\n")
  }
}
