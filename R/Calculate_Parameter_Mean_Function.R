#' Calculate specified parameter of specified station
#'
#' @param station_name The name of the station to filter the data.
#' @export

calculate_parameter_mean <- function(query_result, parameter_name, threshold = 0.9) {
  # veriler bos degilse
  if (!is.null(query_result) && length(query_result[[parameter_name]]) > 0) {

    parameter_values <- query_result[[parameter_name]]

    # eksik verileri filtrelioruz
    parameter_values <- parameter_values[!is.na(parameter_values) & !is.nan(parameter_values)]

    # threholddan fazlaysa hesaplıyoruz
    if (length(parameter_values) / length(query_result[[parameter_name]]) >= threshold) {
      mean_value <- mean(parameter_values)
      return(mean_value)
    } else {
      cat("Veri yüzde 90'dan fazla değil, ortalama hesaplanamadı.\n")
      return(NA)
    }
  } else {
    cat("Veri bulunamadı veya eksik.\n")
    return(NA)
  }
}
