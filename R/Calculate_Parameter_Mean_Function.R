#' Calculate mean of specified parameter
#'
#' @param station_name The name of the station to filter the data.
#' @export

calculate_parameter_mean <- function(data, parameter, threshold, total_days, verbose = FALSE) {

  parameter <- gsub("'", "", parameter)

  parameter_data <- data[[parameter]]


  non_na_count <- sum(!is.na(parameter_data))

  if (non_na_count >= threshold * total_days) {
    mean_value <- round(mean(parameter_data, na.rm = TRUE), digits = 2)
    percentage <- (non_na_count / total_days) * 100
    if (verbose) {
      print(paste(parameter, "icin yeterli veri mevcut."))
      print(paste("Ortalama:", mean_value))
    }
  } else {

      mean_value <- NA
      percentage <- (non_na_count / total_days) * 100
      if (verbose) {
        print(paste(parameter, "icin yeterli veri mevcut degil."))
        print(paste("Var olan veri yüzdesi:", percentage, "%"))
      }
    }

  #daily_percentage <- non_na_count / total_days * 100

  #return(list(mean_value = mean_value, percentage = percentage, daily_percentage = daily_percentage))
  return(list(mean_value = mean_value, percentage = percentage))
}
















# calculate_parameter_mean <- function(query_result, parameter_name, threshold = 0.9) {
#   # veriler bos degilse
#   if (!is.null(query_result) && length(query_result[[parameter_name]]) > 0) {
#
#     parameter_values <- query_result[[parameter_name]]
#
#     # eksik verileri filtrelioruz
#     parameter_values <- parameter_values[!is.na(parameter_values) & !is.nan(parameter_values)]
#
#     # threholddan fazlaysa hesaplıyoruz
#     if (length(parameter_values) / length(query_result[[parameter_name]]) >= threshold) {
#       mean_value <- mean(parameter_values)
#       return(mean_value)
#     } else {
#       cat("Veri yüzde 90'dan fazla değil, ortalama hesaplanamadı.\n")
#       return(NA)
#     }
#   } else {
#     cat("Veri bulunamadı veya eksik.\n")
#     return(NA)
#   }
# }
