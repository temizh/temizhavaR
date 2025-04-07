#' Calculate the average of available data
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to calculate the average for
#' @export


calculate_average <- function(data, parameter, data_in_year) {
  data <- data %>%
    summarise(
      result = mean(.data[[parameter]], na.rm = TRUE)  # Calculate average
    )

  return(data)
}