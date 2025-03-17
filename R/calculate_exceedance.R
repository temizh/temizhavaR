#' Calculate the exceedance of available data
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to calculate the exceedance for
#' @param threshold The threshold value to compare the data with
#' @export


calculate_exceedance <- function(data, parameter, threshold, exceedance_direction) {
  if(exceedance_direction == "above") {
    data <- data %>%
      summarise(
        result = sum(ifelse(.data[[parameter]] > threshold, 1, 0), na.rm = TRUE)  # Calculate exceedance
      )
  } else if(exceedance_direction == "below") {
    data <- data %>%
      summarise(
        result = sum(ifelse(.data[[parameter]] < threshold, 1, 0), na.rm = TRUE)  # Calculate exceedance
      )
  } else {
    stop("Invalid exceedance direction. Please use 'above' or 'below'.")
  }

  return(data)
}