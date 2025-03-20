#' Calculate the AOT40 percentage of available data
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to calculate the percentage for
#' @export


calculate_AOT40_percentage <- function(data, parameter) {

  data <- data %>%
    filter(Saat > 8 & Saat < 20) %>%
    summarise(
      result = (sum(ifelse(!is.na(.data[[parameter]]), 1, 0)) / (11 * 365)) * 100,
      .groups = "drop_last"
    )

  return(data)
}

