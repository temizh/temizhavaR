#' Calculate the AOT40 percentage of available data
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to calculate the percentage for
#' @export


calculate_AOT40_percentage <- function(data, parameter, months, days) {

  filtered_data <- data %>%
    filter(!is.na(.data[[parameter]])) %>%
    filter(Saat > 8 & Saat <= 20) %>%
    filter(Ay %in% months)

  # Calculate the percentage of available data
  result <- filtered_data %>%
    summarise(
      result = (n() / (12 * days)) * 100,
      .groups = "drop_last"
    )

  return(result)
}

