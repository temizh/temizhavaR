#' Calculate the percentage of available data
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to calculate the percentage for
#' @param data_in_year The number of data entries in a year
#' @export


calculate_percentage <- function(data, parameter, data_in_year, season = NULL) {

  if(!is.null(season)) {
    data <- data %>%
      filter(Ay %in% season)
  }

  data <- data %>%
    summarise(
      total_entries = data_in_year,  # Total entries
      available_entries = sum(ifelse(!is.na(.data[[parameter]]), 1, 0)),  # Count non-NA values
    ) %>%
    mutate(result = (available_entries / total_entries) * 100) %>%  # Calculate percentage
    select(-c(total_entries, available_entries))  # Remove intermediate columns

  return(data)
}

