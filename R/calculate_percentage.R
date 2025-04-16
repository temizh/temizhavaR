#' Calculate the percentage of available data
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to calculate the percentage for
#' @param data_in_year The number of data entries in a year
#' @export


calculate_percentage <- function(data, parameter, data_in_year, season = NULL) {

  # # Detect leap year and modify data_in_year accordingly for daily and hourly data
  # if(data_in_year == 365) {
  #   data <- data %>%
  #     summarise(data_count = 
  #       ifelse(Yıl%%4 == 0, 366, 365) # Leap year check
  #     )
  # }
  # else if(data_in_year == 365 * 24) {
  #   data <- data %>%
  #     summarise(data_count = 
  #       ifelse(Yıl%%4 == 0, 366 * 24, 365 * 24) # Leap year check
  #     )
  # }
  # else {
  #   data <- data %>%
  #     summarise(data_count = data_in_year)
  # }
  
  if(!is.null(season)) {
    data <- data %>%
      filter(Ay %in% season)
  }

  data <- data %>%
    summarise(
      data_count = ifelse(data_in_year == 365, 
                          ifelse(Yıl %% 4 == 0, 366, 365),  # Leap year check
                          ifelse(data_in_year == 365 * 24, 
                                 ifelse(Yıl %% 4 == 0, 366 * 24, 365 * 24),  # Leap year check
                                 data_in_year)),  # For other cases
      available_entries = sum(ifelse(!is.na(.data[[parameter]]), 1, 0)),  # Count non-NA values
    ) %>%
    mutate(result = (available_entries / data_count) * 100) %>%  # Calculate percentage
    select(-c(data_count, available_entries))  # Remove intermediate columns

  return(data)
}

