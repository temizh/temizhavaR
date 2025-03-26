#' Estimate PM2.5
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param data_in_year The number of data entries in a year
#' @export

estimate_PM25 <- function(data, data_in_year) {
  data <- data %>%
    
    summarise(
      total_entries = data_in_year,  # Total entries
      available_PM25_entries = sum(ifelse(!is.na(PM25), 1, 0)),  # Count non-NA values
      available_PM10_entries = sum(ifelse(!is.na(PM10), 1, 0)),  # Count non-NA values
      PM25_average = mean(PM25, na.rm = TRUE), # Calculate average
      PM10_average = mean(PM10, na.rm = TRUE) # Calculate average
    ) %>%

    mutate(PM25_percentage = (available_PM25_entries / total_entries) * 100) %>%  # Calculate percentage
    
    # Estimate PM2.5
    mutate(result = ifelse(PM25_percentage >= 75, PM25_average, PM10_average * 0.6667)) %>%
    
    select(-c(total_entries, available_PM25_entries, available_PM10_entries, PM25_average, PM10_average, PM25_percentage))  # Remove intermediate columns

  return(data)
}