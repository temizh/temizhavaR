#' Calculate the consecutive exceedance of available data
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to calculate the exceedance for
#' @param threshold The threshold value to compare the data with
#' @param consecutive The number of consecutive exceedances
#' @param exceedance_direction The direction of the exceedance (above or below)
#' @export

calculate_consecutive_exceedance <- function(data, parameter, threshold, consecutive, exceedance_direction) {
  if (!exceedance_direction %in% c("above", "below")) {
    stop("Invalid exceedance direction. Please use 'above' or 'below'.")
  }
  
  # Calculate consecutive exceedances
  data <- data %>%
    mutate(exceedance = ifelse(exceedance_direction == "above", .data[[parameter]] > threshold, .data[[parameter]] < threshold)) %>%
    group_by(group = cumsum(ifelse(exceedance, 0, 1)), .add = TRUE) %>% # Group by non-exceedance breaks
    summarise(consecutive_count = sum(ifelse(exceedance, 1, 0)), .groups = "drop_last") %>% # Count consecutive exceedances
    filter(consecutive_count >= consecutive) %>% # Filter for consecutive exceedances
    summarise(result = n(), .groups = 'drop') # Count the number of exceedances
  
  return(data)
}