#' Estimate PM2.5
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param data_in_year The number of data entries in a year
#' @export

estimate_PM25 <- function(data, data_in_year) {
  data <- data %>%
    summarise(
      total_entries = ifelse(
        min(Yıl, na.rm = TRUE) %% 400 == 0 |
          (min(Yıl, na.rm = TRUE) %% 4 == 0 &
             min(Yıl, na.rm = TRUE) %% 100 != 0),
        ifelse(
          data_in_year == 365,
          366,
          ifelse(data_in_year == 365 * 24, 366 * 24, data_in_year)
        ),
        data_in_year
      ),
      available_PM25_entries = sum(ifelse(!is.na(PM25), 1, 0)),
      available_PM10_entries = sum(ifelse(!is.na(PM10), 1, 0)),
      PM25_average = mean(PM25, na.rm = TRUE),
      PM10_average = mean(PM10, na.rm = TRUE)
    ) %>%
    mutate(PM25_percentage = (available_PM25_entries / total_entries) * 100) %>%
    mutate(result = ifelse(PM25_percentage >= 75, PM25_average, PM10_average * 0.6667)) %>%
    select(-c(total_entries, available_PM25_entries, available_PM10_entries,
              PM25_average, PM10_average, PM25_percentage))

  return(data)
}
