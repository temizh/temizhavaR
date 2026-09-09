#' Calculate the percentage of available data
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to calculate the percentage for
#' @param data_in_year The number of data entries in a year
#' @param season Optional month numbers used for seasonal completeness.
#' @export


calculate_percentage <- function(data, parameter, data_in_year, season = NULL) {
  seasonal_days <- NULL
  if(!is.null(season)) {
    if (any(!season %in% 1:12)) stop("season must contain month numbers from 1 to 12")
    month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    seasonal_days <- sum(month_days[unique(season)])
    data <- data %>%
      filter(Ay %in% season)
  }

  data <- data %>%
    summarise(
      data_count = if (!is.null(seasonal_days)) {
        seasonal_days + ifelse(
          2 %in% season & (min(Yıl, na.rm = TRUE) %% 400 == 0 |
            (min(Yıl, na.rm = TRUE) %% 4 == 0 & min(Yıl, na.rm = TRUE) %% 100 != 0)),
          1,
          0
        )
      } else {
        ifelse(data_in_year == 365,
                          ifelse(min(Yıl, na.rm = TRUE) %% 400 == 0 |
                                   (min(Yıl, na.rm = TRUE) %% 4 == 0 & min(Yıl, na.rm = TRUE) %% 100 != 0), 366, 365),
                          ifelse(data_in_year == 365 * 24, 
                                 ifelse(min(Yıl, na.rm = TRUE) %% 400 == 0 |
                                          (min(Yıl, na.rm = TRUE) %% 4 == 0 & min(Yıl, na.rm = TRUE) %% 100 != 0), 366 * 24, 365 * 24),
                                 data_in_year))
      },
      available_entries = sum(ifelse(!is.na(.data[[parameter]]), 1, 0)),
    ) %>%
    mutate(result = (available_entries / data_count) * 100) %>%
    select(-c(data_count, available_entries))

  return(data)
}
