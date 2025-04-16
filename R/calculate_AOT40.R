#' Calculate the AOT40
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to calculate the AOT40 for
#' @param months The months to calculate the AOT40 for
#' @export


calculate_AOT40 <- function(data, parameter, months) {

  data <- data %>%
    filter(Saat > 8 & Saat < 20) %>%
    filter(Ay %in% months) %>%
    group_by(Tarih, .add = TRUE) %>%
    summarise(
      AOT40 = sum(ifelse(.data[[parameter]] > 40, .data[[parameter]] - 40, 0)),
      .groups = "drop_last"
    ) %>%
    summarise(
      result = sum(AOT40, na.rm = TRUE),
      .groups = "drop_last"
    )

  return(data)
}

