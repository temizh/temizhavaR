#' Count Stations meeting threshold criteria for specified parameter
#'
#' @param parameter_name The name of the parameter
#' @param data_type Either 'daily' or 'hourly'
#' @param threshold The threshold percentage for data availability (default is 90)
#' @export
count_stations_with_parameter_threshold <- function(parameter_name, data_type = "daily", threshold = 90, 
                                                  verbose = FALSE, season = NULL, until_year = 2023) {
  tryCatch({
    conn <- create_postgres_conn()

    query <- if(data_type == "daily") {
      sprintf('SELECT "Tarih", "Istasyon_modified", "%s" FROM daily_detail', parameter_name)
    } else {
      sprintf('SELECT "Tarih", "Istasyon_modified", "%s" FROM hourly_detail', parameter_name)
    }

    if(verbose) print(paste("Executing query:", query))

    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)

    if(nrow(data) == 0) {
      return(create_empty_result(threshold, data_type, until_year))
    }

    data$date <- as.POSIXct(data$Tarih, format = "%Y-%m-%d %H:%M:%S")
    data$year <- as.numeric(format(data$date, "%Y"))

    if(!is.null(season)) {
      month <- as.numeric(format(data$date, "%m"))
      data <- data %>%
        filter(
          if(season == "winter") month %in% c(12,1,2)
          else if(season == "summer") month %in% c(6,7,8)
          else TRUE
        )
    }

    result <- data %>%
      group_by(year, Istasyon_modified) %>%
      summarise(
        total_records = n(),
        valid_records = sum(!is.na(!!sym(parameter_name))),
        data_percentage = round(valid_records / total_records * 100, 2),
        .groups = "drop"
      ) %>%
      filter(data_percentage >= threshold) %>%
      group_by(year) %>%
      summarise(
        station_count = n(),
        stations_checked = n_distinct(Istasyon_modified),
        period_count = max(total_records),  
        threshold = threshold,
        message = if(n() > 0) "OK" else "No stations meet threshold",
        season = ifelse(is.null(season), "all", season),
        .groups = "drop"
      ) %>%
      filter(year <= until_year) %>%
      arrange(year)

    if (data_type == "daily") {
      names(result)[names(result) == "period_count"] <- "days_in_period"
    } else {
      names(result)[names(result) == "period_count"] <- "hours_in_period"
    }

    full_years <- data.frame(year = 2014:until_year)
    result <- merge(full_years, result, by = "year", all.x = TRUE)
    result[is.na(result)] <- 0
    result$message[result$station_count == 0] <- "No stations meet threshold"
    result$season[is.na(result$season)] <- ifelse(is.null(season), "all", season)

    if(verbose) {
      print("Final results:")
      print(result)
    }

    return(result)

  }, error = function(e) {
    warning("Error in count calculation: ", e$message)
    period_col <- if(data_type == "daily") "days_in_period" else "hours_in_period"
    result <- data.frame(
      year = 2014:until_year,
      station_count = 0,
      stations_checked = 0,
      threshold = threshold,
      message = paste("Error:", e$message),
      season = ifelse(is.null(season), "all", season)
    )
    result[[period_col]] <- 0
    return(result)
  })
}

create_empty_result <- function(threshold, data_type = "daily", until_year) {
  period_col <- if(data_type == "daily") "days_in_period" else "hours_in_period"
  result <- data.frame(
    year = 2014:until_year,
    station_count = 0,
    stations_checked = 0,
    threshold = threshold,
    message = "No data available",
    season = "all"
  )
  result[[period_col]] <- 0
  return(result)
}
