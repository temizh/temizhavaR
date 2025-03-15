#' Calculate AOT40 for a given period
#'
#' This function calculates the AOT40 for a specified period and filters stations
#' with data availability greater than or equal to the threshold.
#'
#' @param start_dates The start dates of the period in "YYYY-MM-DD" format.
#' @param end_dates The end dates of the period in "YYYY-MM-DD" format.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @return A data frame with columns: Istasyon_modified, aot40_measured, measured_hours, aot40_estimated, period.
#' @export
#'
#' @examples
#' calculate_aot40_hourly_threshold(c("2021-04-01", "2021-07-01"), c("2021-06-30", "2021-09-30"),threshold = 90)

hourly_list_aot40_threshold <- function(start_months, end_months, threshold = 90, data_threshold = 0) {
  
  # Function to calculate total hours between start and end month over multiple years
  calculate_total_hours <- function(start_month, end_month) {
    days <- 0
    for (month in start_month:end_month) {
      days <- days + days_in_month(ymd(paste0("2023-", month, "-01")))
    }
    return(days * 12)
  }

  conn <- create_postgres_conn()

  # Her döneme ait sonuçları depolamak için boş bir liste başlatır
  all_results <- list()

  # Belirtilen dönem ve zaman aralığı için O3 verilerini almak için sorgulama işlemi
  query <- "SELECT * FROM hourly_detail WHERE 'O3' IS NOT NULL"

  raw_data <- dbGetQuery(conn, query)

  # Her periyot için iterate işlemi
  for (i in 1:length(start_months)) {

    # Saatlik AOT40'ı hesaplamak için verileri işler
    data <- raw_data %>%
      mutate(Date = as.POSIXct(Tarih, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%  # Convert to date
      mutate(Hour = format(Date, "%H")) %>%  # Extract hour
      filter(Hour > 8 & Hour <= 20) %>% # Filter data between 8 and 20
      mutate(Month = format(Date, "%m")) %>% # Extract month
      filter(Month >= start_months[i] & Month <= end_months[i]) %>% # Filter data between start and end months
      mutate(Year = format(Date, "%Y")) %>% # Extract year
      mutate(excess = ifelse(O3 > 80, O3 - 80, 0)) # Calculate excess

    # İstasyon başına veri kullanılabilirliğini hesaplar
    total_hours <- calculate_total_hours(start_months[i], end_months[i])

    print("A")

    data_availability <- data %>%
      group_by(Istasyon_modified, Year) %>%
      summarise(available_hours = sum(!is.na(O3)),
                data_availability = (available_hours / total_hours) * 100)

    print("B")

    # Veri kullanılabilirliği %90 eşiğini aşan Istasyon_modifiedları filtreler
    filtered_stations <- data_availability %>%
      filter(data_availability >= threshold) %>%
      pull(Istasyon_modified)

    print("C")

    # Filtrelenen Istasyon_modifiedlar için AOT40'ı hesaplar
    aot40_data <- data %>%
      filter(Istasyon_modified %in% filtered_stations) %>%
      group_by(Istasyon_modified, Year) %>%
      summarise(aot40_measured = sum(excess, na.rm = TRUE),
                measured_hours = sum(!is.na(O3)),
                aot40_estimated = aot40_measured * total_hours / measured_hours) %>%
      mutate(months = paste0(start_months[i], "-", end_months[i]) ) %>%
      ungroup()

    print("D")

    # Sonucu listeye ekler
    all_results[[i]] <- aot40_data
  }

  print("E")

  # Tüm dönem sonuçlarını tek bir data.frame'de birleştirir
  final_result <- do.call(rbind, all_results)

  disconnect_postgres(conn)

  return(as.data.frame(final_result))
}
