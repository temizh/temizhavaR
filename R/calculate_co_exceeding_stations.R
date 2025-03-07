#' Calculate CO Exceeding Stations for 8-hour Moving Average
#'
#' This function calculates the number of stations where the maximum daily 8-hour
#' moving average of CO exceeds 10 mg/m3. For valid calculation, at least 18
#' values per day should be present. If a station has less than 90% valid data
#' over the year, its annual average is set to NA.
#'
#' @return A data.frame with the stations and the annual average exceeding the threshold.
#' @export

calculate_co_exceeding_stations <- function() {


  #mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")
  conn <- create_postgres_conn()

  query <- "SELECT Istasyon_modified, Tarih, CO FROM hourly_detail"

  data <- dbGetQuery(conn, query)


  data <- data %>%
    mutate(TarihSaat = as.POSIXct(Tarih, format="%Y-%m-%d %H:%M:%S"),
           date = as.Date(TarihSaat))

  # 8 saatlik hareketli ortalamaların hesaplanması
  data <- data %>%
    arrange(Istasyon_modified, TarihSaat) %>%
    group_by(Istasyon_modified) %>%
    mutate(rolling_avg = zoo::rollapplyr(CO, width = 8, FUN = mean, fill = NA, align = "right")) %>%
    ungroup()

  # Günlük maksimum değerlerin hesaplanması
  daily_max <- data %>%
    group_by(Istasyon_modified, date) %>%
    summarise(valid_counts = sum(!is.na(rolling_avg)),
              max_8hr_avg = if(valid_counts >= 18) max(rolling_avg, na.rm = TRUE) else NA) %>%
    ungroup()

  # İstasyon bazında yıllık veri oranı ve maksimum günlük değerlerin hesaplanması
  annual_data <- daily_max %>%
    group_by(Istasyon_modified) %>%
    summarise(total_days = n(),
              valid_days = sum(!is.na(max_8hr_avg)),
              annual_data_ratio = valid_days / total_days,
              annual_avg_max = if(annual_data_ratio >= 0.90) mean(max_8hr_avg, na.rm = TRUE) else NA) %>%
    ungroup()

  # Eşiği aşan Istasyon_modifiedların belirlenmesi
  exceeding_stations <- annual_data %>%
    filter(!is.na(annual_avg_max) & annual_avg_max > 10) %>%
    select(Istasyon_modified, annual_avg_max)

  disconnect_postgres(conn)


  return(exceeding_stations)
}


