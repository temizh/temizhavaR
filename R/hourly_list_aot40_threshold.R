#' Calculate AOT40 for a given period
#'
#' This function calculates the AOT40 for a specified period and filters stations
#' with data availability greater than or equal to the threshold.
#'
#' @param start_dates The start dates of the period in "YYYY-MM-DD" format.
#' @param end_dates The end dates of the period in "YYYY-MM-DD" format.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @return A data frame with columns: Istasyon, aot40_measured, measured_hours, aot40_estimated, period.
#' @export
#'
#' @examples
#' calculate_aot40_hourly_threshold(c("2021-04-01", "2021-07-01"), c("2021-06-30", "2021-09-30"),threshold = 90)

hourly_list_aot40_threshold <- function(start_dates, end_dates, threshold = 90) {
  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  # Her döneme ait sonuçları depolamak için boş bir liste başlatır
  all_results <- list()

  # Her periyot için iterate işlemi
  for (i in 1:length(start_dates)) {
    # Tarihleri Date nesnelerine dönüştürür
    start_date <- as.Date(start_dates[i])
    end_date <- as.Date(end_dates[i])

    # Belirtilen dönem ve zaman aralığı için O3 verilerini almak için sorgulama işlemi
    query <- paste0("SELECT Istasyon, Tarih, O3
                     FROM hourly_detail
                     WHERE Tarih BETWEEN '", start_date, "' AND '", end_date,
                     "' AND strftime('%H', Tarih) BETWEEN '08' AND '20'")

    data <- dbGetQuery(mydb, query)

    # Saatlik AOT40'ı hesaplamak için verileri işler
    data <- data %>%
      mutate(TarihSaat = ymd_hms(Tarih),
             hour = hour(TarihSaat)) %>%
      filter(hour > 8 & hour <= 20) %>%
      mutate(excess = ifelse(O3 > 80, O3 - 80, 0))

    # İstasyon başına veri kullanılabilirliğini hesaplar
    total_hours <- length(seq(start_date, end_date, by = "day")) * 13
    data_availability <- data %>%
      group_by(Istasyon) %>%
      summarise(available_hours = sum(!is.na(O3)),
                data_availability = (available_hours / total_hours) * 100)

    # Veri kullanılabilirliği %90 eşiğini aşan istasyonları filtreler
    filtered_stations <- data_availability %>%
      filter(data_availability >= threshold) %>%
      pull(Istasyon)

    # Filtrelenen istasyonlar için AOT40'ı hesaplar
    aot40_data <- data %>%
      filter(Istasyon %in% filtered_stations) %>%
      group_by(Istasyon) %>%
      summarise(aot40_measured = sum(excess, na.rm = TRUE),
                measured_hours = sum(!is.na(O3)),
                aot40_estimated = aot40_measured * total_hours / measured_hours) %>%
      mutate(period = paste(format(start_date, "%b"), "-", format(end_date, "%b"))) %>%
      ungroup()

    # Sonucu listeye ekler
    all_results[[i]] <- aot40_data
  }

  # Tüm dönem sonuçlarını tek bir data.frame'de birleştirir
  final_result <- do.call(rbind, all_results)

  dbDisconnect(mydb)

  return(as.data.frame(final_result))
}
