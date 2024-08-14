#' Calculate the number of stations with high data availability
#'
#' This function calculates the number of stations with >= threshold data availability
#' for the specified periods.
#'
#' @param start_dates A character vector of start dates in "YYYY-MM-DD" format.
#' @param end_dates A character vector of end dates in "YYYY-MM-DD" format.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @return A data frame with columns: period, high_availability_stations.
#' @export

count_stations_aot40_threshold <- function(start_dates, end_dates, threshold = 90) {


  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  all_results <- data.frame(period = character(), high_availability_stations = integer())

  for (i in 1:length(start_dates)) {
    #Tarihleri Date nesnelerine dönüştürür
    start_date <- as.Date(start_dates[i])
    end_date <- as.Date(end_dates[i])

    #Belirtilen dönem ve zaman aralığı için O3 verilerini almak için sorgular
    query <- paste0("SELECT Istasyon, Tarih, O3
                     FROM hourly_detail
                     WHERE Tarih BETWEEN '", start_date, "' AND '", end_date, "'
                     AND strftime('%H', Tarih) BETWEEN '08' AND '20'")

    data <- dbGetQuery(mydb, query)

    #Veri kullanılabilirliğini hesaplamak için verileri işler
    data <- data %>%
      mutate(hour = hour(ymd_hms(Tarih))) %>%
      filter(hour > 8 & hour <= 20)

    # İstasyon başına veri kullanılabilirliğini hesaplar
    total_hours <- length(seq(start_date, end_date, by = "day")) * 13
    data_availability <- data %>%
      group_by(Istasyon) %>%
      summarise(available_hours = sum(!is.na(O3)),
                data_availability = (available_hours / total_hours) * 100)

    # Veri kullanılabilirliği %90 eşiğini aşan istasyonları filtreler
    high_availability_stations <- data_availability %>%
      filter(data_availability >= threshold) %>%
      nrow()

    # Sonucu veri çerçevesinde saklar
    all_results <- rbind(all_results, data.frame(period = paste(start_date, end_date, sep = " to "),
                                                 high_availability_stations = high_availability_stations))
  }


  dbDisconnect(mydb)

  return(all_results)
}
