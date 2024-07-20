#' Calculate the number of stations exceeding 120 µg/m³ based on 8-hour rolling averages for a specified parameter
#'
#' This function calculates the 8-hour rolling averages for all stations and identifies
#' the number of stations with daily maximum values exceeding 120 µg/m³ for a specified parameter.
#'
#' @param parameter_name The name of the parameter (e.g., "O3", "PM10") to use for the calculations.
#' @return A data frame with a single column `num_exceeding_stations` representing the number of stations with daily maximum 8-hour rolling averages exceeding 120 µg/m³.
#' @export

calculate_aot40_exceeding_stations <- function(parameter_name) {


  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  # Verilen parametre için verileri saatlik detay tablosundan almak için sorgulama işlemi
  query <- paste0("SELECT Istasyon, Tarih, ", parameter_name, " FROM hourly_detail")

  data <- dbGetQuery(mydb, query)

  # Tarih ve saat bilgilerini işleme
  data <- data %>%
    mutate(TarihSaat = as.POSIXct(Tarih, format="%Y-%m-%d %H:%M:%S"),
           hour = as.numeric(format(TarihSaat, "%H"))) %>%
    arrange(Istasyon, TarihSaat)

  # 8 saatlik ortalamaların hesaplanması
  data <- data %>%
    group_by(Istasyon) %>%
    mutate(rolling_avg = zoo::rollapply(get(parameter_name), width = 8, FUN = mean, fill = NA, align = 'right', partial = TRUE))

  # Günlük maksimum 8 saatlik ortalama
  daily_max_8h_avg <- data %>%
    mutate(date = as.Date(TarihSaat)) %>%
    group_by(Istasyon, date) %>%
    summarise(max_8h_avg = if(all(is.na(rolling_avg))) NA else max(rolling_avg, na.rm = TRUE)) %>%
    ungroup()

  # 120 µg/m³'ü aşan istasyonları belirleme
  exceed_stations <- daily_max_8h_avg %>%
    filter(!is.na(max_8h_avg) & max_8h_avg > 120) %>%
    select(Istasyon) %>%
    distinct()

  # Aşan istasyon sayısını hesaplama
  num_exceeding_stations <- nrow(exceed_stations)

  # Sonuçları data.frame nesnesine dönüştürme
  result_df <- data.frame(num_exceeding_stations = num_exceeding_stations)


  dbDisconnect(mydb)

  return(result_df)
}



