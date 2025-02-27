#' Calculate the total AOT40 values for vegetation protection within specified date ranges
#'
#' This function calculates the total AOT40 values for vegetation protection based on the hourly ozone data.
#' It evaluates whether the AOT40 value exceeds the specified thresholds within specified date ranges.
#'
#' @param parameter_name The name of the parameter (e.g., "O3") to use for the calculations.
#' @param start_month The starting month (e.g., "04" for April) for the calculation.
#' @param end_month The ending month (e.g., "09" for September) for the calculation.
#' @param thresholds A vector of threshold values to check for exceedance (e.g., c(6000, 18000, 20000)).
#' @param period A label for the period (e.g., "May-Jul" or "Apr-Sep") to distinguish the results.
#' @return A data frame with columns: Istasyon, period, total_AOT40, exceeds_thresholds.
#' @export

calculate_aot40_vegetation_and_forest_protection <- function(parameter_name, start_month, end_month, thresholds, period) {

  conn <- create_postgres_conn()

  # Verilen parametre ve ay aralığı için verileri saatlik detay tablosundan almak için sorgulama işlemi
  query <- paste0("SELECT Istasyon, Tarih, ", parameter_name, " FROM hourly_detail WHERE strftime('%m', Tarih) BETWEEN '", start_month, "' AND '", end_month, "'")

  data <- dbGetQuery(conn, query)

  # Tarih ve saat bilgilerini işleme
  data <- data %>%
    mutate(TarihSaat = as.POSIXct(Tarih, format="%Y-%m-%d %H:%M:%S"),
           hour = as.numeric(format(TarihSaat, "%H"))) %>%
    arrange(Istasyon, TarihSaat)

  # AOT40 değerlerinin hesaplanması (08:00 - 20:00 saatleri arası)
  data <- data %>%
    filter(hour > 8 & hour <= 20 & !is.na(get(parameter_name)) & get(parameter_name) > 80) %>%
    mutate(AOT40 = get(parameter_name) - 80)

  # İstasyonlar bazında toplam AOT40 değerinin hesaplanması
  total_AOT40 <- data %>%
    group_by(Istasyon) %>%
    summarise(total_AOT40 = sum(AOT40, na.rm = TRUE)) %>%
    ungroup()

  # Eşik değerlerin kontrol edilmesi
  for (threshold in thresholds) {
    total_AOT40 <- total_AOT40 %>%
      mutate(!!paste0("exceeds_", threshold) := total_AOT40 > threshold)
  }

  # Period bilgisi ekleme
  total_AOT40 <- total_AOT40 %>%
    mutate(period = period)

  disconnect_postgres(conn)

  # Sonucu data.frame nesnesine dönüştürme
  result_df <- as.data.frame(total_AOT40)

  return(result_df)
}
