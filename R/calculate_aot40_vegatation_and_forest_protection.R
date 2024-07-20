#' Calculate the total AOT40 values for vegetation protection within a specified date range
#'
#' This function calculates the total AOT40 values for vegetation protection based on the hourly ozone data.
#' It evaluates whether the AOT40 value exceeds 6000 (µg/m³)*hours and 18000 (µg/m³)*hours within a specified date range.
#'
#' @param parameter_name The name of the parameter (e.g., "O3") to use for the calculations.
#' @param start_month The starting month (e.g., "05" for May) for the calculation.
#' @param end_month The ending month (e.g., "07" for July) for the calculation.
#' @return A data frame with columns: Istasyon, total_AOT40, exceeds_6000, exceeds_18000.
#' @export

calculate_AOT40_vegetation_and_forest_protection <- function(parameter_name, start_month, end_month) {


  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  # Verilen parametre ve ay aralığı için verileri saatlik detay tablosundan almak için sorgulama işlemi
  query <- paste0("SELECT Istasyon, Tarih, ", parameter_name, " FROM hourly_detail WHERE strftime('%m', Tarih) BETWEEN '", start_month, "' AND '", end_month, "'")

  data <- dbGetQuery(mydb, query)

  # Tarih ve saat bilgilerini işleme
  data <- data %>%
    mutate(TarihSaat = as.POSIXct(Tarih, format="%Y-%m-%d %H:%M:%S"),
           hour = as.numeric(format(TarihSaat, "%H"))) %>%
    arrange(Istasyon, TarihSaat)

  # AOT40 değerlerinin hesaplanması (08:00 - 20:00 saatleri arası)
  data <- data %>%
    filter(hour >= 8 & hour <= 20 & !is.na(get(parameter_name)) & get(parameter_name) > 80) %>%
    mutate(AOT40 = get(parameter_name) - 80)

  # İstasyonlar bazında toplam AOT40 değerinin hesaplanması
  total_AOT40 <- data %>%
    group_by(Istasyon) %>%
    summarise(total_AOT40 = sum(AOT40, na.rm = TRUE)) %>%
    ungroup()

  # Eşik değerlerin kontrol edilmesi
  total_AOT40 <- total_AOT40 %>%
    mutate(
      exceeds_6000 = total_AOT40 > 6000,
      exceeds_18000 = total_AOT40 > 18000
    )

  # Sonuçları data.frame nesnesine dönüştürme
  result_df <- data.frame(
    Istasyon = total_AOT40$Istasyon,
    total_AOT40 = total_AOT40$total_AOT40,
    exceeds_6000 = total_AOT40$exceeds_6000,
    exceeds_18000 = total_AOT40$exceeds_18000
  )


  dbDisconnect(mydb)

  return(result_df)
}

