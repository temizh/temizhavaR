#' List Stations for Specified Parameter and Season from daily_detail
#'
#' @param parameter_name The name of the parameter.
#' @param threshold The threshold percentage for data availability (default is 90).
#' @param season The season for which data availability is calculated (default is NULL). Use "summer" or "winter".
#' @return A data frame with stations and their data availability percentage.
#' @export

daily_list_stations_with_parameter_threshold <- function(parameter_name, threshold = 90, season = NULL, verbose = FALSE) {

  parameter_name <- gsub('\\"', "", parameter_name)
  data <- all_daily_detail_load_from_database(parameter_name, verbose = FALSE)

  if (verbose) {
    print("Orijinal veri boyutu:")
    print(dim(data))
  }
  if (!is.null(season)) {
    if (season == "summer") {
      summer_months <- c(4, 5, 6, 7, 8, 9)
      data <- data %>% filter(month(Tarih) %in% summer_months)
    } else if (season == "winter") {
      winter_months <- c(1, 2, 3, 10, 11, 12)
      data <- data %>% filter(month(Tarih) %in% winter_months)
    }
  }

  if (verbose) {
    print("Sezon filtrelemesi sonrası veri boyutu:")
    print(dim(data))
  }

  days_in_season <- length(unique(data$Tarih))
  if (verbose) {
    print("Sezondaki gün sayısı:")
    print(days_in_season)
  }

  query_result <- data %>%
    group_by(istasyon_modified) %>%
    summarise(
      total_days = n(),
      non_na_days = sum(!is.na(.data[[parameter_name]])),
      veri_mevcudiyet_yuzdesi = round(non_na_days / days_in_season * 100, 2)
    ) %>%
    # %89.5 ile %90 arasındaki veri mevcudiyet yüzdelerini %90'a yuvarlama
    mutate(
      threshold_status = ifelse(veri_mevcudiyet_yuzdesi >= threshold, "Üstünde", "Altında")
    ) %>%
    arrange(desc(veri_mevcudiyet_yuzdesi), Istasyon_modified)

  if (verbose) {
    print("İstasyon sayımı sonuçları:")
    print(query_result)

    print("Eşik değerin üstündeki istasyon sayısı:")
    print(sum(query_result$threshold_status == "Üstünde"))

    print("Eşik değerin altındaki istasyon sayısı:")
    print(sum(query_result$threshold_status == "Altında"))

    print("Toplam istasyon sayısı:")
    print(nrow(query_result))
  }

  return(as.data.frame(query_result))
}
