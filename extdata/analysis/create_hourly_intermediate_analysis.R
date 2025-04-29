# Description: This script creates intermediate analysis for hourly data.
create_hourly_intermediate_analysis <- function(start_year, end_year, schema_name) {
  message("Creating intermediate analysis for hourly data...")

  # Configuration
  data_in_year <- 365 * 24 # for hourly data

  # Database
  message("Connecting to the database...")
  con <- create_postgres_conn()

  data <- tbl(con, "hourly_detail")

  # Prepare data
  data <- data %>%
    rename(Istasyon = "Istasyon_modified") %>%
    mutate(Yıl = year(Tarih)) %>%
    mutate(Ay = month(Tarih)) %>%
    mutate(Gün = day(Tarih)) %>%
    mutate(Saat = hour(Tarih)) %>%
    mutate(Dakika = minute(Tarih)) %>%
    mutate(Saat = ifelse(Dakika > 30, Saat + 1, Saat)) %>%
    filter(Yıl >= start_year & Yıl <= end_year)

  # Filter data by station
  data <- data %>%
    filter(Istasyon %in% c("Eskişehir-Vişnepark", "İzmir-Bornova", "İstanbul-Silivri-MTHM", "İstanbul-Ümraniye"))

  # Group data
  grouped_data <- data %>%
    group_by(Istasyon, Yıl)

  # Analyse data
  message("Analysing data...")

  # SO2 ----------------------------
  # SO2_350_Üstü_Veri_Sayısı <- grouped_data %>%
  #   calculate_exceedance("SO2", 350, "above") %>%
  #   rename("SO2_350_Üstü_Veri_Sayısı" = result)

  # SO2_3_Ardışık_500_Üstü_Veri_Sayısı <- grouped_data %>%
  #   calculate_consecutive_exceedance("SO2", 500, 3, "above") %>%
  #   rename("SO2_3_Ardışık_500_Üstü_Veri_Sayısı" = result)

  # NO2 ----------------------------
  # NO2_200_Üstü_Veri_Sayısı <- grouped_data %>%
  #   calculate_exceedance("NO2", 200, "above") %>%
  #   rename("NO2_200_Üstü_Veri_Sayısı" = result)

  # NO2_3_Ardışık_400_Üstü_Veri_Sayısı <- grouped_data %>%
  #   calculate_consecutive_exceedance("NO2", 400, 3, "above") %>%
  #   rename("NO2_3_Ardışık_400_Üstü_Veri_Sayısı" = result)

  # O3 ----------------------------
  O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti <- grouped_data %>%
    calculate_AOT40_percentage("O3", months = c(5, 6, 7), days = 92) %>%
    rename("O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti" = result)

  # O3 ----------------------------
  O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti <- grouped_data %>%
    calculate_AOT40_percentage("O3", months = c(4, 5, 6, 7, 8, 9), days = 183) %>%
    rename("O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti" = result)

  # O3_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayısı <- grouped_data %>%
  #   calculate_rolling_exceedance("O3", 120, 8, "Day") %>%
  #   rename("O3_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayısı" = result)

  O3_Mayıs_Temmuz_AOT40 <- grouped_data %>%
    calculate_AOT40("O3", months = c(5, 6, 7), days = 92) %>%
    rename("O3_Mayıs_Temmuz_AOT40" = result)

  O3_Nisan_Eylül_AOT40 <- grouped_data %>%
    calculate_AOT40("O3", months = c(4, 5, 6, 7, 8, 9), days = 183) %>%
    rename("O3_Nisan_Eylül_AOT40" = result)

  # O3_Nisan_Eylül_1_Saat_Ortalama_Günlük_180_Üstü_Veri_Sayısı <- grouped_data %>%
  #   calculate_rolling_exceedance("O3", 180, 1, "Day", months = c(4, 5, 6, 7, 8, 9)) %>%
  #   rename("O3_Nisan_Eylül_1_Saat_Ortalama_Günlük_180_Üstü_Veri_Sayısı" = result)

  # O3_Nisan_Eylül_1_Saat_Ortalama_Günlük_240_Üstü_Veri_Sayısı <- grouped_data %>%
  #   calculate_rolling_exceedance("O3", 240, 1, "Day", months = c(4, 5, 6, 7, 8, 9)) %>%
  #   rename("O3_Nisan_Eylül_1_Saat_Ortalama_Günlük_240_Üstü_Veri_Sayısı" = result)

  # O3_Nisan_Eylül_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayısı <- grouped_data %>%
  #   calculate_rolling_exceedance("O3", 120, 8, "Day", months = c(4, 5, 6, 7, 8, 9)) %>%
  #   rename("O3_Nisan_Eylül_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayısı" = result)

  # O3_Nisan_Eylül_1_Saat_Ortalama_Aylık_120_Üstü_Veri_Sayısı <- grouped_data %>%
  #   calculate_rolling_exceedance("O3", 120, 1, "Month", months = c(4, 5, 6, 7, 8, 9)) %>%
  #   rename("O3_Nisan_Eylül_1_Saat_Ortalama_Aylık_120_Üstü_Veri_Sayısı" = result)

  # CO ----------------------------

  # CO_8_Saat_Ortalama_Günlük_10_Üstü_Veri_Sayısı <- grouped_data %>%
  #   calculate_rolling_exceedance("CO", 10, 8, "Day") %>%
  #   rename("CO_8_Saat_Ortalama_Günlük_10_Üstü_Veri_Sayısı" = result)

  # Combine results
  message("Combining results...")
  analysed_data <- #SO2_350_Üstü_Veri_Sayısı %>%
    # left_join(SO2_3_Ardışık_500_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%
    # left_join(NO2_200_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%
    # left_join(NO2_3_Ardışık_400_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%
    # left_join(O3_AOT40_Veri_Mevcudiyeti, by = c("Istasyon", "Yıl")) %>%
    O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti %>%
    left_join(O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti, by = c("Istasyon", "Yıl")) %>%
    # left_join(O3_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%
    left_join(O3_Mayıs_Temmuz_AOT40, by = c("Istasyon", "Yıl")) %>%
    left_join(O3_Nisan_Eylül_AOT40, by = c("Istasyon", "Yıl"))
    # left_join(O3_Nisan_Eylül_1_Saat_Ortalama_Günlük_180_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%
    # left_join(O3_Nisan_Eylül_1_Saat_Ortalama_Günlük_240_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%
    # left_join(O3_Nisan_Eylül_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%
    # left_join(O3_Nisan_Eylül_1_Saat_Ortalama_Aylık_120_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl")) %>%
    # left_join(CO_8_Saat_Ortalama_Günlük_10_Üstü_Veri_Sayısı, by = c("Istasyon", "Yıl"))

  # Save data
  message("Saving data...")
  compute(analysed_data, in_schema(schema_name, "hourly_intermediate_analysis"), temporary = FALSE)

  # Close conection
  dbDisconnect(con)
  message("Intermediate analysis for hourly data is created.")
}