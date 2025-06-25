create_hourly_analysis_views <- function(start_year, end_year, schema_name, parameters) {
  message("Creating hourly analysis views...")

  # Database
  message("Connecting to the database...")
  con <- create_postgres_conn()

  # Get the stations
  message("Getting stations...")
  cities <- tbl(con, "location") %>%
    rename (Istasyon = "Istasyon_modified") %>%
    select(Istasyon, Sehir)

  # Get the intermediate analysis
  message("Getting intermediate analysis...")
  intermediate_analysis <- tbl(con, in_schema(schema_name, "hourly_intermediate_analysis"))

  # Create views
  message("Creating views...")

  # SO2 ----------------------------
  if("SO2" %in% parameters) {
    message("Calculating SO2...")

    SO2_5 <- intermediate_analysis %>%
      select(Istasyon, Yıl, SO2_Saatlik_Veri_Mevcudiyeti, SO2_350_Üstü_Veri_Sayısı) %>%
      rename(SO2_Veri_Mevcudiyeti = SO2_Saatlik_Veri_Mevcudiyeti) %>%
      rename(SO2_Ortalaması = SO2_350_Üstü_Veri_Sayısı) %>%
      list_station_averages("SO2", data_threshold = 0, threshold_direction = "Üstü", threshold = 90) %>%
      sql_render()

    SO2_6 <- intermediate_analysis %>%
      select(Istasyon, Yıl, SO2_350_Üstü_Veri_Sayısı) %>%
      rename(SO2_Ortalaması = SO2_350_Üstü_Veri_Sayısı) %>%
      list_station_averages("SO2", data_threshold = 24, threshold_direction = "Üstü", threshold = 0) %>%
      sql_render()

    SO2_12 <- intermediate_analysis %>%
      select(Istasyon, Yıl, SO2_3_Ardışık_500_Üstü_Veri_Sayısı) %>%
      rename(SO2_Ortalaması = SO2_3_Ardışık_500_Üstü_Veri_Sayısı) %>%
      list_station_averages("SO2", data_threshold = 0, threshold_direction = "Üstü", threshold = 0) %>%
      sql_render()

    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_5", " AS ", SO2_5))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_6", " AS ", SO2_6))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_12", " AS ", SO2_12))
  } else {
    message("SO2 not in parameters, skipping...")
  }

  # NO2 ----------------------------
  if("NO2" %in% parameters) {
    message("Calculating NO2...")

    NO2_4 <- intermediate_analysis %>%
      select(Istasyon, Yıl, NO2_200_Üstü_Veri_Sayısı) %>%
      rename(NO2_Ortalaması = NO2_200_Üstü_Veri_Sayısı) %>%
      list_station_averages("NO2", data_threshold = 18, threshold_direction = "Üstü", threshold = 0) %>%
      sql_render()

    NO2_10 <- intermediate_analysis %>%
      select(Istasyon, Yıl, NO2_3_Ardışık_400_Üstü_Veri_Sayısı) %>%
      rename(NO2_Ortalaması = NO2_3_Ardışık_400_Üstü_Veri_Sayısı) %>%
      list_station_averages("NO2", data_threshold = 0, threshold_direction = "Üstü", threshold = 0) %>%
      sql_render()

    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NO2_4", " AS ", NO2_4))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NO2_10", " AS ", NO2_10))

  } else {
    message("NO2 not in parameters, skipping...")
  }

  # O3 ----------------------------
  if("O3" %in% parameters) {
    message("Calculating O3...")

    O3_9_1 <- intermediate_analysis %>%
      select(Istasyon, Yıl, O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti) %>%
      rename(O3_Veri_Mevcudiyeti = O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti) %>%
      list_stations("O3", 90) %>%
      sql_render()

    O3_9_2 <- intermediate_analysis %>%
      select(Istasyon, Yıl, O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti) %>%
      rename(O3_Veri_Mevcudiyeti = O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti) %>%
      list_stations("O3", 90) %>%
      sql_render()

    O3_10_1 <- intermediate_analysis %>%
      select(Istasyon, Yıl, O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti) %>%
      rename(O3_Veri_Mevcudiyeti = O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti) %>%
      count_stations("O3", 90) %>%
      sql_render()

    O3_10_2 <- intermediate_analysis %>%
      select(Istasyon, Yıl, O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti) %>%
      rename(O3_Veri_Mevcudiyeti = O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti) %>%
      count_stations("O3", 90) %>%
      sql_render()

    O3_11 <- intermediate_analysis %>%
      select(Istasyon, Yıl, O3_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayısı) %>%
      rename(O3_Ortalaması = O3_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayısı) %>%
      list_station_averages("O3", data_threshold = 0, threshold_direction = "Üstü", threshold = 0) %>%
      sql_render()

    O3_12 <- intermediate_analysis %>%
      select(Istasyon, Yıl, O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti, O3_Mayıs_Temmuz_AOT40) %>%
      rename(O3_Veri_Mevcudiyeti = O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti) %>%
      rename(O3_Ortalaması = O3_Mayıs_Temmuz_AOT40) %>%
      list_station_averages("O3", data_threshold = 0, threshold_direction = "Üstü", threshold = 90) %>%
      sql_render()

    O3_13 <- intermediate_analysis %>%
      select(Istasyon, Yıl, O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti, O3_Nisan_Eylül_AOT40) %>%
      rename(O3_Veri_Mevcudiyeti = O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti) %>%
      rename(O3_Ortalaması = O3_Nisan_Eylül_AOT40) %>%
      list_station_averages("O3", data_threshold = 0, threshold_direction = "Üstü", threshold = 90) %>%
      sql_render()

    O3_14 <- intermediate_analysis %>%
      select(Istasyon, Yıl, O3_Nisan_Eylül_1_Saat_Ortalama_Günlük_180_Üstü_Veri_Sayı) %>%
      rename(O3_Ortalaması = O3_Nisan_Eylül_1_Saat_Ortalama_Günlük_180_Üstü_Veri_Sayı) %>%
      list_station_averages("O3", data_threshold = 0, threshold_direction = "Üstü", threshold = 0) %>%
      sql_render()

    O3_15 <- intermediate_analysis %>%
      select(Istasyon, Yıl, O3_Nisan_Eylül_1_Saat_Ortalama_Günlük_240_Üstü_Veri_Sayı) %>%
      rename(O3_Ortalaması = O3_Nisan_Eylül_1_Saat_Ortalama_Günlük_240_Üstü_Veri_Sayı) %>%
      list_station_averages("O3", data_threshold = 0, threshold_direction = "Üstü", threshold = 0) %>%
      sql_render()

    O3_16 <- intermediate_analysis %>%
      select(Istasyon, Yıl, O3_Nisan_Eylül_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayı) %>%
      rename(O3_Ortalaması = O3_Nisan_Eylül_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayı) %>%
      list_station_averages("O3", data_threshold = 0, threshold_direction = "Üstü", threshold = 0) %>%
      sql_render()

    O3_17 <- intermediate_analysis %>%
      select(Istasyon, Yıl, O3_Nisan_Eylül_1_Saat_Ortalama_Aylık_120_Üstü_Veri_Sayı) %>%
      rename(O3_Ortalaması = O3_Nisan_Eylül_1_Saat_Ortalama_Aylık_120_Üstü_Veri_Sayı) %>%
      list_station_averages("O3", data_threshold = 0, threshold_direction = "Üstü", threshold = 0) %>%
      sql_render()

    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_9_1", " AS ", O3_9_1))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_9_2", " AS ", O3_9_2))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_10_1", " AS ", O3_10_1))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_10_2", " AS ", O3_10_2))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_11", " AS ", O3_11))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_12", " AS ", O3_12))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_13", " AS ", O3_13))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_14", " AS ", O3_14))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_15", " AS ", O3_15))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_16", " AS ", O3_16))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_17", " AS ", O3_17))
  } else {
    message("O3 not in parameters, skipping...")
  }

  # CO ----------------------------
  if("CO" %in% parameters) {
    message("Calculating CO...")

    CO_4 <- intermediate_analysis %>%
      select(Istasyon, Yıl, CO_8_Saat_Ortalama_Günlük_10_Üstü_Veri_Sayısı) %>%
      rename(CO_Ortalaması = CO_8_Saat_Ortalama_Günlük_10_Üstü_Veri_Sayısı) %>%
      list_station_averages("CO", data_threshold = 0, threshold_direction = "Üstü", threshold = 0) %>%
      sql_render()

    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".CO_4", " AS ", CO_4))
  } else {
    message("CO not in parameters, skipping...")
  }
  
  # Disconnect
  dbDisconnect(con)

  message("Analysis views for daily data are created.")
}