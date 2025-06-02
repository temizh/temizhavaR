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

  # O3 ----------------------------
  if("O3" %in% parameters) {
    message("Calculating O3...")

    # O3_9_1 <- intermediate_analysis %>%
    #   select(Istasyon, Yıl, O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti) %>%
    #   rename(O3_Veri_Mevcudiyeti = O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti) %>%
    #   list_stations("O3", 90) %>%
    #   sql_render()

    # O3_9_2 <- intermediate_analysis %>%
    #   select(Istasyon, Yıl, O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti) %>%
    #   rename(O3_Veri_Mevcudiyeti = O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti) %>%
    #   list_stations("O3", 90) %>%
    #   sql_render()

    # O3_10_1 <- intermediate_analysis %>%
    #   select(Istasyon, Yıl, O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti) %>%
    #   rename(O3_Veri_Mevcudiyeti = O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti) %>%
    #   count_stations("O3", 90) %>%
    #   sql_render()

    # O3_10_2 <- intermediate_analysis %>%
    #   select(Istasyon, Yıl, O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti) %>%
    #   rename(O3_Veri_Mevcudiyeti = O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti) %>%
    #   count_stations("O3", 90) %>%
    #   sql_render()

    # O3_12 <- intermediate_analysis %>%
    #   select(Istasyon, Yıl, O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti, O3_Mayıs_Temmuz_AOT40) %>%
    #   rename(O3_Veri_Mevcudiyeti = O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti) %>%
    #   rename(O3_Ortalaması = O3_Mayıs_Temmuz_AOT40) %>%
    #   list_station_averages("O3", data_threshold = 0, threshold_direction = "Üstü", threshold = 90) %>%
    #   sql_render()

    # O3_13 <- intermediate_analysis %>%
    #   select(Istasyon, Yıl, O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti, O3_Nisan_Eylül_AOT40) %>%
    #   rename(O3_Veri_Mevcudiyeti = O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti) %>%
    #   rename(O3_Ortalaması = O3_Nisan_Eylül_AOT40) %>%
    #   list_station_averages("O3", data_threshold = 0, threshold_direction = "Üstü", threshold = 90) %>%
    #   sql_render()

    O3_11 <- intermediate_analysis %>%
      select(Istasyon, Yıl, O3_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayısı) %>%
      rename(O3_Ortalaması = O3_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayısı) %>%
      list_station_averages("O3", data_threshold = 0, threshold_direction = "Üstü", threshold = 0) %>%
      sql_render()

    # dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_9_1", " AS ", O3_9_1))
    # dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_9_2", " AS ", O3_9_2))
    # dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_10_1", " AS ", O3_10_1))
    # dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_10_2", " AS ", O3_10_2))
    # dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_12", " AS ", O3_12))
    # dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_13", " AS ", O3_13))
    dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_11", " AS ", O3_11))
    
  } else {
    message("O3 not in parameters, skipping...")
  }
  
  # Disconnect
  dbDisconnect(con)

  message("Analysis views for daily data are created.")
}