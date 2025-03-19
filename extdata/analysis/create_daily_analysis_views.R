create_daily_analysis_views <- function(start_year, end_year, schema_name) {
  message("Creating daily analysis views...")

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
  intermediate_analysis <- tbl(con, in_schema(schema_name, "daily_intermediate_analysis"))

  # Create views
  message("Creating views...")

  # PM10 ----------------------------
  PM10_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Veri_Mevcudiyeti) %>%
    list_stations("PM10", 0) %>%
    sql_render()

  PM10_2 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Veri_Mevcudiyeti) %>%
    count_stations("PM10", 0) %>%
    sql_render()

  PM10_3_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Veri_Mevcudiyeti) %>%
    list_stations("PM10", 90) %>%
    sql_render()

  PM10_4_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Veri_Mevcudiyeti) %>%
    count_stations("PM10", 90) %>%
    sql_render()

  PM10_3_2 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Veri_Mevcudiyeti) %>%
    list_stations("PM10", 75) %>%
    sql_render()

  PM10_4_2 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Veri_Mevcudiyeti) %>%
    count_stations("PM10", 75) %>%
    sql_render()

  PM10_5 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Ortalaması) %>%
    list_station_averages("PM10", data_threshold = 0, threshold_direction = "Üstü") %>%
    sql_render()

  PM10_6 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Ortalaması) %>%
    list_station_averages("PM10", data_threshold = 40, threshold_direction = "Üstü") %>%
    sql_render()

  PM10_7 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Ortalaması) %>%
    list_station_averages("PM10", data_threshold = 40, threshold_direction = "Altı") %>%
    sql_render()

  PM10_8 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Ortalaması) %>%
    list_station_averages("PM10", data_threshold = 15, threshold_direction = "Üstü") %>%
    sql_render()

  PM10_9 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Ortalaması) %>%
    list_station_averages("PM10", data_threshold = 15, threshold_direction = "Altı") %>%
    sql_render()

  PM10_10 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Veri_Mevcudiyeti, PM10_50_Üstü_Veri_Sayısı) %>%
    list_station_exceedance("PM10", threshold = 90, data_threshold = 50, threshold_direction = "Üstü") %>%
    sql_render()

  # PM10_11 ???

  PM10_12 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Veri_Mevcudiyeti, PM10_45_Altı_Veri_Sayısı) %>%
    list_station_exceedance("PM10", threshold = 90, data_threshold = 45, threshold_direction = "Altı") %>%
    sql_render()

  # PM10_13 ???

  PM10_14 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM10_Veri_Mevcudiyeti, PM10_Ortalaması) %>%
    calculate_city_average("PM10", cities = cities, threshold = 90) %>%
    sql_render()

  # PM25 ----------------------------
  PM25_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Veri_Mevcudiyeti) %>%
    list_stations("PM25", 0) %>%
    sql_render()

  PM25_2 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Veri_Mevcudiyeti) %>%
    count_stations("PM25", 0) %>%
    sql_render()

  PM25_3 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Veri_Mevcudiyeti) %>%
    list_stations("PM25", 90) %>%
    sql_render()

  PM25_4 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Veri_Mevcudiyeti) %>%
    count_stations("PM25", 90) %>%
    sql_render()

  PM25_5 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Veri_Mevcudiyeti) %>%
    list_stations("PM25", 75) %>%
    sql_render()

  PM25_6 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Veri_Mevcudiyeti) %>%
    count_stations("PM25", 75) %>%
    sql_render()

  PM25_7 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Ortalaması) %>%
    list_station_averages("PM25", data_threshold = 0, threshold_direction = "Üstü") %>%
    sql_render()

  PM25_8 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Ortalaması) %>%
    list_station_averages("PM25", data_threshold = 5, threshold_direction = "Üstü") %>%
    sql_render()

  PM25_9 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Ortalaması) %>%
    list_station_averages("PM25", data_threshold = 5, threshold_direction = "Altı") %>%
    sql_render()

  PM25_10 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Veri_Mevcudiyeti, PM25_15_Üstü_Veri_Sayısı) %>%
    list_station_exceedance("PM25", threshold = 90, data_threshold = 15, threshold_direction = "Üstü") %>%
    sql_render()

  # PM25_11 ???

  PM25_12 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Veri_Mevcudiyeti, PM25_15_Altı_Veri_Sayısı) %>%
    list_station_exceedance("PM25", threshold = 90, data_threshold = 15, threshold_direction = "Altı") %>%
    sql_render()

  PM25_13 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Tamamlanmış) %>%
    rename("PM25_Ortalaması" = "PM25_Tamamlanmış") %>%
    list_station_averages("PM25", data_threshold = 0, threshold_direction = "Üstü") %>%
    sql_render()

  PM25_14 <- intermediate_analysis %>%
    select(Istasyon, Yıl, PM25_Veri_Mevcudiyeti, PM25_Tamamlanmış) %>%
    rename("PM25_Ortalaması" = "PM25_Tamamlanmış") %>%
    calculate_city_average("PM25", cities = cities, threshold = 0) %>%
    sql_render()

  # SO2 ----------------------------
  SO2_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, SO2_Veri_Mevcudiyeti) %>%
    list_stations("SO2", 0) %>%
    sql_render()

  SO2_2_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, SO2_Veri_Mevcudiyeti) %>%
    list_stations("SO2", 90) %>%
    sql_render()

  SO2_3_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, SO2_Veri_Mevcudiyeti) %>%
    count_stations("SO2", 90) %>%
    sql_render()

  SO2_2_2 <- intermediate_analysis %>%
    select(Istasyon, Yıl, SO2_Veri_Mevcudiyeti) %>%
    list_stations("SO2", 75) %>%
    sql_render()

  SO2_3_2 <- intermediate_analysis %>%
    select(Istasyon, Yıl, SO2_Veri_Mevcudiyeti) %>%
    count_stations("SO2", 75) %>%
    sql_render()

  SO2_4 <- intermediate_analysis %>%
    select(Istasyon, Yıl, SO2_Ortalaması) %>%
    list_station_averages("SO2", data_threshold = 0, threshold_direction = "Üstü") %>%
    sql_render()

  SO2_7 <- intermediate_analysis %>%
    select(Istasyon, Yıl, SO2_Veri_Mevcudiyeti, SO2_40_Üstü_Veri_Sayısı) %>%
    list_station_exceedance("SO2", threshold = 90, data_threshold = 40, threshold_direction = "Üstü") %>%
    sql_render()

  SO2_8 <- intermediate_analysis %>%
    select(Istasyon, Yıl, SO2_Veri_Mevcudiyeti, SO2_125_Üstü_Veri_Sayısı) %>%
    list_station_exceedance("SO2", threshold = 90, data_threshold = 125, threshold_direction = "Üstü") %>%
    sql_render()

  SO2_9 <- intermediate_analysis %>%
    select(Istasyon, Yıl, SO2_Veri_Mevcudiyeti, SO2_125_Üstü_Veri_Sayısı) %>%
    list_station_exceedance("SO2", threshold = 90, data_threshold = 125, threshold_direction = "Üstü", exceedance_threshold = 3) %>%
    sql_render()

  SO2_11 <- intermediate_analysis %>%
    select(Istasyon, Yıl, SO2_Veri_Mevcudiyeti, SO2_Ortalaması) %>%
    list_station_averages("SO2", data_threshold = 20, threshold_direction = "Üstü") %>%
    sql_render()

  # NO2 ----------------------------
  NO2_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NO2_Veri_Mevcudiyeti) %>%
    list_stations("NO2", 0) %>%
    sql_render()

  NO2_2_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NO2_Veri_Mevcudiyeti) %>%
    list_stations("NO2", 90) %>%
    sql_render()

  NO2_3_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NO2_Veri_Mevcudiyeti) %>%
    count_stations("NO2", 90) %>%
    sql_render()

  NO2_2_2 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NO2_Veri_Mevcudiyeti) %>%
    list_stations("NO2", 75) %>%
    sql_render()

  NO2_3_2 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NO2_Veri_Mevcudiyeti) %>%
    count_stations("NO2", 75) %>%
    sql_render()

  NO2_5 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NO2_25_Üstü_Veri_Sayısı) %>%
    list_station_exceedance("NO2", threshold = 90, data_threshold = 25, threshold_direction = "Üstü", exceedance_threshold = 3) %>%
    sql_render()

  NO2_6 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NO2_Ortalaması) %>%
    list_station_averages("NO2", data_threshold = 0, threshold_direction = "Üstü") %>%
    sql_render()

  NO2_7 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NO2_Ortalaması) %>%
    list_station_averages("NO2", data_threshold = 40, threshold_direction = "Üstü") %>%
    sql_render()

  NO2_8 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NO2_Ortalaması) %>%
    list_station_averages("NO2", data_threshold = 10, threshold_direction = "Üstü") %>%
    sql_render()

  NO2_9 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NO2_Veri_Mevcudiyeti, NO2_Ortalaması) %>%
    calculate_city_average("NO2", cities = cities, threshold = 90) %>%
    sql_render()

  # NOX ----------------------------
  NOX_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NOX_Veri_Mevcudiyeti) %>%
    list_stations("NOX", 0) %>%
    sql_render()

  NOX_2_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NOX_Veri_Mevcudiyeti) %>%
    list_stations("NOX", 90) %>%
    sql_render()

  NOX_3_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NOX_Veri_Mevcudiyeti) %>%
    count_stations("NOX", 90) %>%
    sql_render()

  NOX_2_2 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NOX_Veri_Mevcudiyeti) %>%
    list_stations("NOX", 75) %>%
    sql_render()

  NOX_3_2 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NOX_Veri_Mevcudiyeti) %>%
    count_stations("NOX", 75) %>%
    sql_render()

  NOX_4 <- intermediate_analysis %>%
    select(Istasyon, Yıl, NOX_Ortalaması) %>%
    list_station_averages("NOX", data_threshold = 30, threshold_direction = "Üstü") %>%
    sql_render()

  # O3 ----------------------------
  O3_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, O3_Veri_Mevcudiyeti) %>%
    list_stations("O3", 0) %>%
    sql_render()

  O3_2 <- intermediate_analysis %>%
    select(Istasyon, Yıl, O3_Veri_Mevcudiyeti) %>%
    count_stations("O3", 0) %>%
    sql_render()

  O3_3 <- intermediate_analysis %>%
    select(Istasyon, Yıl, O3_Veri_Mevcudiyeti) %>%
    list_stations("O3", 90) %>%
    sql_render()

  O3_4 <- intermediate_analysis %>%
    select(Istasyon, Yıl, O3_Veri_Mevcudiyeti) %>%
    count_stations("O3", 90) %>%
    sql_render()

  O3_5 <- intermediate_analysis %>%
    select(Istasyon, Yıl, O3_Yaz_Veri_Mevcudiyeti) %>%
    rename("O3_Veri_Mevcudiyeti" = "O3_Yaz_Veri_Mevcudiyeti") %>%
    list_stations("O3", 90) %>%
    sql_render()

  O3_6 <- intermediate_analysis %>%
    select(Istasyon, Yıl, O3_Yaz_Veri_Mevcudiyeti) %>%
    rename("O3_Veri_Mevcudiyeti" = "O3_Yaz_Veri_Mevcudiyeti") %>%
    count_stations("O3", 90) %>%
    sql_render()

  O3_7 <- intermediate_analysis %>%
    select(Istasyon, Yıl, O3_Kış_Veri_Mevcudiyeti) %>%
    rename("O3_Veri_Mevcudiyeti" = "O3_Kış_Veri_Mevcudiyeti") %>%
    list_stations("O3", 75) %>%
    sql_render()

  O3_8 <- intermediate_analysis %>%
    select(Istasyon, Yıl, O3_Kış_Veri_Mevcudiyeti) %>%
    rename("O3_Veri_Mevcudiyeti" = "O3_Kış_Veri_Mevcudiyeti") %>%
    count_stations("O3", 75) %>%
    sql_render()

  # CO ----------------------------
  CO_1 <- intermediate_analysis %>%
    select(Istasyon, Yıl, CO_Veri_Mevcudiyeti) %>%
    list_stations("CO", 0) %>%
    sql_render()

  CO_2 <- intermediate_analysis %>%
    select(Istasyon, Yıl, CO_Veri_Mevcudiyeti) %>%
    list_stations("CO", 90) %>%
    sql_render()

  CO_3 <- intermediate_analysis %>%
    select(Istasyon, Yıl, CO_Veri_Mevcudiyeti) %>%
    count_stations("CO", 90) %>%
    sql_render()

  # Save the views
  message("Saving views...")

  # PM10 ----------------------------
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_1", " AS ", PM10_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_2", " AS ", PM10_2))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_3_1", " AS ", PM10_3_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_4_1", " AS ", PM10_4_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_3_2", " AS ", PM10_3_2))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_4_2", " AS ", PM10_4_2))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_5", " AS ", PM10_5))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_6", " AS ", PM10_6))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_7", " AS ", PM10_7))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_8", " AS ", PM10_8))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_9", " AS ", PM10_9))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_10", " AS ", PM10_10))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_12", " AS ", PM10_12))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM10_14", " AS ", PM10_14))

  # PM25 ----------------------------
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_1", " AS ", PM25_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_2", " AS ", PM25_2))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_3", " AS ", PM25_3))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_4", " AS ", PM25_4))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_5", " AS ", PM25_5))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_6", " AS ", PM25_6))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_7", " AS ", PM25_7))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_8", " AS ", PM25_8))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_9", " AS ", PM25_9))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_10", " AS ", PM25_10))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_12", " AS ", PM25_12))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_13", " AS ", PM25_13))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".PM25_14", " AS ", PM25_14))

  # SO2 ----------------------------
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_1", " AS ", SO2_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_2_1", " AS ", SO2_2_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_3_1", " AS ", SO2_3_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_2_2", " AS ", SO2_2_2))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_3_2", " AS ", SO2_3_2))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_4", " AS ", SO2_4))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_7", " AS ", SO2_7))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_8", " AS ", SO2_8))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_9", " AS ", SO2_9))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".SO2_11", " AS ", SO2_11))

  # NO2 ----------------------------
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NO2_1", " AS ", NO2_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NO2_2_1", " AS ", NO2_2_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NO2_3_1", " AS ", NO2_3_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NO2_2_2", " AS ", NO2_2_2))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NO2_3_2", " AS ", NO2_3_2))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NO2_5", " AS ", NO2_5))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NO2_6", " AS ", NO2_6))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NO2_7", " AS ", NO2_7))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NO2_8", " AS ", NO2_8))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NO2_9", " AS ", NO2_9))

  # NOX ----------------------------
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NOX_1", " AS ", NOX_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NOX_2_1", " AS ", NOX_2_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NOX_3_1", " AS ", NOX_3_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NOX_2_2", " AS ", NOX_2_2))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NOX_3_2", " AS ", NOX_3_2))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".NOX_4", " AS ", NOX_4))

  # O3 ----------------------------
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_1", " AS ", O3_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_2", " AS ", O3_2))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_3", " AS ", O3_3))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_4", " AS ", O3_4))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_5", " AS ", O3_5))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_6", " AS ", O3_6))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_7", " AS ", O3_7))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".O3_8", " AS ", O3_8))

  # CO ----------------------------
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".CO_1", " AS ", CO_1))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".CO_2", " AS ", CO_2))
  dbExecute(con, paste0("CREATE VIEW ", schema_name, ".CO_3", " AS ", CO_3))

  # Disconnect
  dbDisconnect(con)

  message("Analysis views for daily data are created.")
}