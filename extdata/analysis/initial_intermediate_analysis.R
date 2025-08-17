library(temizhavaR)
library(DBI)
library(jsonlite)
library(dplyr)
library(dbplyr)

# Connect to database
conn <- create_postgres_conn()

# If table does not exist, create it
if (!dbExistsTable(conn, "intermediate_analysis")) {
  message("Creating table intermediate_analysis...")
  dbExecute(conn, "CREATE TABLE intermediate_analysis
    (
      name TEXT PRIMARY KEY,
      analysis TEXT,
      data_type TEXT,
      pollutant TEXT,
      parameters JSONB,
      is_default BOOLEAN
    )")
}

# Delete all default analysis
message("Deleting all default intermediate analysis...")
dbExecute(conn, "DELETE FROM intermediate_analysis WHERE is_default = TRUE")

# Dataframe to store all analysis
analysis <- data.frame(
  name = c(),
  analysis = c(),
  data_type = c(),
  pollutant = c(),
  parameters = c(),
  is_default = c()
)

# PM10 intermediate analysis
message("Adding PM10 intermediate analysis...")
analysis <- analysis %>%
  rbind(data.frame(
    name = "PM10_Veri_Mevcudiyeti",
    analysis = "percentage",
    data_type = "daily",
    pollutant = "PM10",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_Ortalaması",
    analysis = "average",
    data_type = "daily",
    pollutant = "PM10",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_45_Üstü_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "daily",
    pollutant = "PM10",
    parameters = '{"threshold": 45, "direction": "above"}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_45_Altı_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "daily",
    pollutant = "PM10",
    parameters = '{"threshold": 45, "direction": "below"}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_50_Üstü_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "daily",
    pollutant = "PM10",
    parameters = '{"threshold": 50, "direction": "above"}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_50_Altı_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "daily",
    pollutant = "PM10",
    parameters = '{"threshold": 50, "direction": "below"}',
    is_default = TRUE
  ))

# PM25 intermediate analysis
message("Adding PM25 intermediate analysis...")
analysis <- analysis %>%
  rbind(data.frame(
    name = "PM25_Veri_Mevcudiyeti",
    analysis = "percentage",
    data_type = "daily",
    pollutant = "PM25",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_Ortalaması",
    analysis = "average",
    data_type = "daily",
    pollutant = "PM25",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_15_Üstü_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "daily",
    pollutant = "PM25",
    parameters = '{"threshold": 15, "direction": "above"}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_15_Altı_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "daily",
    pollutant = "PM25",
    parameters = '{"threshold": 15, "direction": "below"}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_Tamamlanmış",
    analysis = "estimate",
    data_type = "daily",
    pollutant = "PM25",
    parameters = '{"estimate_from": "PM10", "threshold": 75}',
    is_default = TRUE
  ))

# SO2 intermediate analysis
message("Adding SO2 intermediate analysis...")
analysis <- analysis %>%
  rbind(data.frame(
    name = "SO2_Veri_Mevcudiyeti",
    analysis = "percentage",
    data_type = "daily",
    pollutant = "SO2",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "SO2_Ortalaması",
    analysis = "average",
    data_type = "daily",
    pollutant = "SO2",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "SO2_40_Üstü_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "daily",
    pollutant = "SO2",
    parameters = '{"threshold": 40, "direction": "above"}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "SO2_125_Üstü_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "daily",
    pollutant = "SO2",
    parameters = '{"threshold": 125, "direction": "above"}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "SO2_Saatlik_Veri_Mevcudiyeti",
    analysis = "percentage",
    data_type = "hourly",
    pollutant = "SO2",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "SO2_350_Üstü_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "hourly",
    pollutant = "SO2",
    parameters = '{"threshold": 350, "direction": "above"}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "SO2_3_Ardışık_500_Üstü_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "hourly",
    pollutant = "SO2",
    parameters = '{"threshold": 500, "direction": "above", "consecutive": 3}',
    is_default = TRUE
  ))


# NO2 intermediate analysis
message("Adding NO2 intermediate analysis...")
analysis <- analysis %>%
  rbind(data.frame(
    name = "NO2_Veri_Mevcudiyeti",
    analysis = "percentage",
    data_type = "daily",
    pollutant = "NO2",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "NO2_Ortalaması",
    analysis = "average",
    data_type = "daily",
    pollutant = "NO2",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "NO2_25_Üstü_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "daily",
    pollutant = "NO2",
    parameters = '{"threshold": 25, "direction": "above"}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "NO2_200_Üstü_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "hourly",
    pollutant = "NO2",
    parameters = '{"threshold": 200, "direction": "above"}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "NO2_3_Ardışık_400_Üstü_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "hourly",
    pollutant = "NO2",
    parameters = '{"threshold": 400, "direction": "above", "consecutive": 3}',
    is_default = TRUE
  ))


# NOX intermediate analysis
message("Adding NOX intermediate analysis...")
analysis <- analysis %>%
  rbind(data.frame(
    name = "NOX_Veri_Mevcudiyeti",
    analysis = "percentage",
    data_type = "daily",
    pollutant = "NOX",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "NOX_Ortalaması",
    analysis = "average",
    data_type = "daily",
    pollutant = "NOX",
    parameters = "{}",
    is_default = TRUE
  ))

# O3 intermediate analysis
message("Adding O3 intermediate analysis...")
analysis <- analysis %>%
  rbind(data.frame(
    name = "O3_Veri_Mevcudiyeti",
    analysis = "percentage",
    data_type = "daily",
    pollutant = "O3",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "O3_Ortalaması",
    analysis = "average",
    data_type = "daily",
    pollutant = "O3",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "O3_Kış_Veri_Mevcudiyeti",
    analysis = "percentage",
    data_type = "daily",
    pollutant = "O3",
    parameters = '{"months": [1, 2, 3, 10, 11, 12]}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "O3_Yaz_Veri_Mevcudiyeti",
    analysis = "percentage",
    data_type = "daily",
    pollutant = "O3",
    parameters = '{"months": [4, 5, 6, 7, 8, 9]}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "O3_AOT40_Mayıs_Temmuz_Veri_Mevcudiyeti",
    analysis = "aot40_percentage",
    data_type = "hourly",
    pollutant = "O3",
    parameters = '{"months": [5, 6, 7], "days": 92}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "O3_AOT40_Nisan_Eylül_Veri_Mevcudiyeti",
    analysis = "aot40_percentage",
    data_type = "hourly",
    pollutant = "O3",
    parameters = '{"months": [4, 5, 6, 7, 8, 9], "days": 183}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "O3_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "hourly",
    pollutant = "O3",
    parameters = '{"threshold": 120, "direction": "above", "rolling": 8}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "O3_Mayıs_Temmuz_AOT40",
    analysis = "aot40",
    data_type = "hourly",
    pollutant = "O3",
    parameters = '{"months": [5, 6, 7]}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "O3_Nisan_Eylül_AOT40",
    analysis = "aot40",
    data_type = "hourly",
    pollutant = "O3",
    parameters = '{"months": [4, 5, 6, 7, 8, 9]}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "O3_Nisan_Eylül_1_Saat_Ortalama_Günlük_180_Üstü_Veri_Sayı",
    analysis = "exceedance",
    data_type = "hourly",
    pollutant = "O3",
    parameters = '{"threshold": 180, "direction": "above", "rolling": 1, "months": [4, 5, 6, 7, 8, 9]}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "O3_Nisan_Eylül_1_Saat_Ortalama_Günlük_240_Üstü_Veri_Sayı",
    analysis = "exceedance",
    data_type = "hourly",
    pollutant = "O3",
    parameters = '{"threshold": 240, "direction": "above", "rolling": 1, "months": [4, 5, 6, 7, 8, 9]}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "O3_Nisan_Eylül_8_Saat_Ortalama_Günlük_120_Üstü_Veri_Sayı",
    analysis = "exceedance",
    data_type = "hourly",
    pollutant = "O3",
    parameters = '{"threshold": 120, "direction": "above", "rolling": 8, "months": [4, 5, 6, 7, 8, 9]}',
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "O3_Nisan_Eylül_1_Saat_Ortalama_Aylık_120_Üstü_Veri_Sayı",
    analysis = "exceedance",
    data_type = "hourly",
    pollutant = "O3",
    parameters = '{"threshold": 120, "direction": "above", "rolling": 1, "months": [4, 5, 6, 7, 8, 9]}',
    is_default = TRUE
  ))


# CO intermediate analysis
message("Adding CO intermediate analysis...")
analysis <- analysis %>%
  rbind(data.frame(
    name = "CO_Veri_Mevcudiyeti",
    analysis = "percentage",
    data_type = "daily",
    pollutant = "CO",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "CO_Ortalaması",
    analysis = "average",
    data_type = "daily",
    pollutant = "CO",
    parameters = "{}",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "CO_8_Saat_Ortalama_Günlük_10_Üstü_Veri_Sayısı",
    analysis = "exceedance",
    data_type = "hourly",
    pollutant = "CO",
    parameters = '{"threshold": 10, "direction": "above", "rolling": 8}',
    is_default = TRUE
  ))

# Save analysis to database
message("Saving intermediate analysis to database...")
dbAppendTable(conn, "intermediate_analysis", analysis)

# Disconnect from the database
dbDisconnect(conn)