library(temizhavaR)
library(DBI)
library(jsonlite)
library(dplyr)
library(dbplyr)

# Connect to database
conn <- create_postgres_conn()

# If table does not exist, create it
if (!dbExistsTable(conn, "final_analysis")) {
  message("Creating table final_analysis...")
  dbExecute(conn, "CREATE TABLE final_analysis
    (
      name TEXT PRIMARY KEY,
      analysis TEXT,
      data TEXT,
      filters JSONB,
      group_by TEXT,
      description TEXT,
      is_default BOOLEAN
    )")
}

# Delete all default analysis
message("Deleting all default final analysis...")
dbExecute(conn, "DELETE FROM final_analysis WHERE is_default = TRUE")

# Dataframe to store all analysis
analysis <- data.frame(
  name = character(),
  analysis = character(),
  data = character(),
  filters = character(),
  group_by = character(),
  description = character(),
  is_default = logical()
)

# PM10 final analysis
message("Adding PM10 final analysis...")
analysis <- analysis %>%
  rbind(data.frame(
    name = "PM10_1",
    analysis = "list",
    data = "PM10_Veri_Mevcudiyeti",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 0
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM10 veri alınan istasyon listesi",
    is_default = TRUE
  ))  %>%
  rbind(data.frame(
    name = "PM10_2",
    analysis = "count",
    data = "PM10_Veri_Mevcudiyeti",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 0
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM10 veri alınan istasyon sayısı",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_3_1",
    analysis = "list",
    data = "PM10_Veri_Mevcudiyeti",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM10 %90 ve üstü veri alınan istasyon listesi",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_4_1",
    analysis = "count",
    data = "PM10_Veri_Mevcudiyeti",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM10 %90 ve üstü veri alınan istasyon sayısı",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_3_2",
    analysis = "list",
    data = "PM10_Veri_Mevcudiyeti",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 75
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM10 %75 ve üstü veri alınan istasyon listesi",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_4_2",
    analysis = "count",
    data = "PM10_Veri_Mevcudiyeti",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 75
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM10 %75 ve üstü veri alınan istasyon sayısı",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_5_1",
    analysis = "list",
    data = "PM10_Ortalaması",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Her bir istasyonun yıllık PM10 ortalaması (90% üstü veri mevcudiyeti)",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_5_2",
    analysis = "list",
    data = "PM10_Ortalaması",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 75
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Her bir istasyonun yıllık PM10 ortalaması (75% üstü veri mevcudiyeti)",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_6",
    analysis = "list",
    data = "PM10_Ortalaması",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      ),
      list(
        filter = "PM10_Ortalaması",
        direction = "above",
        value = 40
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Yıllık PM10 ortalaması 40 µg/m3'ün üstündeki istasyonların listesi ve ortalamaları",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_7",
    analysis = "list",
    data = "PM10_Ortalaması",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      ),
      list(
        filter = "PM10_Ortalaması",
        direction = "below",
        value = 40
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Yıllık PM10 ortalaması 40 µg/m3'ün altında olan istasyonların listesi ve ortalamaları",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_8",
    analysis = "list",
    data = "PM10_Ortalaması",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      ),
      list(
        filter = "PM10_Ortalaması",
        direction = "above",
        value = 15
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Yıllık PM10 ortalaması 15 µg/m3'ün üstündeki istasyonların listesi ve ortalamaları",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_9",
    analysis = "list",
    data = "PM10_Ortalaması",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      ),
      list(
        filter = "PM10_Ortalaması",
        direction = "below",
        value = 15
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Yıllık PM10 ortalaması 15 µg/m3'ün altında olan istasyonların listesi ve ortalamaları",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_10",
    analysis = "list",
    data = "PM10_50_Üstü_Veri_Sayısı",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Günlük PM10 ortalaması 50 µg/m3'ün üstündeki istasyonların listesi ve gün sayısı",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_11",
    analysis = "list",
    data = "PM10_50_Altı_Veri_Sayısı",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Günlük PM10 ortalaması 50 µg/m3'ün altında olan istasyonların listesi ve gün sayısı",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_12",
    analysis = "list",
    data = "PM10_45_Üstü_Veri_Sayısı",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Günlük PM10 ortalaması 45 µg/m3'ün üstündeki istasyonların listesi ve gün sayısı",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_13",
    analysis = "list",
    data = "PM10_45_Altı_Veri_Sayısı",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Günlük PM10 ortalaması 45 µg/m3'ün altında olan istasyonların listesi ve gün sayısı",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_14_1",
    analysis = "list",
    data = "PM10_Ortalaması",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "city",
    description = "İl PM10 yıllık ortalaması (90% üstü veri mevcudiyeti)",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM10_14_2",
    analysis = "list",
    data = "PM10_Ortalaması",
    filters = toJSON(list(
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 75
      )
    ), auto_unbox = TRUE),
    group_by = "city",
    description = "İl PM10 yıllık ortalaması (75% üstü veri mevcudiyeti)",
    is_default = TRUE
  ))

# PM25 final analysis
message("Adding PM25 final analysis...")
analysis <- analysis %>%
  rbind(data.frame(
    name = "PM25_1",
    analysis = "list",
    data = "PM25_Veri_Mevcudiyeti",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 0
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM25 veri alınan istasyon listesi",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_2",
    analysis = "count",
    data = "PM25_Veri_Mevcudiyeti",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 0
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM25 veri alınan istasyon sayısı",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_3",
    analysis = "list",
    data = "PM25_Veri_Mevcudiyeti",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM25 %90 ve üstü veri alınan istasyon listesi",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_4",
    analysis = "count",
    data = "PM25_Veri_Mevcudiyeti",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM25 %90 ve üstü veri alınan istasyon sayısı",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_5",
    analysis = "list",
    data = "PM25_Veri_Mevcudiyeti",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 75
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM25 %75 ve üstü veri alınan istasyon listesi",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_6",
    analysis = "count",
    data = "PM25_Veri_Mevcudiyeti",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 75
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM25 %75 ve üstü veri alınan istasyon sayısı",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_7_1",
    analysis = "list",
    data = "PM25_Ortalaması",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Her bir istasyonun yıllık PM25 ortalaması (90% üstü veri mevcudiyeti)",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_7_2",
    analysis = "list",
    data = "PM25_Ortalaması",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 75
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Her bir istasyonun yıllık PM25 ortalaması (75% üstü veri mevcudiyeti)",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_8",
    analysis = "list",
    data = "PM25_Ortalaması",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      ),
      list(
        filter = "PM25_Ortalaması",
        direction = "above",
        value = 15
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Yıllık PM25 ortalaması 15 µg/m3'ün üstündeki istasyonların listesi ve ortalamaları",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_9",
    analysis = "list",
    data = "PM25_Ortalaması",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      ),
      list(
        filter = "PM25_Ortalaması",
        direction = "below",
        value = 15
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Yıllık PM25 ortalaması 15 µg/m3'ün altında olan istasyonların listesi ve ortalamaları",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_10",
    analysis = "list",
    data = "PM25_15_Üstü_Veri_Sayısı",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Günlük PM25 ortalaması 15 µg/m3'ün üstündeki istasyonların listesi ve gün sayısı",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_12",
    analysis = "list",
    data = "PM25_15_Altı_Veri_Sayısı",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "Günlük PM25 ortalaması 15 µg/m3'ün altında olan istasyonların listesi ve gün sayısı",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_13_1",
    analysis = "list",
    data = "PM25_Tamamlanmış",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 75
      ),
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM25 veri yüzdesi %75'in altında olan ve PM10 veri yüzdesi %90'ın üstü istasyonlar için istasyonların PM10 yıllık ortalamalarından 0,6667 faktörü ile çarpılarak elde edilerek hesaplanan PM2.5 yıllık ortalamaları",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_13_2",
    analysis = "list",
    data = "PM25_Tamamlanmış",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 75
      ),
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 75
      )
    ), auto_unbox = TRUE),
    group_by = "station",
    description = "PM25 veri yüzdesi %75'in altında olan ve PM10 veri yüzdesi %75'in üstü istasyonlar için istasyonların PM10 yıllık ortalamalarından 0,6667 faktörü ile çarpılarak elde edilerek hesaplanan PM2.5 yıllık ortalamaları",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_14_1",
    analysis = "list",
    data = "PM25_Tamamlanmış",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      ),
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 90
      )
    ), auto_unbox = TRUE),
    group_by = "city",
    description = "Tamamlanmış PM25 İl yıllık ortalamaları 90% PM10 veri alınan istasyonlar için",
    is_default = TRUE
  )) %>%
  rbind(data.frame(
    name = "PM25_14_2",
    analysis = "list",
    data = "PM25_Tamamlanmış",
    filters = toJSON(list(
      list(
        filter = "PM25_Veri_Mevcudiyeti",
        direction = "above",
        value = 75
      ),
      list(
        filter = "PM10_Veri_Mevcudiyeti",
        direction = "above",
        value = 75
      )
    ), auto_unbox = TRUE),
    group_by = "city",
    description = "Tamamlanmış PM25 İl yıllık ortalamaları 75% PM10 veri alınan istasyonlar için",
    is_default = TRUE
  ))

# SO2 final analysis
message("Adding SO2 final analysis...")


# Save analysis to database
message("Saving final analysis to database...")
dbAppendTable(conn, "final_analysis", analysis)

# Disconnect from the database
dbDisconnect(conn)