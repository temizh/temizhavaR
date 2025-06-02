library(glue)
library(DBI)
library(googlesheets4)
library(googledrive)

get_view_names <- function(con, schema) {

  # Filter the objects to get only views
  views <- dbGetQuery(con, glue("SELECT table_name FROM information_schema.views WHERE table_schema = '{schema}'"))

  return(views)
}

setup_google_auth <- function(email = NULL) {
  tryCatch({
    drive_auth(cache = ".secrets", email = email, scopes = "https://www.googleapis.com/auth/drive")
    gs4_auth(token = drive_token())
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

export_to_google_sheets <- function(ss, data, folder_id, view_name, sheet_name) {
  tryCatch({
    sheet_write(data, ss = ss, sheet = sheet_name)
    drive_mv(file = as_id(ss), path = as_id(folder_id))
    message(paste0("Exported ", view_name, " to Google Sheets", "folder_id: ", folder_id))
    return(TRUE)
  }, error = function(e) {
    message(e)
    return(FALSE)
  })
}

save_views_to_drive <- function(schema_name, folder_id, parameters) {
  message("Authenticating to Google Drive...")
  setup_google_auth()
  
  message("Connecting to the database...")
  # Get the views
  con <- create_postgres_conn()

  all_views <- get_view_names(con, schema_name)$table_name

  # Creating excels
  message("Creating excels...")
  message(str(all_views))
  message("pm10_1" %in% all_views)
  # PM10 ----------------------------
  PM10 <- list()
  if(!"pm10_1" %in% all_views) {
    message("PM10_1 view does not exist.")
  } else {
    PM10$PM10_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_1")),
      description = "PM10 Veri alınan istasyon listesi"
    )
  }
  if(!"pm10_2" %in% all_views) {
    message("PM10_2 view does not exist.")
  } else {
    PM10$PM10_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_2")),
      description = "PM10 Veri alınan istasyon sayısı"
    )
  }
  if(!"pm10_3_1" %in% all_views) {
    message("PM10_3_1 view does not exist.")
  } else {
    PM10$PM10_3_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_3_1")),
      description = "PM10 %90 veri alınan istasyon listesi"
    )
  }
  if(!"pm10_3_2" %in% all_views) {
    message("PM10_3_2 view does not exist.")
  } else {
    PM10$PM10_3_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_3_2")),
      description = "PM10 %90 veri alınan istasyon sayısı"
    )
  }
  if(!"pm10_4_1" %in% all_views) {
    message("PM10_4_1 view does not exist.")
  } else {
    PM10$PM10_4_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_4_1")),
      description = "PM10 %75 veri alınan istasyon listesi"
    )
  }
  if(!"pm10_4_2" %in% all_views) {
    message("PM10_4_2 view does not exist.")
  } else {
    PM10$PM10_4_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_4_2")),
      description = "PM10 %75 veri alınan istasyon sayısı"
    )
  }
  if(!"pm10_5_1" %in% all_views) {
    message("PM10_5_1 view does not exist.")
  } else {
    PM10$PM10_5_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_5_1")),
      description = "Her bir istasyonun yıllık PM10 ortalaması (90% üstü veri mevcudiyeti)"
    )
  }
  if(!"pm10_5_2" %in% all_views) {
    message("PM10_5_2 view does not exist.")
  } else {
    PM10$PM10_5_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_5_2")),
      description = "Her bir istasyonun yıllık PM10 ortalaması (75% üstü veri mevcudiyeti)"
    )
  }
  if(!"pm10_6" %in% all_views) {
    message("PM10_6 view does not exist.")
  } else {
    PM10$PM10_6 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_6")),
      description = "Yıllık PM10 ortalaması 40 µg/m3'ün üstündeki istasyonların listesi ve ortalamaları"
    )
  }
  if(!"pm10_7" %in% all_views) {
    message("PM10_7 view does not exist.")
  } else {
    PM10$PM10_7 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_7")),
      description = "Yıllık PM10 ortalaması 40 µg/m3'ün altı istasyonların listesi ve ortalamaları"
    )
  }
  if(!"pm10_8" %in% all_views) {
    message("PM10_8 view does not exist.")
  } else {
    PM10$PM10_8 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_8")),
      description = "Yıllık PM10 ortalaması 15 µg/m3'ün üstündeki istasyonların listesi ve ortalamaları"
    )
  }
  if(!"pm10_9" %in% all_views) {
    message("PM10_9 view does not exist.")
  } else {
    PM10$PM10_9 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_9")),
      description = "Yıllık PM10 ortalaması 15 µg/m3'ün altı istasyonların listesi ve ortalamaları"
    )
  }
  if(!"pm10_10" %in% all_views) {
    message("PM10_10 view does not exist.")
  } else {
    PM10$PM10_10 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_10")),
      description = "Günlük PM10 ortalaması 50 µg/m3'ün üstündeki istasyonların listesi ve gün sayısı"
    )
  }
  if(!"pm10_12" %in% all_views) {
    message("PM10_12 view does not exist.")
  } else {
    PM10$PM10_12 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_12")),
      description = "Günlük PM10 ortalaması 45 µg/m3'ün altı istasyonların listesi ve gün sayısı"
    )
  }
  if(!"pm10_14_1" %in% all_views) {
    message("PM10_14_1 view does not exist.")
  } else {
    PM10$PM10_14_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_14_1")),
      description = "İl PM10 yıllık ortalaması (90% üstü veri mevcudiyeti)"
    )
  }
  if(!"pm10_14_2" %in% all_views) {
    message("PM10_14_2 view does not exist.")
  } else {
    PM10$PM10_14_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_14_2")),
      description = "İl PM10 yıllık ortalaması (75% üstü veri mevcudiyeti)"
    )
  } 

  # PM25 ----------------------------
  PM25 <- list()
  if (!"pm25_1" %in% all_views) {
    message("PM25_1 view does not exist.")
  } else {
    PM25$PM25_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_1")),
      description = "PM25 Veri alınan istasyon listesi"
    )
  }
  if (!"pm25_2" %in% all_views) {
    message("PM25_2 view does not exist.")
  } else {
    PM25$PM25_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_2")),
      description = "PM25 Veri alınan istasyon sayısı"
    )
  }
  if (!"pm25_3" %in% all_views) {
    message("PM25_3 view does not exist.")
  } else {
    PM25$PM25_3 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_3")),
      description = "PM25 %90 veri alınan istasyon listesi"
    )
  }
  if (!"pm25_4" %in% all_views) {
    message("PM25_4 view does not exist.")
  } else {
    PM25$PM25_4 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_4")),
      description = "PM25 %90 veri alınan istasyon sayısı"
    )
  }
  if (!"pm25_5" %in% all_views) {
    message("PM25_5 view does not exist.")
  } else {
    PM25$PM25_5 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_5")),
      description = "PM25 %75 veri alınan istasyon listesi"
    )
  }
  if (!"pm25_6" %in% all_views) {
    message("PM25_6 view does not exist.")
  } else {
    PM25$PM25_6 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_6")),
      description = "PM25 %75 veri alınan istasyon sayısı"
    )
  }
  if (!"pm25_7_1" %in% all_views) {
    message("PM25_7_1 view does not exist.")
  } else {
    PM25$PM25_7_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_7_1")),
      description = "Her bir istasyonun yıllık PM25 ortalaması (90% üstü veri mevcudiyeti)"
    )
  }
  if (!"pm25_7_2" %in% all_views) {
    message("PM25_7_2 view does not exist.")
  } else {
    PM25$PM25_7_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_7_2")),
      description = "Her bir istasyonun yıllık PM25 ortalaması (75% üstü veri mevcudiyeti)"
    )
  }
  if (!"pm25_8" %in% all_views) {
    message("PM25_8 view does not exist.")
  } else {
    PM25$PM25_8 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_8")),
      description = "Yıllık PM25 ortalaması 5 µg/m3'ün üstündeki istasyonların listesi ve ortalamaları"
    )
  }
  if (!"pm25_9" %in% all_views) {
    message("PM25_9 view does not exist.")
  } else {
    PM25$PM25_9 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_9")),
      description = "Yıllık PM25 ortalaması 5 µg/m3'ün altı istasyonların listesi ve ortalamaları"
    )
  }
  if (!"pm25_10" %in% all_views) {
    message("PM25_10 view does not exist.")
  } else {
    PM25$PM25_10 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_10")),
      description = "Günlük PM25 ortalaması 15 µg/m3'ün üstündeki istasyonların listesi ve gün sayısı"
    )
  }
  if (!"pm25_12" %in% all_views) {
    message("PM25_12 view does not exist.")
  } else {
    PM25$PM25_12 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_12")),
      description = "Günlük PM25 ortalaması 15 µg/m3'ün altı istasyonların listesi ve gün sayısı"
    )
  }
  if (!"pm25_13_1" %in% all_views) {
    message("PM25_13_1 view does not exist.")
  } else {
    PM25$PM25_13_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_13_1")),
      description = "PM25 veri yüzdesi %75'in altında olan ve PM10 veri yüzdesi %90'ın üstü istasyonlar için istasyonların PM10 yıllık ortalamalarından 0,6667 faktörü ile çarpılarak elde edilerek hesaplanan PM2.5 yıllık ortalamaları",
      additional = list(
        list(data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_5_1")), description = "Her bir istasyonun yıllık PM10 ortalaması (90% üstü veri mevcudiyeti)"),
        list(data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_7_2")), description = "Her bir istasyonun yıllık PM25 ortalaması (75% üstü veri mevcudiyeti)"),
        list(data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_1")), description = "PM10 Veri mevcudiyeti"),
        list(data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_1")), description = "PM25 Veri mevcudiyeti")
      )
    )
  }
  if (!"pm25_13_2" %in% all_views) {
    message("PM25_13_2 view does not exist.")
  } else {
    PM25$PM25_13_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_13_2")),
      description = "PM25 veri yüzdesi %75'in altında olan ve PM10 veri yüzdesi %75'ın üstü istasyonlar için istasyonların PM10 yıllık ortalamalarından 0,6667 faktörü ile çarpılarak elde edilerek hesaplanan PM2.5 yıllık ortalamaları",
      additional = list(
        list(data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_5_2")), description = "Her bir istasyonun yıllık PM10 ortalaması (75% üstü veri mevcudiyeti)"),
        list(data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_7_2")), description = "Her bir istasyonun yıllık PM25 ortalaması (75% üstü veri mevcudiyeti)"),
        list(data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM10_1")), description = "PM10 Veri mevcudiyeti"),
        list(data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_1")), description = "PM25 Veri mevcudiyeti")
      )
    )
  }
  if (!"pm25_14_1" %in% all_views) {
    message("PM25_14_1 view does not exist.")
  } else {
    PM25$PM25_14_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_14_1")),
      description = "Tamamlanmış PM25 İl yıllık ortalamaları 90% PM10 veri alınan istasyonlar için",
    )
  }
  if (!"pm25_14_2" %in% all_views) {
    message("PM25_14_2 view does not exist.")
  } else {
    PM25$PM25_14_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".PM25_14_2")),
      description = "Tamamlanmış PM25 il yıllık ortalamaları 75% PM10 veri alınan istasyonlar için",
    )
  }

  # SO2 ----------------------------
  SO2 <- list()
  if (!"SO2_1" %in% all_views) {
    message("SO2_1 view does not exist.")
  } else {
    SO2$SO2_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".SO2_1")),
      description = "SO2 Veri alınan istasyon listesi"
    )
  }
  if (!"SO2_2_1" %in% all_views) {
    message("SO2_2_1 view does not exist.")
  } else {
    SO2$SO2_2_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".SO2_2_1")),
      description = "SO2 %90 veri alınan istasyon listesi"
    )
  }
  if (!"SO2_3_1" %in% all_views) {
    message("SO2_3_1 view does not exist.")
  } else {
    SO2$SO2_3_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".SO2_3_1")),
      description = "SO2 %90 veri alınan istasyon sayısı"
    )
  }
  if (!"SO2_2_2" %in% all_views) {
    message("SO2_2_2 view does not exist.")
  } else {
    SO2$SO2_2_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".SO2_2_2")),
      description = "SO2 %75 veri alınan istasyon listesi"
    )
  }
  if (!"SO2_3_2" %in% all_views) {
    message("SO2_3_2 view does not exist.")
  } else {
    SO2$SO2_3_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".SO2_3_2")),
      description = "SO2 %75 veri alınan istasyon sayısı"
    )
  }
  if (!"SO2_4" %in% all_views) {
    message("SO2_4 view does not exist.")
  } else {
    SO2$SO2_4 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".SO2_4")),
      description = "Her bir istasyonun yıllık SO2 ortalaması"
    )
  }
  if (!"SO2_7" %in% all_views) {
    message("SO2_7 view does not exist.")
  } else {
    SO2$SO2_7 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".SO2_7")),
      description = "Günlük SO2 ortalaması 40 µg/m3'ün üstündeki istasyonların listesi ve gün sayısı"
    )
  }
  if (!"SO2_8" %in% all_views) {
    message("SO2_8 view does not exist.")
  } else {
    SO2$SO2_8 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".SO2_8")),
      description = "Günlük SO2 ortalaması 125 µg/m3'ün üstündeki istasyonların listesi ve gün sayısı"
    )
  }
  if (!"SO2_9" %in% all_views) {
    message("SO2_9 view does not exist.")
  } else {
    SO2$SO2_9 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".SO2_9")),
      description = "Günlük ortalaması 125 µg/m3'ü 3 defadan fazla aşan istasyonlar ve kaç defa aştıkları"
    )
  }
  if (!"SO2_11" %in% all_views) {
    message("SO2_11 view does not exist.")
  } else {
    SO2$SO2_11 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".SO2_11")),
      description = "Her bir istasyonun yıllık SO2 ortalaması"
    )
  }

  # NO2 ----------------------------
  NO2 <- list()
  if (!"NO2_1" %in% all_views) {
    message("NO2_1 view does not exist.")
  } else {
    NO2$NO2_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NO2_1")),
      description = "NO2 Veri alınan istasyon listesi"
    )
  }
  if (!"NO2_2_1" %in% all_views) {
    message("NO2_2_1 view does not exist.")
  } else {
    NO2$NO2_2_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NO2_2_1")),
      description = "NO2 %90 veri alınan istasyon listesi"
    )
  }
  if (!"NO2_3_1" %in% all_views) {
    message("NO2_3_1 view does not exist.")
  } else {
    NO2$NO2_3_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NO2_3_1")),
      description = "NO2 %90 veri alınan istasyon sayısı"
    )
  }
  if (!"NO2_2_2" %in% all_views) {
    message("NO2_2_2 view does not exist.")
  } else {
    NO2$NO2_2_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NO2_2_2")),
      description = "NO2 %75 veri alınan istasyon listesi"
    )
  }
  if (!"NO2_3_2" %in% all_views) {
    message("NO2_3_2 view does not exist.")
  } else {
    NO2$NO2_3_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NO2_3_2")),
      description = "NO2 %75 veri alınan istasyon sayısı"
    )
  }
  if (!"NO2_5" %in% all_views) {
    message("NO2_5 view does not exist.")
  } else {
    NO2$NO2_5 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NO2_5")),
      description = "Günlük ortalaması 25  µg/m3'ü 3 defadan fazla aşan istasyonlar ve kaç defa aştıkları"
    )
  }
  if (!"NO2_6" %in% all_views) {
    message("NO2_6 view does not exist.")
  } else {
    NO2$NO2_6 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NO2_6")),
      description = "Her bir istasyonun yıllık NO2 ortalaması"
    )
  }
  if (!"NO2_7" %in% all_views) {
    message("NO2_7 view does not exist.")
  } else {
    NO2$NO2_7 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NO2_7")),
      description = "Yıllık NO2 ortalaması 25 µg/m3'ün üstündeki istasyonların listesi ve ortalamaları"
    )
  }
  if (!"NO2_8" %in% all_views) {
    message("NO2_8 view does not exist.")
  } else {
    NO2$NO2_8 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NO2_8")),
      description = "Yıllık NO2 ortalaması 10 µg/m3'ün üstündeki istasyonların listesi ve ortalamaları"
    )
  }
  if (!"NO2_9" %in% all_views) {
    message("NO2_9 view does not exist.")
  } else {
    NO2$NO2_9 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NO2_9")),
      description = "İl NO2 yıllık ortalaması"
    )
  }

  # NOX ----------------------------
  NOX <- list()
  if (!"NOX_1" %in% all_views) {
    message("NOX_1 view does not exist.")
  } else {
    NOX$NOX_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NOX_1")),
      description = "NOX Veri alınan istasyon listesi"
    )
  }
  if (!"NOX_2_1" %in% all_views) {
    message("NOX_2_1 view does not exist.")
  } else {
    NOX$NOX_2_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NOX_2_1")),
      description = "NOX %90 veri alınan istasyon listesi"
    )
  }
  if (!"NOX_3_1" %in% all_views) {
    message("NOX_3_1 view does not exist.")
  } else {
    NOX$NOX_3_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NOX_3_1")),
      description = "NOX %90 veri alınan istasyon sayısı"
    )
  }
  if (!"NOX_2_2" %in% all_views) {
    message("NOX_2_2 view does not exist.")
  } else {
    NOX$NOX_2_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NOX_2_2")),
      description = "NOX %75 veri alınan istasyon listesi"
    )
  }
  if (!"NOX_3_2" %in% all_views) {
    message("NOX_3_2 view does not exist.")
  } else {
    NOX$NOX_3_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NOX_3_2")),
      description = "NOX %75 veri alınan istasyon sayısı"
    )
  }
  if (!"NOX_4" %in% all_views) {
    message("NOX_4 view does not exist.")
  } else {
    NOX$NOX_4 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".NOX_4")),
      description = "Yıllık ortalaması 30 µg/m3'ü aşan istasyonların listesi ve ortalamaları"
    )
  }

  # O3 ----------------------------
  O3 <- list()
  if (!"O3_1" %in% all_views) {
    message("O3_1 view does not exist.")
  } else {
    O3$O3_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_1")),
      description = "O3 Veri alınan istasyon listesi"
    )
  }
  if (!"O3_2" %in% all_views) {
    message("O3_2 view does not exist.")
  } else {
    O3$O3_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_2")),
      description = "O3 Veri alınan istasyon sayısı"
    )
  }
  if (!"O3_3" %in% all_views) {
    message("O3_3 view does not exist.")
  } else {
    O3$O3_3 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_3")),
      description = "O3 %90 veri alınan istasyon listesi"
    )
  }
  if (!"O3_4" %in% all_views) {
    message("O3_4 view does not exist.")
  } else {    
    O3$O3_4 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_4")),
      description = "O3 %90 veri alınan istasyon sayısı"
    )
  }
  if (!"O3_5" %in% all_views) {
    message("O3_5 view does not exist.")
  } else {
    O3$O3_5 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_5")),
      description = "Yaz boyunca O3 %90 ve üstü veri alınan istasyon listesi "
    )
  }
  if (!"O3_6" %in% all_views) {
    message("O3_6 view does not exist.")
  } else {
    O3$O3_6 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_6")),
      description = "Yaz boyunca %90 ve üstü veri alınan istasyon sayısı"
    )
  }
  if (!"O3_7" %in% all_views) {
    message("O3_7 view does not exist.")
  } else {
    O3$O3_7 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_7")),
      description = "Kış boyunca O3 %75 ve üstü veri alınan istasyon listesi"
    )
  }
  if (!"O3_8" %in% all_views) {
    message("O3_8 view does not exist.")
  } else {
    O3$O3_8 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_8")),
      description = "Kış boyunca %75 ve üstü veri alınan istasyon sayısı"
    )
  }
  if (!"o3_9_1" %in% all_views) {
    message("O3_9_1 view does not exist.")
  } else {
    O3$O3_9_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_9_1")),
      description = "AOT40 değerini hesaplamak için tanımlanan süre için 1 saatlik değerlerin %90 ve üstü veri alınan istasyon listesi (Mayıs, Temmuz)"
    )
  }
  if (!"o3_9_2" %in% all_views) {
    message("O3_9_2 view does not exist.")
  } else {
    O3$O3_9_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_9_2")),
      description = "AOT40 değerini hesaplamak için tanımlanan süre için 1 saatlik değerlerin %90 ve üstü veri alınan istasyon listesi (Nisan, Eylül)"
    )
  }
  if (!"o3_10_1" %in% all_views) {
    message("O3_10_1 view does not exist.")
  } else {
    O3$O3_10_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_10_1")),
      description = "AOT40 değerini hesaplamak için tanımlanan süre için 1 saatlik değerlerin %90 ve üstü veri alınan istasyon sayısı (Mayıs, Temmuz)"
    )
  }
  if (!"o3_10_2" %in% all_views) {
    message("O3_10_2 view does not exist.")
  } else {
    O3$O3_10_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_10_2")),
      description = "AOT40 değerini hesaplamak için tanımlanan süre için 1 saatlik değerlerin %90 ve üstü veri alınan istasyon sayısı (Nisan, Eylül)"
    )
  }
  if (!"o3_11" %in% all_views) {
    message("O3_11 view does not exist.")
  } else {
    tmp <- dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_11"))
    tmp <- tmp %>% mutate_all(as.character)
    O3$O3_11 <- create_analysis_excel(
      data = tmp,
      description = "8 saatlik ortalamaların günlük maksimum değerlerinden 120 µg/m3'ü aşanların sayısı"
    )
  }
  if (!"O3_12" %in% all_views) {
    message("O3_12 view does not exist.")
  } else {
    O3$O3_12 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_12")),
      description = "Mayıs ayından Temmuz ayına kadar AOT40 değerlerinin toplamı"
    )
  }
  if (!"O3_13" %in% all_views) {
    message("O3_13 view does not exist.")
  } else {
    O3$O3_13 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".O3_13")),
      description = "Nisan ayından Eylül ayına kadar AOT40 değerlerinin toplamı"
    )
  }

  # CO ----------------------------
  CO <- list()
  if (!"CO_1" %in% all_views) {
    message("CO_1 view does not exist.")
  } else {
    CO$CO_1 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".CO_1")),
      description = "CO Veri alınan istasyon listesi"
    )
  }
  if (!"CO_2" %in% all_views) {
    message("CO_2 view does not exist.")
  } else {
    CO$CO_2 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".CO_2")),
      description = "CO %90 veri alınan istasyon listesi"
    )
  }
  if (!"CO_3" %in% all_views) {
    message("CO_3 view does not exist.")
  } else {
    CO$CO_3 <- create_analysis_excel(
      data = dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".CO_3")),
      description = "CO %90 veri alınan istasyon sayısı"
    )
  }

  message("Exporting excels to Google Sheets...")
  ss <- gs4_create("PM_10", sheets = names(PM10))
  for (sheet in names(PM10)) {
    export_to_google_sheets(ss, PM10[[sheet]], folder_id, "PM_10", sheet)
  }

  ss <- gs4_create("PM_25", sheets = names(PM25))
  for (sheet in names(PM25)) {
    export_to_google_sheets(ss, PM25[[sheet]], folder_id, "PM_25", sheet)
  }

  ss <- gs4_create("SO2", sheets = names(SO2))
  for (sheet in names(SO2)) {
    export_to_google_sheets(ss, SO2[[sheet]], folder_id, "SO2", sheet)
  }

  ss <- gs4_create("NO2", sheets = names(NO2))
  for (sheet in names(NO2)) {
    export_to_google_sheets(ss, NO2[[sheet]], folder_id, "NO2", sheet)
  }

  ss <- gs4_create("NOX", sheets = names(NOX))
  for (sheet in names(NOX)) {
    export_to_google_sheets(ss, NOX[[sheet]], folder_id, "NOX", sheet)
  }

  ss <- gs4_create("O3", sheets = names(O3))
  for (sheet in names(O3)) {
    export_to_google_sheets(ss, O3[[sheet]], folder_id, "O3", sheet)
  }

  ss <- gs4_create("CO", sheets = names(CO))
  for (sheet in names(CO)) {
    export_to_google_sheets(ss, CO[[sheet]], folder_id, "CO", sheet)
  }
}