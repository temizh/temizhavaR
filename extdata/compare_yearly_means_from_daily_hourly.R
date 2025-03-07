library(writexl)
library(dplyr)

options(temizhavaR.basedir = "/home/acizmeli/Documents/KaraRaporu/HamVeriler_")
#YEAR <- options()$temizhavaR.YEAR

compare_yearly_means_from_daily_hourly <- function(YEAR) {
  init.temizhavaR()


  datadir <- paste0(options()$temizhavaR.base_dir, YEAR)
  stopifnot(dir.exists(datadir))

  conn <- create_postgres_conn()
  #Get location table
  full_stations <- dbGetQuery(conn, paste0("SELECT * FROM location", YEAR))

  all_daily <- dbGetQuery(conn, "SELECT * FROM daily_detail")
  all_hourly <- dbGetQuery(conn, "SELECT * FROM hourly_detail")

  disconnect_postgres(conn)

  all_stations <- sort(unique(full_stations$Istasyon_modifiedlar))
  all_stations <- tibble(Istasyon_modified=all_stations)

  u_stations_daily <- sort(unique(all_daily$Istasyon_modified))
  u_stations_hourly <- sort(unique(all_hourly$Istasyon_modified))

  daily_summary <- all_daily %>%
    select(Istasyon_modified, PM10, PM25, SO2, CO, NO2, NOX, NO, O3) %>%
    group_by(Istasyon_modified) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    rename(PM10_daily=PM10, PM25_daily=PM25, SO2_daily=SO2, CO_daily=CO, NO2_daily=NO2, NOX_daily=NOX, NO_daily=NO, O3_daily=O3)

  hourly_summary <- all_hourly %>%
    select(Istasyon_modified, PM10, PM25, SO2, CO, NO2, NOX, NO, O3) %>%
    group_by(Istasyon_modified) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    rename(PM10_hourly=PM10, PM25_hourly=PM25, SO2_hourly=SO2, CO_hourly=CO, NO2_hourly=NO2, NOX_hourly=NOX, NO_hourly=NO, O3_hourly=O3)

  all_summary <- left_join(all_stations, daily_summary, by = join_by(Istasyon_modified))

  all_summary <- left_join(all_summary, hourly_summary, by = join_by(Istasyon_modified)) %>%
    relocate(PM10_hourly, .after=PM10_daily) %>%
    relocate(PM25_hourly, .after=PM25_daily) %>%
    relocate(SO2_hourly, .after=SO2_daily) %>%
    relocate(CO_hourly, .after=CO_daily) %>%
    relocate(NO2_hourly, .after=NO2_daily) %>%
    relocate(NOX_hourly, .after=NOX_daily) %>%
    relocate(NO_hourly, .after=NO_daily) %>%
    relocate(O3_hourly, .after=O3_daily)

  analiz_output_file <- file.path(base_dir, "__results", "compare_yearly_means_from_daily_hourly.xlsx")

  write_xlsx(
    all_summary,
    path = analiz_output_file,
    col_names = TRUE,
    format_headers = TRUE
  )

  library(gridExtra)
  library(grid)
  library(ggplot2)
  library(lattice)
  p1 <- ggplot(data=all_summary,aes(x=PM10_hourly, y=PM10_daily)) + geom_point()
  p2 <- ggplot(data=all_summary,aes(x=PM25_hourly, y=PM25_daily)) + geom_point()
  p3 <- ggplot(data=all_summary,aes(x=SO2_hourly, y=SO2_daily)) + geom_point()
  p4 <- ggplot(data=all_summary,aes(x=CO_hourly, y=CO_daily)) + geom_point()
  p5 <- ggplot(data=all_summary,aes(x=NO2_hourly, y=NO2_daily)) + geom_point()
  p6 <- ggplot(data=all_summary,aes(x=NOX_hourly, y=NOX_daily)) + geom_point()
  p7 <- ggplot(data=all_summary,aes(x=NO_hourly, y=NO_daily)) + geom_point()
  p8 <- ggplot(data=all_summary,aes(x=O3_hourly, y=O3_daily)) + geom_point()
  tt<-grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=3)
}

#compare_yearly_means_from_daily_hourly("2022")
compare_yearly_means_from_daily_hourly("2023")

#location2023 <- dbReadTable(mydb, paste0("location_", YEAR))
# allstations_2023 <- sort(unique(location2023$Istasyon_modifiedlar))
# allstations_2023 <- gsub("\\/", " ", allstations_2023)
# allstations_2023 <- tibble(Istasyon_modified=allstations_2023)
# #Get 2023 station saatlik/gunluk list
# setwd(base_dir)
# gunlukler_2023 <- list.files(pattern = "_gunluk_detay_", recursive = TRUE)
# gunlukler_2023 <- sapply(strsplit(gunlukler_2023, "/"), function(x) x[[2]])
# gunlukler_2023 <- sapply(strsplit(gunlukler_2023, "_gunluk"), function(x) x[[1]])
# gunlukler_2023 <- data.frame(Istasyon_modified = gunlukler_2023, gunlukler_2023 = TRUE)
# saatlikler_2023 <- list.files(pattern = "_saatlik_detay_", recursive = TRUE, full.names = FALSE, include.dirs	= FALSE)
# saatlikler_2023 <- sapply(strsplit(saatlikler_2023, "/"), function(x) x[[2]])
# saatlikler_2023 <- sapply(strsplit(saatlikler_2023, "_saatlik"), function(x) x[[1]])
# saatlikler_2023 <- data.frame(Istasyon_modified = saatlikler_2023, saatlikler_2023 = TRUE)
# veriler_2023 <- left_join(allstations_2023, gunlukler_2022, by = join_by(Istasyon_modified))
# veriler_2023 <- left_join(veriler_2023, saatlikler_2022, by = join_by(Istasyon_modified))
# init.temizhavaR()
#
# write_xlsx(
#   veriler_2023,
#   path = "../veriler_2023.xlsx",
#   col_names = TRUE,
#   format_headers = TRUE
# )


