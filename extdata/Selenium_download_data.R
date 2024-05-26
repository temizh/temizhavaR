library(RSelenium)
library(netstat)
library(wdman)
library(uuid)
library(DBI)
library(RSQLite)
library(temizhavaR)

#dir.create(result_dir, recursive = TRUE)
result_dir = "C:/TemizHava_raw_data2023"

mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

location_2023 <- dbReadTable(mydb, "location_2023")

eCaps <- list(chromeOptions = list(prefs = list(
  "download.default_directory" = normalizePath(result_dir),
  "download.prompt_for_download" = FALSE,
  "download.directory_upgrade" = TRUE,
  "safebrowsing.enabled" = TRUE
)))

selenium()
selenium_object <- selenium(retcommand = T, check= F)

remote_driver <- rsDriver(browser = "chrome", chromever = "125.0.6422.78",  extraCapabilities = eCaps, verbose = F, port = free_port())
remDr <- remote_driver$client

remDr$open()
remDr$navigate("https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew")


for (i in 1:nrow(location_2023)) {
  city_dir <- file.path(result_dir, location_2023$Sehir[i])
  istasyon <- location_2023$Istasyonlar[i]

  # for hourly data
  if (download_check(city_dir, istasyon, "hourly")) {
    download_data(
      bolge = location_2023$Bolge[i],
      sehir = location_2023$Sehir[i],
      istasyon = istasyon,
      data_type = "hourly",
      startdate = "01.01.2023",
      enddate = "01.01.2024",
      result_dir = result_dir
    )
    Sys.sleep(5)
  }

  # for daily data
  if (download_check(city_dir, istasyon, "daily")) {
    download_data(
      bolge = location_2023$Bolge[i],
      sehir = location_2023$Sehir[i],
      istasyon = istasyon,
      data_type = "daily",
      startdate = "01.01.2023",
      enddate = "01.01.2024",
      result_dir = result_dir
    )
    Sys.sleep(8)
  }
}


remDr$close()
remote_driver$server$stop()

dbDisconnect(mydb)




















#dir.create(result_dir, recursive = TRUE)

# result_dir = "C:/TemizHava_result_2023/hourly"
#
# mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")
#
# location_2023 <- dbReadTable(mydb, "location_2023")
#
# # Setup Selenium with Chrome options to specify download directory
# eCaps <- list(chromeOptions = list(prefs = list(
#   "download.default_directory" = normalizePath(result_dir),
#   "download.prompt_for_download" = FALSE,
#   "download.directory_upgrade" = TRUE,
#   "safebrowsing.enabled" = TRUE
# )))
#
# selenium()
# selenium_object <- selenium(retcommand = T, check= F)
#
# remote_driver <- rsDriver(browser = "chrome", chromever = "123.0.6312.86",  extraCapabilities = eCaps, verbose = F, port = free_port())
# remDr <- remote_driver$client
#
# remDr$open()
# remDr$navigate("https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew")
#
# # son islenen indeks ve istasyon adını tutan dosya
# progress_file <- "progress.txt"
#
# start_index <- 6
# last_station <- ""
# if (file.exists(progress_file)) {
#   progress_data <- readLines(progress_file)
#   start_index <- as.numeric(progress_data[1])
#   last_station <- progress_data[2]
# }
#
# previous_file_count <- length(list.files(result_dir, pattern = "\\.xlsx$", full.names = TRUE))
#
#
# for (i in start_index:nrow(location_2023)) {
#   # Gerekli değişkenleri tanımlama
#   bolge <- location_2023$Bolge[i]
#   sehir <- location_2023$Sehir[i]
#   istasyon <- location_2023$Istasyonlar[i]
#
#   # Veri indirme
#   tryCatch({
#     download_data(
#       bolge = bolge,
#       sehir = sehir,
#       istasyon = istasyon,
#       data_type = "hourly",  # veya ihtiyacınıza göre "daily"
#       startdate = "01.01.2023",
#       enddate = "01.01.2024",
#       result_dir = result_dir,
#       previous_file_count = previous_file_count
#     )
#   }, error = function(e) {
#     message(paste("Hata:", e$message))
#     message(paste("Istasyon:", istasyon, " için indirme işlemi tamamlanamadı veya dosya bulunamadı."))
#   })
#
#   # Her başarılı indirmeden sonra ilerleme dosyasını güncelleme
#   writeLines(c(as.character(i + 1), istasyon), progress_file)
#
#   Sys.sleep(10)
# }
#
# remDr$close()
# remote_driver$server$stop()
#
# dbDisconnect(mydb)
#
 #file.remove(progress_file)
#
#
#
# if (file.exists(progress_file)) {
#   # Dosyanın içeriğini oku
#   progress_data <- readLines(progress_file)
#
#   # İçeriği ekrana yazdır
#   print(progress_data)
# } else {
#   message("Progress file bulunamadı.")
# }













