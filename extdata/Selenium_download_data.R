library(RSelenium)
library(netstat)
library(wdman)
library(uuid)
library(DBI)
library(RSQLite)
library(temizhavaR)

#dir.create(result_dir, recursive = TRUE)

result_dir = "C:/TemizHava_result_2023/hourly"

mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

location_2023 <- dbReadTable(mydb, "location_2023")

# Setup Selenium with Chrome options to specify download directory
eCaps <- list(chromeOptions = list(prefs = list(
  "download.default_directory" = normalizePath(result_dir),
  "download.prompt_for_download" = FALSE,
  "download.directory_upgrade" = TRUE,
  "safebrowsing.enabled" = TRUE
)))

selenium()
selenium_object <- selenium(retcommand = T, check= F)

remote_driver <- rsDriver(browser = "chrome", chromever = "123.0.6312.86",  extraCapabilities = eCaps, verbose = F, port = free_port())
remDr <- remote_driver$client

remDr$open()
remDr$navigate("https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew")

#fonksiyonu calıstırmadan once result_dir'de bulunan file sayisi
previous_file_count <- 0

for (i in 1:nrow(location_2023)) {
  download_data(
    bolge = location_2023$Bolge[i],
    sehir = location_2023$Sehir[i],
    istasyon = location_2023$Istasyonlar[i],
    data_type = "hourly",  # or "daily" depending on your requirement
    startdate = "01.01.2023",
    enddate = "01.01.2024",
    result_dir = result_dir
  )

  Sys.sleep(20)
}

remDr$close()
remote_driver$server$stop()

dbDisconnect(mydb)




















