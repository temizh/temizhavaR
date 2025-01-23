library(RSelenium)
library(netstat)
library(wdman)
library(uuid)
library(DBI)

clickByXpath <- function(driver, xp) {
  element <- tryCatch({
    driver$findElement(using = 'xpath', value = xp)
  }, error = function(e) {
    message("Element not found: ", xp)
    return(NULL)
  })
  if (!is.null(element)) element$clickElement()
}

tryCatch(
  {
    #https://www.zenrows.com/blog/rselenium#installation

    selenium_server <- rsDriver(browser = "chrome", port = 4445L, chromever = "latest", verbose = T)
   

    driver <- selenium_server$client

    driver$open()
    print("Remote driver opened successfully")
    url <- "https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew"
    print("Navigating to URL...")
    driver$navigate(url)
    print("Navigation successful")
  },
  error = function(e) {
    message("Error occurred during navigation: ", e)
    if (exists("driver")) {
      driver$close()
    }
  }
)

DBDIR <- "/home/byte/Desktop/Work/temizhavaR/"
mydb <- dbConnect(RSQLite::SQLite(), paste0(DBDIR, "temiz-hava.sqlite"))
dbExecute(mydb, "DROP TABLE IF EXISTS location_2023")
dbExecute(mydb, "CREATE TABLE location_2023 (Bolge TEXT, Sehir TEXT, Plaka TEXT, Istasyonlar TEXT, Id TEXT)")
source("extdata/plaka_list.R")

wait <- driver$setTimeout(type = "script", milliseconds = 5000)

dropdown12_element <- tryCatch({
  driver$findElement(using = 'id', value = 'dropdown12-contentDataDowloadNew')
}, error = function(e) {
  message("Element not found within the timeout.")
  NULL
})
dropdown12_element$clickElement()
Sys.sleep(3)

dropdown12_list_items <- driver$findElements(using = "css", value = ".k-reset li")

bolge_list <- sapply(dropdown12_list_items, function(item) item$getElementText()[[1]])
bolge_list_dolu <- bolge_list[bolge_list != "" & bolge_list != "None"]

print("Bolge list retrieved")

clickByXpath(driver, '//*[@id="page-wrapper"]/div[1]')

for (bolge in bolge_list_dolu) {
  print(paste("Processing bolge:", bolge))
  clickByXpath(driver, '//*[@id="dropdown1-contentDataDowloadNew"]/div/div/div/div/span[2]')
  clickByXpath(driver, '//*[@id="dropdown12-contentDataDowloadNew"]/div[1]/div/div/div/span[2]')
  
  dropdown12_element <- driver$findElement(using = 'id', value = 'dropdown12-contentDataDowloadNew')
  dropdown12_element$clickElement()
  Sys.sleep(3)
  
  bolge_item <- driver$findElement(using = 'xpath', value = paste0("//li[contains(text(), '", bolge, "')]"))
  bolge_item$clickElement()
  Sys.sleep(3)
  
  clickByXpath(driver, '//*[@id="page-wrapper"]/div[1]')
  clickByXpath(driver, '//*[@id="dropdown1-contentDataDowloadNew"]/div/div/div/div/span[2]')
  
  dropdown1_element <- driver$findElement(using = 'id', value = 'dropdown1-contentDataDowloadNew')
  dropdown1_element$clickElement()
  Sys.sleep(3)
  
  dropdown1_list_items <- driver$findElements(using = "css", value = ".k-reset li")
  sehir_list <- sapply(dropdown1_list_items, function(item) item$getElementText()[[1]])
  sehir_list_dolu <- sehir_list[sehir_list != ""]
  sehir_list_dolu <- sehir_list_dolu[-1]
  
  for (sehir in sehir_list_dolu) {
    print(paste("Processing sehir:", sehir))
    clickByXpath(driver, '//*[@id="dropdown1-contentDataDowloadNew"]/div/div/div/div/span[2]')
    Sys.sleep(3)
    
    dropdown1_element <- driver$findElement(using = 'id', value = 'dropdown1-contentDataDowloadNew')
    dropdown1_element$clickElement()
    Sys.sleep(3)
    
    sehir_item <- driver$findElement(using = 'xpath', value = paste0("//li[contains(text(), '", sehir, "')]"))
    sehir_item$clickElement()
    Sys.sleep(3)
    
    clickByXpath(driver, '//*[@id="page-wrapper"]/div[1]')
    clickByXpath(driver, '//*[@id="dropdown2-contentDataDowloadNew"]')
    Sys.sleep(3)
    
    dropdown_element_list2 <- driver$findElements(using = 'css', value = '.k-reset li')
    station_list <- sapply(dropdown_element_list2, function(item) item$getElementText()[[1]])
    station_list_dolu <- station_list[station_list != ""]
    station_list_dolu <- station_list_dolu[-c(1, 2)]
    
    for (istasyon in station_list_dolu) {
      print(paste("Processing istasyon:", istasyon))
      id <- UUIDgenerate()
      plaka <- plaka_list[[sehir]]
      
      dbExecute(mydb, "INSERT INTO location_2023 (Bolge, Sehir, Plaka, Istasyonlar, Id) VALUES (?, ?, ?, ?, ?)",
                params = list(bolge, sehir, plaka, istasyon, id))
    }
    
    clickByXpath(driver, '//*[@id="page-wrapper"]/div[1]')
    Sys.sleep(3)
  }
  
  clickByXpath(driver, '//*[@id="page-wrapper"]/div[1]')
  Sys.sleep(3)
}

dbDisconnect(mydb)

driver$close()

dropdown1_element <- driver$findElement(using = 'id', value = 'dropdown1-contentDataDowloadNew')
dropdown1_element$clickElement()
Sys.sleep(2)

dropdown1_list_items <- driver$findElements(using = "css", value = ".k-reset li")
sehir_list <- sapply(dropdown1_list_items, function(item) item$getElementText()[[1]])
sehir_list_dolu <- sehir_list[sehir_list != ""]

clickByXpath(driver, '//*[@id="page-wrapper"]/div[1]')

clickByXpath(driver, '//*[@id="dropdown2-contentDataDowloadNew"]')
Sys.sleep(2)
dropdown_element_list2 <- driver$findElements(using = 'css', value = '.k-reset li')
station_list <- sapply(dropdown_element_list2, function(item) item$getElementText()[[1]])
station_list_dolu <- station_list[station_list != ""]

dropdown12_element <- driver$findElement(using = 'id', value = 'dropdown12-contentDataDowloadNew')
dropdown12_element$clickElement()
Sys.sleep(2)

dropdown12_list_items <- driver$findElements(using = "css", value = ".k-reset li")

bolge_list <- sapply(dropdown12_list_items, function(item) item$getElementText()[[1]])
bolge_list_dolu <- bolge_list[bolge_list != ""]

query <- "SELECT * FROM location_deneme LIMIT 10"
result <- dbGetQuery(mydb, query)

print(result)

dbDisconnect(mydb)

driver$close()


