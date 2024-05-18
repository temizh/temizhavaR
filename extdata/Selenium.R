library(RSelenium)
library(netstat)
library(wdman)

#Yeni bir chromedriver versiyonu oluştuğunda C:\Users\Hp\AppData\Local\binman\binman_chromedriver\win32 buradan lisans belgesini sil

selenium()
selenium_object <- selenium(retcommand = T, check= F)

#google chrome
#binman::list_versions("chromedriver")

remote_driver <- rsDriver(browser = "chrome", chromever = "123.0.6312.86", verbose = F, port = free_port())

#close server
#remote_driver$server$stop()

remDr <- remote_driver$client

remDr$open()
remDr$navigate("https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew")

#BOLGE ISIMLERI

dropdown12_element <- remDr$findElement(using = 'id', value = 'dropdown12-contentDataDowloadNew')
dropdown12_element$clickElement()
Sys.sleep(2)

dropdown12_list_items <- remDr$findElements(using = "css", value = ".k-reset li")

bolge_list <- sapply(dropdown12_list_items, function(item) item$getElementText()[[1]])
bolge_list_dolu <- bolge_list[bolge_list != ""]

#SEHIR ISIMLERI

dropdown1_element <- remDr$findElement(using = 'id', value = 'dropdown1-contentDataDowloadNew')
dropdown1_element$clickElement()
 Sys.sleep(2)

dropdown1_list_items <- remDr$findElements(using = "css", value = ".k-reset li")
sehir_list <- sapply(dropdown1_list_items , function(item) item$getElementText()[[1]])
sehir_list_dolu <- sehir_list[sehir_list != ""]

#ISTASYON ISIMLERI

dropdown_element2 <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown2-contentDataDowloadNew"]')
dropdown_element2$clickElement()
Sys.sleep(2)

dropdown_element_list2 <- remDr$findElements(using = 'css', value = '.k-reset li')
station_list <- sapply(dropdown_element_list2 , function(item) item$getElementText()[[1]])
station_list_dolu <- station_list[station_list != ""]
