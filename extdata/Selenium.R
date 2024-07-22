library(RSelenium)
library(netstat)
library(wdman)
library(uuid)

#Yeni bir chromedriver versiyonu oluştuğunda C:\Users\Hp\AppData\Local\binman\binman_chromedriver\win32 buradan lisans belgesini sil

selenium()
selenium_object <- selenium(retcommand = T, check= F)

#google chrome
#binman::list_versions("chromedriver")

remote_driver <- rsDriver(browser = "chrome", chromever = "125.0.6422.78", verbose = F, port = free_port())

#close server
#remote_driver$server$stop()

remDr <- remote_driver$client

remDr$open()
remDr$navigate("https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew")

mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")


plaka_list <- list(
  "Adana" = "01",
  "Adıyaman" = "02",
  "Afyonkarahisar" = "03",
  "Ağrı" = "04",
  "Amasya" = "05",
  "Ankara" = "06",
  "Antalya" = "07",
  "Artvin" = "08",
  "Aydın" = "09",
  "Balıkesir" = "10",
  "Bilecik" = "11",
  "Bingöl" = "12",
  "Bitlis" = "13",
  "Bolu" = "14",
  "Burdur" = "15",
  "Bursa" = "16",
  "Çanakkale" = "17",
  "Çankırı" = "18",
  "Çorum" = "19",
  "Denizli" = "20",
  "Diyarbakır" = "21",
  "Edirne" = "22",
  "Elazığ" = "23",
  "Erzincan" = "24",
  "Erzurum" = "25",
  "Eskişehir" = "26",
  "Gaziantep" = "27",
  "Giresun" = "28",
  "Gümüşhane" = "29",
  "Hakkari" = "30",
  "Hatay" = "31",
  "Isparta" = "32",
  "Mersin" = "33",
  "İstanbul" = "34",
  "İzmir" = "35",
  "Kars" = "36",
  "Kastamonu" = "37",
  "Kayseri" = "38",
  "Kırklareli" = "39",
  "Kırşehir" = "40",
  "Kocaeli" = "41",
  "Konya" = "42",
  "Kütahya" = "43",
  "Malatya" = "44",
  "Manisa" = "45",
  "Kahramanmaraş" = "46",
  "Mardin" = "47",
  "Muğla" = "48",
  "Muş" = "49",
  "Nevşehir" = "50",
  "Niğde" = "51",
  "Ordu" = "52",
  "Rize" = "53",
  "Sakarya" = "54",
  "Samsun" = "55",
  "Siirt" = "56",
  "Sinop" = "57",
  "Sivas" = "58",
  "Tekirdağ" = "59",
  "Tokat" = "60",
  "Trabzon" = "61",
  "Tunceli" = "62",
  "Şanlıurfa" = "63",
  "Uşak" = "64",
  "Van" = "65",
  "Yozgat" = "66",
  "Zonguldak" = "67",
  "Aksaray" = "68",
  "Bayburt" = "69",
  "Karaman" = "70",
  "Kırıkkale" = "71",
  "Batman" = "72",
  "Şırnak" = "73",
  "Bartın" = "74",
  "Ardahan" = "75",
  "Iğdır" = "76",
  "Yalova" = "77",
  "Karabük" = "78",
  "Kilis" = "79",
  "Osmaniye" = "80",
  "Düzce" = "81"
)


# Bölge isimlerini alma
dropdown12_element <- remDr$findElement(using = 'id', value = 'dropdown12-contentDataDowloadNew')
dropdown12_element$clickElement()
Sys.sleep(3)

dropdown12_list_items <- remDr$findElements(using = "css", value = ".k-reset li")

bolge_list <- sapply(dropdown12_list_items, function(item) item$getElementText()[[1]])
bolge_list_dolu <- bolge_list[bolge_list != "" & bolge_list != "None"]

###CLICK ANOTHER ELEMENT
element <- remDr$findElement(using = 'xpath', value = '//*[@id="page-wrapper"]/div[1]')
element$clickElement()

# Her bölge için şehir ve istasyon isimlerini al
for (bolge in bolge_list_dolu) {

  clean_dropdown_element <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown1-contentDataDowloadNew"]/div/div/div/div/span[2]')
  clean_dropdown_element$clickElement()

  clean_dropdown_element <- remDr$findElement(using = 'xpath', value = ' //*[@id="dropdown12-contentDataDowloadNew"]/div[1]/div/div/div/span[2]')
  clean_dropdown_element$clickElement()

  dropdown12_element <- remDr$findElement(using = 'id', value = 'dropdown12-contentDataDowloadNew')
  dropdown12_element$clickElement()
  Sys.sleep(3)
  bolge_item <- remDr$findElement(using = 'xpath', value = paste0("//li[contains(text(), '", bolge, "')]"))
  bolge_item$clickElement()
  Sys.sleep(3)

  element <- remDr$findElement(using = 'xpath', value = '//*[@id="page-wrapper"]/div[1]')
  element$clickElement()

  clean_dropdown_element <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown1-contentDataDowloadNew"]/div/div/div/div/span[2]')
  clean_dropdown_element$clickElement()

  dropdown1_element <- remDr$findElement(using = 'id', value = 'dropdown1-contentDataDowloadNew')
  dropdown1_element$clickElement()
  Sys.sleep(3)

  dropdown1_list_items <- remDr$findElements(using = "css", value = ".k-reset li")
  sehir_list <- sapply(dropdown1_list_items, function(item) item$getElementText()[[1]])
  sehir_list_dolu <- sehir_list[sehir_list != ""]

  sehir_list_dolu <- sehir_list_dolu[-1]

  for (sehir in sehir_list_dolu) {

    clean_dropdown_element <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown1-contentDataDowloadNew"]/div/div/div/div/span[2]')
    clean_dropdown_element$clickElement()
    Sys.sleep(3)
    dropdown1_element <- remDr$findElement(using = 'id', value = 'dropdown1-contentDataDowloadNew')
    dropdown1_element$clickElement()
    Sys.sleep(3)
    sehir_item <- remDr$findElement(using = 'xpath', value = paste0("//li[contains(text(), '", sehir, "')]"))
    sehir_item$clickElement()
    Sys.sleep(3)

    element <- remDr$findElement(using = 'xpath', value = '//*[@id="page-wrapper"]/div[1]')
    element$clickElement()



    dropdown_element2 <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown2-contentDataDowloadNew"]')
    dropdown_element2$clickElement()
    Sys.sleep(3)

    dropdown_element_list2 <- remDr$findElements(using = 'css', value = '.k-reset li')
    station_list <- sapply(dropdown_element_list2, function(item) item$getElementText()[[1]])
    station_list_dolu <- station_list[station_list != ""]

    station_list_dolu <- station_list_dolu[-c(1, 2)]

    for (istasyon in station_list_dolu) {
      # Benzersiz ID oluştur
      id <- UUIDgenerate()

      plaka <- plaka_list[[sehir]]


      # Veriyi veri tabanına yaz
      dbExecute(mydb, "INSERT INTO location_2023 (Bolge, Sehir, Plaka, Istasyonlar, Id) VALUES (?, ?, ?, ?, ?)",
                params = list(bolge, sehir, plaka, istasyon, id))



    }

    # Bir sonraki şehir için sayfayı sıfırlamak gerekirse bu noktada işlem yapabilirsiniz
    element <- remDr$findElement(using = 'xpath', value = '//*[@id="page-wrapper"]/div[1]')
    element$clickElement()
    Sys.sleep(3)
  }

  # Bir sonraki bölge için sayfayı sıfırlamak gerekirse bu noktada işlem yapabilirsiniz
  element <- remDr$findElement(using = 'xpath', value = '//*[@id="page-wrapper"]/div[1]')
  element$clickElement()
  Sys.sleep(3)
}


remDr$close()
rD$server$stop()











































































































# Şehir isimlerini alma
dropdown1_element <- remDr$findElement(using = 'id', value = 'dropdown1-contentDataDowloadNew')
dropdown1_element$clickElement()
Sys.sleep(2)

dropdown1_list_items <- remDr$findElements(using = "css", value = ".k-reset li")
sehir_list <- sapply(dropdown1_list_items, function(item) item$getElementText()[[1]])
sehir_list_dolu <- sehir_list[sehir_list != ""]

element <- remDr$findElement(using = 'xpath', value = '//*[@id="page-wrapper"]/div[1]')
element$clickElement()


# İstasyon isimlerini alma
dropdown_element2 <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown2-contentDataDowloadNew"]')
dropdown_element2$clickElement()
Sys.sleep(2)
dropdown_element_list2 <- remDr$findElements(using = 'css', value = '.k-reset li')
station_list <- sapply(dropdown_element_list2, function(item) item$getElementText()[[1]])
station_list_dolu <- station_list[station_list != ""]



# Bölge isimlerini alma
dropdown12_element <- remDr$findElement(using = 'id', value = 'dropdown12-contentDataDowloadNew')
dropdown12_element$clickElement()
Sys.sleep(2)

dropdown12_list_items <- remDr$findElements(using = "css", value = ".k-reset li")

bolge_list <- sapply(dropdown12_list_items, function(item) item$getElementText()[[1]])
bolge_list_dolu <- bolge_list[bolge_list != ""]

-




query <- "SELECT * FROM location_deneme LIMIT 10"
result <- dbGetQuery(mydb, query)

# Sonucu görüntüleme
print(result)










dbDisconnect(mydb)




remDr$close()
rD$server$stop()














#BOLGE ISIMLERI

# dropdown12_element <- remDr$findElement(using = 'id', value = 'dropdown12-contentDataDowloadNew')
# dropdown12_element$clickElement()
# Sys.sleep(2)
#
# dropdown12_list_items <- remDr$findElements(using = "css", value = ".k-reset li")
#
# bolge_list <- sapply(dropdown12_list_items, function(item) item$getElementText()[[1]])
# bolge_list_dolu <- bolge_list[bolge_list != ""]



#SEHIR ISIMLERI

# dropdown1_element <- remDr$findElement(using = 'id', value = 'dropdown1-contentDataDowloadNew')
# dropdown1_element$clickElement()
#  Sys.sleep(2)
#
# dropdown1_list_items <- remDr$findElements(using = "css", value = ".k-reset li")
# sehir_list <- sapply(dropdown1_list_items , function(item) item$getElementText()[[1]])
# sehir_list_dolu <- sehir_list[sehir_list != ""]

#ISTASYON ISIMLERI

# dropdown_element2 <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown2-contentDataDowloadNew"]')
# dropdown_element2$clickElement()
# Sys.sleep(2)
#
# dropdown_element_list2 <- remDr$findElements(using = 'css', value = '.k-reset li')
# station_list <- sapply(dropdown_element_list2 , function(item) item$getElementText()[[1]])
# station_list_dolu <- station_list[station_list != ""]


