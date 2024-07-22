
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

#İSTASYON GRUBU SEÇİNİZ

  dropdown12_element <- remDr$findElement(using = 'id', value = 'dropdown12-contentDataDowloadNew')

  dropdown12_element$clickElement()
  Sys.sleep(2)


  multiselect12_element <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown12-contentDataDowloadNew"]/div[1]/div/div/div')

  multiselect12_element$clickElement()

   Sys.sleep(2)


    dropdown12_list_items <- remDr$findElements(using = "css", value = ".k-reset li")

    for (item in dropdown12_list_items) {

     item_text <- item$getElementText()
      if ("Akdeniz THM" %in% item_text) {
        item$clickElement()
       break
      }
    }

   #ŞEHİR SEÇİNİZ


    # dropdown1_element <- remDr$findElement(using = 'id', value = 'dropdown1-contentDataDowloadNew')
    #
    # dropdown1_element$clickElement()
    # Sys.sleep(2)
    #
    # multiselect1_element <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown1-contentDataDowloadNew"]/div/div/div/div')
    #
    # multiselect1_element$clickElement()
    #
    # Sys.sleep(2)
    #
    #
    # dropdown1_list_items <- remDr$findElements(using = "css", value = ".k-reset li")
    #
    # for (item in dropdown1_list_items) {
    #   #
    #   item_text <- item$getElementText()
    #
    #
    #   if ("Adana" %in% item_text) {
    #     item$clickElement()
    #     break
    #   }
    # }

    #İSTASYON SEÇİNİZ

    dropdown_element2 <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown2-contentDataDowloadNew"]')
    dropdown_element2$clickElement()
    Sys.sleep(2)

   dropdown_element_list2 <- remDr$findElements(using = 'css', value = '.k-reset li')

   for (item in dropdown_element_list2) {
        #
        item_text <- item$getElementText()


        if ("Adana - Çatalan" %in% item_text) {
          item$clickElement()
          break
        }
   }

  #BÜTÜN PARAMETRELERİN SEÇİLMESİ

    dropdown_element3 <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown3-contentDataDowloadNew"]/div/div/div/div/span[3]')
    dropdown_element3$clickElement()
    Sys.sleep(2)

  #SAATLİK BUTONUNA TIKLA

    hourly_element <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown4-contentDataDowloadNew"]/div/div/label[1]')
    hourly_element$clickElement()
    Sys.sleep(2)

    # TAKVİM İKONUNA TIKLA
    calender_icon <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown5-contentDataDowloadNew"]/div/div/span/span/span/span[1]')
    calender_icon$clickElement()
    Sys.sleep(2)


#YILA GEÇMEK İÇİN
    calender_icon <- remDr$findElement(using = 'css', value = '.k-nav-fast')
    calender_icon$clickElement()
    Sys.sleep(2)



    months_icon <- remDr$findElement(using = 'css', value = '.k-state-selected')
    months_icon$clickElement()
    Sys.sleep(2)






    # monthss_icon <- remDr$findElement(using = 'css selector', value = 'value = '[title="1 Ocak 2023 Pazar"])
    # monthss_icon$clickElement()
    # Sys.sleep(2)

  Sys.sleep(2)
  inputtElement <- remDr$findElement(
    using = 'xpath',
    value = ' //*[@id="dropdown1-contentDataDowloadNew"]/div/div/div/div')


  # Elemente tıklayarak dropdown menüyü açma
  inputtElement$clickElement()
  Sys.sleep(2)


  aElement <- remDr$findElement(
    using = 'xpath',
    value = ' /html/body/div[2]/div/div[2]/form/fieldset[1]/div[1]/div[1]/div/div[2]/div/div/div/div/ul/li/span[1]')

  aElement$clickElement()
  Sys.sleep(2)

  # new_location <- function(){
  #
  #   #yeni lokasyon tablosunu olusturacak
  #
  # }
  # station_scraping <- function() {
  #   # yeni lokasyon tablosunda tutulanları cekicek
  #   for(
  #     (input : bolge, sehir, ,istasyon, startdate, enddate, result_dir)
  #
  #
  #
  #
  #
  #   ) all stations
  #
  # }
  #
  # #





