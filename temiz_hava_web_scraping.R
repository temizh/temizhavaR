
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
  
  #remDr$open()
  remDr$navigate("https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew")
  
  # Kendo UI Multiselect widget'ını temsil eden input elementini bulma
  inputElement <- remDr$findElement(
    using = 'css selector',
    value = 'input.k-input.k-readonly'
  )
  
  # Elemente tıklayarak dropdown menüyü açma
  inputElement$clickElement()
  Sys.sleep(2) # Sayfanın yüklenmesi için bekleyin
  