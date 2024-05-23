#' Download air quality data from the specified website
#'
#' @param bolge The region to select.
#' @param sehir The city to select.
#' @param istasyon The station to select.
#' @param data_type The type of data to download ("hourly" or "daily").
#' @param startdate The start date for the data download (format: "DD.MM.YYYY").
#' @param enddate The end date for the data download (format: "DD.MM.YYYY").
#' @param result_dir The directory where the downloaded data will be saved.
#' @return None. Downloads the data and saves it to the specified directory.
#' @export


download_data <- function(bolge, sehir, istasyon, data_type, startdate, enddate, result_dir) {

  city_dir <- file.path(result_dir, sehir)
  if (!dir.exists(city_dir)) {
    dir.create(city_dir, recursive = TRUE)
  }

  # set location
  dropdown12_element <- remDr$findElement(using = 'id', value = 'dropdown12-contentDataDowloadNew')
  dropdown12_element$clickElement()
  Sys.sleep(1)
  bolge_item <- remDr$findElement(using = 'xpath', value = paste0("//li[contains(text(), '", bolge, "')]"))
  bolge_item$clickElement()
  Sys.sleep(1)

  element <- remDr$findElement(using = 'xpath', value = '//*[@id="page-wrapper"]/div[1]')
  element$clickElement()


  # set city
  dropdown1_element <- remDr$findElement(using = 'id', value = 'dropdown1-contentDataDowloadNew')
  dropdown1_element$clickElement()
  Sys.sleep(1)
  sehir_item <- remDr$findElement(using = 'xpath', value = paste0("//li[contains(text(), '", sehir, "')]"))
  sehir_item$clickElement()
  Sys.sleep(1)

  element <- remDr$findElement(using = 'xpath', value = '//*[@id="page-wrapper"]/div[1]')
  element$clickElement()


  # set station
  dropdown2_element <- remDr$findElement(using = 'id', value = 'dropdown2-contentDataDowloadNew')
  dropdown2_element$clickElement()
  Sys.sleep(1)
  istasyon_item <- remDr$findElement(using = 'xpath', value = paste0("//li[contains(text(), '", istasyon, "')]"))
  istasyon_item$clickElement()
  Sys.sleep(1)

  # set all parameter
  dropdown_element3 <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown3-contentDataDowloadNew"]/div/div/div/div/span[3]')
  dropdown_element3$clickElement()
  Sys.sleep(1)

  # choose hourly or daily
  if (data_type == "hourly") {
    hourly_element <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown4-contentDataDowloadNew"]/div/div/label[1]')
    hourly_element$clickElement()
    Sys.sleep(1)

    #startdate <- "01.01.2023"

    # set date for hourly data
    start_date_element <- remDr$findElement(using = 'id', value = 'StationDataDownload_StartDateTime')
    start_date_element$clearElement()
    start_date_element$sendKeysToElement(list(paste0(startdate, " 00:00")))

    #enddate <- "01.01.2024"

    end_date_element <- remDr$findElement(using = 'id', value = 'StationDataDownload_EndDateTime')
    end_date_element$clearElement()
    end_date_element$sendKeysToElement(list(paste0(enddate, " 00:00")))
  } else {

    daily_element <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown4-contentDataDowloadNew"]/div/div/label[2]')
    daily_element$clickElement()
    Sys.sleep(1)

    start_date_element <- remDr$findElement(using = 'id', value = 'StationDataDownload_StartDateTime')
    start_date_element$clearElement()
    start_date_element$sendKeysToElement(list(startdate))

    end_date_element <- remDr$findElement(using = 'id', value = 'StationDataDownload_EndDateTime')
    end_date_element$clearElement()
    end_date_element$sendKeysToElement(list(enddate))
  }

  download_button_element <- remDr$findElement(using = 'xpath', value = '//*[@id="StationDataDownloadForm"]/fieldset[1]/div[1]/div[2]/div[1]/div/div/div/button')
  download_button_element$clickElement()
  Sys.sleep(5)

  #download detail data
  excel_button_detail <- remDr$findElement(using = 'css selector', value = "fieldset[data-element='DetailGrid'] a.k-button.k-button-icontext.k-grid-excel")
  excel_button_detail$clickElement()
  Sys.sleep(5)

  indirilen_dosyalar <- list.files(result_dir, pattern = "\\.xlsx$", full.names = TRUE)
  mevcut_dosya <- indirilen_dosyalar[length(indirilen_dosyalar)]
  if (!is.null(mevcut_dosya) && file.exists(mevcut_dosya)) {
    yeni_dosya_adi <- if (data_type == "hourly") {
      paste0(istasyon, "_saatlik_detay_2023.xlsx")
    } else {
      paste0(istasyon, "_gunluk_detay_2023.xlsx")
    }
    yeni_dosya_yolu <- file.path(city_dir, yeni_dosya_adi)
    file.rename(mevcut_dosya, yeni_dosya_yolu)
    message(paste("İstasyon:", istasyon, " için detay veri indirme işlemi başarıyla tamamlandı."))
  } else {
    message(paste("İstasyon:", istasyon, " için detay veri indirme işlemi tamamlanamadı veya dosya bulunamadı."))
  }

  #download summary data

  excel_button_summary <- remDr$findElement(using = 'css selector', value = "fieldset[data-element='SummaryGrid'] a.k-button.k-button-icontext.k-grid-excel")
  excel_button_summary$clickElement()
  Sys.sleep(5)

  indirilen_dosyalar <- list.files(result_dir, pattern = "\\.xlsx$", full.names = TRUE)
  mevcut_dosya <- indirilen_dosyalar[length(indirilen_dosyalar)]
  if (!is.null(mevcut_dosya) && file.exists(mevcut_dosya)) {
    yeni_dosya_adi <- if (data_type == "hourly") {
      paste0(istasyon, "_saatlik_ozet_2023.xlsx")
    } else {
      paste0(istasyon, "_gunluk_ozet_2023.xlsx")
    }
    yeni_dosya_yolu <- file.path(city_dir, yeni_dosya_adi)
    file.rename(mevcut_dosya, yeni_dosya_yolu)
    message(paste("İstasyon:", istasyon, " için özet veri indirme işlemi başarıyla tamamlandı."))
  } else {
    message(paste("İstasyon:", istasyon, " için özet veri indirme işlemi tamamlanamadı veya dosya bulunamadı."))
  }

  clear_button_element <- remDr$findElement(using = 'xpath', value = '//*[@id="StationDataDownloadForm"]/fieldset[1]/div[1]/div[2]/div[2]/div/div/div/button ')
  clear_button_element$clickElement()
  Sys.sleep(5)

}

