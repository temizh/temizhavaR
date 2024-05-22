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

  # set location
  dropdown12_element <- remDr$findElement(using = 'id', value = 'dropdown12-contentDataDowloadNew')
  dropdown12_element$clickElement()
  Sys.sleep(3)
  bolge_item <- remDr$findElement(using = 'xpath', value = paste0("//li[contains(text(), '", bolge, "')]"))
  bolge_item$clickElement()
  Sys.sleep(3)

  element <- remDr$findElement(using = 'xpath', value = '//*[@id="page-wrapper"]/div[1]')
  element$clickElement()


  # set city
  dropdown1_element <- remDr$findElement(using = 'id', value = 'dropdown1-contentDataDowloadNew')
  dropdown1_element$clickElement()
  Sys.sleep(3)
  sehir_item <- remDr$findElement(using = 'xpath', value = paste0("//li[contains(text(), '", sehir, "')]"))
  sehir_item$clickElement()
  Sys.sleep(3)

  element <- remDr$findElement(using = 'xpath', value = '//*[@id="page-wrapper"]/div[1]')
  element$clickElement()


  # set station
  dropdown2_element <- remDr$findElement(using = 'id', value = 'dropdown2-contentDataDowloadNew')
  dropdown2_element$clickElement()
  Sys.sleep(3)
  istasyon_item <- remDr$findElement(using = 'xpath', value = paste0("//li[contains(text(), '", istasyon, "')]"))
  istasyon_item$clickElement()
  Sys.sleep(3)

  # set all parameter
  dropdown_element3 <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown3-contentDataDowloadNew"]/div/div/div/div/span[3]')
  dropdown_element3$clickElement()
  Sys.sleep(2)

  # choose hourly or daily
  if (data_type == "hourly") {
    hourly_element <- remDr$findElement(using = 'xpath', value = '//*[@id="dropdown4-contentDataDowloadNew"]/div/div/label[1]')
    hourly_element$clickElement()
    Sys.sleep(2)

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
    Sys.sleep(2)

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

  excel_button <- remDr$findElement(using = 'css selector', value = "a.k-button.k-button-icontext.k-grid-excel")
  excel_button$clickElement()
  Sys.sleep(60)


  downloaded_files <- list.files(result_dir, pattern = "\\.xlsx$", full.names = TRUE)
  current_file_count <- length(downloaded_files)

  if (current_file_count > previous_file_count) {
    message(paste("Istasyon:", istasyon, " icin indirme islemi basariyla tamamlandi."))
    previous_file_count <- current_file_count
  } else {
    message(paste("Istasyon:", istasyon, " icin indirme islemi tamamlanamadi veya dosya bulunamadi."))
  }


  clear_button_element <- remDr$findElement(using = 'xpath', value = '//*[@id="StationDataDownloadForm"]/fieldset[1]/div[1]/div[2]/div[2]/div/div/div/button ')
  clear_button_element$clickElement()
  Sys.sleep(5)

}
