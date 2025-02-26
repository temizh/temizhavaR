library(stringr)
library(openxlsx)

#' Download air quality data from the specified website
#'
#' @param bolge The region to select.
#' @param sehir The city to select.
#' @param istasyon The station to select.
#' @param data_type The type of data to download ("hourly" or "daily").
#' @param startdate The start date for the data download (format: "DD.MM.YYYY").
#' @param enddate The end date for the data download (format: "DD.MM.YYYY").
#' @param result_dir The directory where the downloaded data will be saved.
#' @param remDr The remote driver object for the Selenium web browser.
#' @return None. Downloads the data and saves it to the specified directory.
#' @export

download_data <- function(bolge, sehir, istasyon, data_type, startdate, enddate, result_dir, remDr) {
  retry <- function(f, retries = 3, sleep = 2, onError = NULL) {
    for (i in 1:retries) {
      tryCatch(
        {
          return(f())
        },
        error = function(e) {
          if (i == retries) {
            if (!is.null(onError)) onError(e)
            stop(e)
          }
          Sys.sleep(sleep)
        }
      )
    }
  }

  log_message <- function(message) {
    cat(paste0(Sys.time(), " - ", message, "\n"))
  }

  log_message("Starting download process...")

  print(result_dir)
  city_dir <- file.path(result_dir, sehir)
  if (!dir.exists(city_dir)) {
    dir.create(city_dir, recursive = TRUE)
    log_message(paste("Created directory:", city_dir))
  }

  click_element <- function(using, value) {
    retry(
      function() {
        remDr$findElement(using = using, value = value)$clickElement()
        log_message(paste("Clicked element with", using, "=", value))
        Sys.sleep(1)

      },
      onError = function(e) {
        log_message(paste("Failed to click element with", using, "=", value,
                          "; Station:", istasyon, "-", e$message))
      }
    )
  }

  set_input_value <- function(using, value, input_text) {
    retry(function() {
      element <- remDr$findElement(using = using, value = value)
      element$clearElement()
      element$sendKeysToElement(list(input_text))
      log_message(paste("Set input value for", using, "=", value, "to", input_text))
    })
  }

  missing_files <- list()

  tryCatch({
    print(paste("Bolge: ", bolge))
    print(paste("Sehir: ", sehir))
    print(paste("Istasyon: ", istasyon))
    # Set location
    click_element('id', 'dropdown12-contentDataDowloadNew')
    Sys.sleep(1)
    click_element('xpath', paste0("//li[contains(text(), '", bolge, "')]") )
    Sys.sleep(1)

    click_element('xpath', '//*[@id="page-wrapper"]/div[1]')

    # Set city
    click_element('id', 'dropdown1-contentDataDowloadNew')
    Sys.sleep(1)
    click_element('xpath', paste0("//li[contains(text(), '", sehir, "')]") )
    Sys.sleep(1)

    click_element('xpath', '//*[@id="page-wrapper"]/div[1]')

    # Set station
    Sys.sleep(2)
    
    click_element('css', "#dropdown2-contentDataDowloadNew .k-dropdown-wrap")

    Sys.sleep(1)

    click_element('xpath', 
             sprintf("//ul[@aria-hidden='false']/li[normalize-space(text())='%s']", istasyon))

    click_element('xpath', '//*[@id="page-wrapper"]/div[1]')
    Sys.sleep(1)
    
    # Select all parameters
    click_element('xpath', '//*[@id="dropdown3-contentDataDowloadNew"]/div/div/div/div/span[3]')
    Sys.sleep(1)

    # Choose hourly or daily
    if (data_type == "hourly") {
      click_element('xpath', '//*[@id="dropdown4-contentDataDowloadNew"]/div/div/label[1]')
      Sys.sleep(1)
      set_input_value('id', 'StationDataDownload_StartDateTime', paste0(startdate, " 00:00"))
      set_input_value('id', 'StationDataDownload_EndDateTime', paste0(enddate, " 00:00"))
    } else {
      click_element('xpath', '//*[@id="dropdown4-contentDataDowloadNew"]/div/div/label[2]')
      Sys.sleep(1)
      set_input_value('id', 'StationDataDownload_StartDateTime', startdate)
      set_input_value('id', 'StationDataDownload_EndDateTime', enddate)
    }

    startYear <- format(as.Date(startdate, format = "%d.%m.%Y"), "%Y")
    endYear <- format(as.Date(enddate, format = "%d.%m.%Y"), "%Y")

    # Download detail data
    click_element('xpath', '//*[@id="StationDataDownloadForm"]/fieldset[1]/div[1]/div[2]/div[1]/div/div/div/button')
    Sys.sleep(15) 
    
    click_element('css selector', "fieldset[data-element='DetailGrid'] a.k-button.k-button-icontext.k-grid-excel")
    Sys.sleep(6)

    indirilen_dosyalar <- list.files(result_dir, pattern = "\\.xlsx$", full.names = TRUE)
    if (length(indirilen_dosyalar) > 0) {
      mevcut_dosya <- indirilen_dosyalar[length(indirilen_dosyalar)]
      if (!is.null(mevcut_dosya) && !is.na(mevcut_dosya) && file.exists(mevcut_dosya)) {
        year <- format(as.Date(startdate, format = "%d.%m.%Y"), "%Y")
        modified_istasyon <- str_replace_all(istasyon, c(" " = "", "\\." = "", "/" = "_"))
        yeni_dosya_adi <- paste0(modified_istasyon, "_", if (data_type == "hourly") "saatlik" else "gunluk", "_detay_", startYear, "-", endYear, ".xlsx")
        yeni_dosya_yolu <- file.path(city_dir, yeni_dosya_adi)
        file.rename(mevcut_dosya, yeni_dosya_yolu)
        log_message(paste("Detail data successfully downloaded for station:", istasyon))
      } else {
        log_message(paste("Detail data download failed for station:", istasyon))
        missing_files <- c(missing_files, paste("Detail data for station:", istasyon))
      }
    } else {
      log_message(paste("No files found for detail data download for station:", istasyon))
      missing_files <- c(missing_files, paste("Detail data for station:", istasyon))
    }

    # Download summary data
    click_element('css selector', "fieldset[data-element='SummaryGrid'] a.k-button.k-button-icontext.k-grid-excel")
    Sys.sleep(10)  

    indirilen_dosyalar <- list.files(result_dir, pattern = "\\.xlsx$", full.names = TRUE)
    if (length(indirilen_dosyalar) > 0) {
      mevcut_dosya <- indirilen_dosyalar[length(indirilen_dosyalar)]
      if (!is.null(mevcut_dosya) && !is.na(mevcut_dosya) && file.exists(mevcut_dosya)) {
        year <- format(as.Date(startdate, format = "%d.%m.%Y"), "%Y")
        # Replace slashes with _, and remove spaces  and  dots from the station name
        modified_istasyon <- str_replace_all(istasyon, c(" " = "", "\\." = "", "/" = "_"))

        


        yeni_dosya_adi <- paste0(modified_istasyon , "_", if (data_type == "hourly") "saatlik" else "gunluk", "_ozet_", startYear, "-", endYear, ".xlsx")
        yeni_dosya_yolu <- file.path(city_dir, yeni_dosya_adi)
        file.rename(mevcut_dosya, yeni_dosya_yolu)
        log_message(paste("Summary data successfully downloaded for station:", istasyon))
      } else {
        log_message(paste("Summary data download failed for station:", istasyon))
        missing_files <- c(missing_files, paste("Summary data for station:", istasyon))
      }
    } else {
      log_message(paste("No files found for summary data download for station:", istasyon))
      missing_files <- c(missing_files, paste("Summary data for station:", istasyon))
    }

    click_element('xpath', '//*[@id="StationDataDownloadForm"]/fieldset[1]/div[1]/div[2]/div[2]/div/div/div/button ')
    Sys.sleep(5)

  }, error = function(e) {
    log_message(paste("Critical error occurred at station:", istasyon, "-", e$message))
    log_message("Closing Selenium driver and stopping the process.")
    remDr$close()
    stop(e)
  })

  log_message("Download process completed.")
  return(missing_files)
}

#' Find element with retry
#'
#' @param driver The remote driver object for the Selenium web browser.
#' @param using The method to use for finding the element (e.g., "id", "xpath").
#' @param value The value to use for finding the element.
#' @param max_retries The maximum number of retries to attempt.
#' @return The found element.
#' @export
#' @examples
#' findElementWithRetry(remDr, "id", "dropdown12-contentDataDowloadNew")
#' 
findElementWithRetry <- function(driver, using, value, max_retries = 5) {
  for (attempt in 1:max_retries) {
    tryCatch({
      Sys.sleep(1)
      element <- driver$findElement(using = using, value = value)
      return(element)
    }, error = function(e) {
      message(sprintf("Retry %d/%d failed for %s: %s", attempt, max_retries, value, e$message))
      Sys.sleep(2)
    })
  }
  stop(sprintf("Failed to find element after %d retries: %s", max_retries, value))
}


#' Safe click on element
#'
#' @param driver The remote driver object for the Selenium web browser.
#' @param element The element to click.
#' @return None.
#' @export
#' @examples
#' safeClick(remDr, element)

safeClick <- function(driver, element) {
  tryCatch({
    element$clickElement()
  }, error = function(e) {
    message(sprintf("Failed to click element: %s", e$message))
  })
}



#' Select dropdown option
#'  
#' @param driver The remote driver object for the Selenium web browser.
#' @param dropdown_xpath The XPath of the dropdown element.
#' @param option_text The text of the option to select.
#' @param max_retries The maximum number of retries to attempt.
#' @return TRUE if the option was successfully selected.
#' @export
#' @examples
#' selectDropdownOption(remDr, "dropdown1-contentDataDowloadNew", "Ankara")
#' 
selectDropdownOption <- function(driver, dropdown_xpath, option_text, max_retries = 5) {
  for (attempt in 1:max_retries) {
    tryCatch({
      dropdown <- findElementWithRetry(driver, 'xpath', dropdown_xpath)
      safeClick(driver, dropdown)
      Sys.sleep(1)

      option <- findElementWithRetry(driver, 'xpath', sprintf("//li[contains(text(), '%s')]", option_text))
      safeClick(driver, option)
      Sys.sleep(2)
      
      return(TRUE)
    }, error = function(e) {
      if (grepl("stale element reference", e$message)) {
        message(sprintf("Attempt %d/%d: Stale element detected, retrying...", attempt, max_retries))
      } else {
        message(sprintf("Attempt %d/%d failed: %s", attempt, max_retries, e$message))
      }
      Sys.sleep(2)  
    })
  }
  
  stop(sprintf("Failed to select '%s' in dropdown '%s' after %d retries", option_text, dropdown_xpath, max_retries))
}
