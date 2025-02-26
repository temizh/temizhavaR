library(RSelenium)
library(netstat)
library(wdman)
library(uuid)
library(DBI)
library(stringr)


# Download Selenium Driver
wdman::selenium(port = 4445L, retcommand = TRUE)



# Retry mechanism for finding elements
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

# Wait for an element to appear
waitForElement <- function(driver, by, identifier, timeout = 10) {
  endTime <- Sys.time() + timeout
  while (Sys.time() < endTime) {
    tryCatch({
      element <- driver$findElement(using = by, value = identifier)
      if (!is.null(element)) return(element)
    }, error = function(e) { })
    Sys.sleep(0.5)
  }
  stop(sprintf("Element not found: %s", identifier))
}

# Safe click with retries
safeClick <- function(driver, element) {
  tryCatch({
    element$clickElement()
  }, error = function(e) {
    stop("Failed to click element: ", e$message)
  })
}


# Database setup
initializeDatabase <- function(db_path) {
  mydb <- dbConnect(RSQLite::SQLite(), db_path)
  dbExecute(mydb, "CREATE TABLE IF NOT EXISTS location (Bolge TEXT, Sehir TEXT, Plaka TEXT, Istasyonlar TEXT, Istasyonlar_modified TEXT, Id TEXT)")
  dbExecute(mydb, "DELETE FROM location") 
  return(mydb)
}

# Insert data into the database
insertLocation <- function(db, bolge, sehir, plaka, istasyon, istasyon_modified, id) {
  tryCatch({
    if (!dbIsValid(db)) stop("Database connection is invalid.")
    print(paste("Inserting into database:", bolge, sehir, plaka, istasyon, istasyon_modified, id))
    
    # Start transaction
    dbExecute(db, "BEGIN TRANSACTION")
    
    dbExecute(db, "INSERT INTO location (Bolge, Sehir, Plaka, Istasyonlar, Istasyonlar_modified, Id) VALUES (?, ?, ?, ?, ?, ?)",
              params = list(bolge, sehir, plaka, istasyon, istasyon_modified, id))
    
    # Commit transaction
    dbExecute(db, "COMMIT")
  }, error = function(e) {
    dbExecute(db, "ROLLBACK")
    message("Database insert error: ", e$message)
  })
}

# Clear the dropdown selection
clickClearButton <- function(driver, parent_div_id, button_title) {
  tryCatch({
    xpath <- sprintf("//div[@id='%s']//span[@title='%s']", parent_div_id, button_title)
    clear_button <- findElementWithRetry(driver, 'xpath', xpath)
    safeClick(driver, clear_button)
    Sys.sleep(1)  
    
    # Ensure dropdown is reset
    dropdown <- findElementWithRetry(driver, 'id', parent_div_id)
    safeClick(driver, dropdown)  
    Sys.sleep(1)  
  }, error = function(e) {
    message(sprintf("Failed to click the clear button in '%s': %s", parent_div_id, e$message))
  })
}

fetchRegionListWithRetry <- function(driver, dropdown_id, item_selector, max_retries = 5) {
  for (attempt in 1:max_retries) {
    tryCatch({
      # Locate the region dropdown
      dropdown <- findElementWithRetry(driver, 'id', dropdown_id)
      print("Found region dropdown")
      safeClick(driver, dropdown)
      print("Clicked region dropdown")
      Sys.sleep(2)
      
      # Fetch region items
      items <- driver$findElements(using = "css", value = item_selector)
      print("Found region items")
      region_list <- sapply(items, function(item) item$getElementText()[[1]])
      region_list <- region_list[region_list != ""]
      region_list <- region_list[!grepl("Bölge Seçiniz|OPEN", region_list)]
      region_list <- unique(region_list)  
      
      print(paste("Fetched region list with", length(region_list), "items"))
      
      # Ensure the list is not empty
      if (length(region_list) > 0) {
        return(region_list)
      } else {
        stop("Region list is empty")
      }
    }, error = function(e) {
      message(sprintf("Retry %d/%d failed for fetching region list: %s", attempt, max_retries, e$message))
      Sys.sleep(3) 
    })
  }
  
  stop(sprintf("Failed to fetch region list after %d retries", max_retries))
}

fetchStationListWithRetry <- function(driver, dropdown_id, item_selector, max_retries = 5) {
  for (attempt in 1:max_retries) {
    tryCatch({
      # Log the attempt
      message(sprintf("Fetching station list, attempt %d/%d", attempt, max_retries))
      
      # Locate the station dropdown
      dropdown <- findElementWithRetry(driver, 'css', dropdown_id)
      message("Found station dropdown")
      safeClick(driver, dropdown)
      message("Clicked station dropdown")
      
      # Fetch station items
      items <- driver$findElements(using = "css", value = item_selector)
      message("Found station items")
      
      # Extract and clean station names
      station_list <- sapply(items, function(item) item$getElementText()[[1]])
      station_list <- station_list[station_list != ""]
      station_list <- station_list[!grepl("İstasyon Seçiniz|OPEN", station_list)]
      station_list <- unique(station_list)
      message(sprintf("Cleaned station list with %d items: %s", 
                      length(station_list), paste(station_list, collapse = ", ")))
      
      # Ensure the list is not empty
      if (length(station_list) > 0) {
        return(station_list)
      } else {
        stop("Station list is empty")
      }
      
    }, error = function(e) {
      message(sprintf("Retry %d/%d failed: %s", attempt, max_retries, e$message))
      Sys.sleep(3)  
    })
  }
  
  stop("Failed to fetch station list after maximum retries")
}


selectDropdownOption <- function(driver, dropdown_id, option_text, max_retries = 5) {
  for (attempt in 1:max_retries) {
    tryCatch({
      dropdown <- findElementWithRetry(driver, 'id', dropdown_id)
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
  
  stop(sprintf("Failed to select '%s' in dropdown '%s' after %d retries", option_text, dropdown_id, max_retries))
}



########## MAIN SCRIPT ##########
retrieveStationInfo <- function() {
  tryCatch({
    selenium_server <- rsDriver(browser = "chrome", port = 4445L, chromever = "latest", extraCapabilities = list(
      chromeOptions = list(
        args = c('--headless', '--disable-gpu', '--window-size=1280,800', '--no-sandbox', '--disable-dev-shm-usage')
      )
    ))
    driver <- selenium_server$client
    driver$maxWindowSize()
    print("Remote driver opened successfully")

    # Navigate to the URL
    url <- "https://sim.csb.gov.tr/STN/STN_Report/StationDataDownloadNew"
    driver$navigate(url)
    print("Navigation successful")

    # Database setup
    DBDIR <- "./"
    mydb <- initializeDatabase(paste0(DBDIR, "temiz-hava.sqlite"))

    # Load plaka list
    source("extdata/plaka_list.R")
    all_cities <- names(plaka_list)
    processed_cities <- c()

    # Fetch region list
    bolge_list <- fetchRegionListWithRetry(driver, 'dropdown12-contentDataDowloadNew', ".k-reset li")

    for (bolge in bolge_list) {
      print(paste("Processing bolge:", bolge))
      selectDropdownOption(driver, 'dropdown12-contentDataDowloadNew', bolge)

      sehir_dropdown <- findElementWithRetry(driver, 'id', 'dropdown1-contentDataDowloadNew')
      safeClick(driver, sehir_dropdown)
      Sys.sleep(2)

      sehir_items <- driver$findElements(using = "css", value = ".k-reset li")
      sehir_list <- sapply(sehir_items, function(item) item$getElementText()[[1]])
      sehir_list <- sehir_list[sehir_list != ""]
      sehir_list <- sehir_list[!grepl("Şehir Seçiniz|OPEN", sehir_list)]  
      sehir_list <- unique(sehir_list) 
      print(paste("Cleaned city list with", length(sehir_list), "items:", paste(sehir_list, collapse = ", ")))

      for (sehir in sehir_list) {
        if (sehir %in% processed_cities) next
        print(paste("Processing sehir:", sehir))
        processed_cities <- c(processed_cities, sehir)

        selectDropdownOption(driver, 'dropdown1-contentDataDowloadNew', sehir)

        anotherArea <- findElementWithRetry(driver, 'xpath', '//*[@id="page-wrapper"]/div[1]')
        safeClick(driver, anotherArea)
        Sys.sleep(1)      
        dropdown_wrapper <- findElementWithRetry(driver, 'css', '.k-dropdown-wrap')
        safeClick(driver, dropdown_wrapper)
        option <- findElementWithRetry(driver, 'xpath', "//span[contains(text(), 'İstasyon Seçiniz')]")
        safeClick(driver, option) 
        print("Clicked on istasyon dropdown")
        Sys.sleep(2)

        istasyon_list <- fetchStationListWithRetry(driver, '#dropdown2-contentDataDowloadNew > div > div > span.k-widget.k-dropdown.k-header.form-control', ".k-reset li", max_retries = 5)
        print(paste("Fetched station list with", length(istasyon_list), "items"))

        station_data <- istasyon_list[istasyon_list != "İstasyon Seçiniz..."]
        print(paste("Fetched station list with", length(istasyon_list), "items"))
        print(paste("Stations in", sehir, ":", istasyon_list))

        for (istasyon in istasyon_list) {
          print(paste("Processing istasyon:", istasyon))
          id <- UUIDgenerate()
          plaka <- plaka_list[[sehir]]
          istasyon_modified  <- str_replace_all(istasyon, c(" " = "", "\\." = "", "/" = "_"))

          

          insertLocation(mydb, bolge, sehir, plaka, istasyon, istasyon_modified, id)
        }
        clickClearButton(driver, "dropdown1-contentDataDowloadNew", "Temizle")
      }
      clickClearButton(driver, "dropdown12-contentDataDowloadNew", "Temizle")
    }

    city_count <- dbGetQuery(mydb, "SELECT COUNT(DISTINCT Sehir) AS city_count FROM location")$city_count
    print(paste("Total unique cities in database:", city_count))

    dbDisconnect(mydb)
    driver$close()
    selenium_server$server$stop()
  }, error = function(e) {
    message("An error occurred: ", e$message)
    if (exists("driver")) driver$close()
    if (exists("selenium_server")) selenium_server$server$stop()
  })
}

# Call the function
retrieveStationInfo()