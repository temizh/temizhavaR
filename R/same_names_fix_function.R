#' Download air quality data from the specified website
#'
#' @param bolge The region to select.
#' @param sehir The city to select.
#' @param istasyon The station to select..
#' @export



select_option <- function(dropdown_id, option_text) {
  dropdown_element <- remDr$findElement(using = 'id', value = dropdown_id)
  dropdown_element$clickElement()
  Sys.sleep(1)

  # Get all options
  options <- remDr$findElements(using = 'xpath', value = paste0('//*[@id="', dropdown_id, '"]//li'))

  for (option in options) {
    option_text_actual <- option$getElementText()[[1]]
    if (grepl(option_text, option_text_actual, ignore.case = TRUE)) {
      option$clickElement()
      Sys.sleep(1)
      break
    }
  }
}
