#' Map Turkish characters to ASCII equivalents
#'
#' @param text The text containing Turkish characters to be normalized.
#' @return Normalized text with ASCII equivalents.
#' @export
normalize_turkish <- function(text) {
  # Replace Turkish characters with ASCII equivalents
  replacements <- list(
    "ş" = "s", "Ş" = "S",
    "ı" = "i", "İ" = "I",
    "ğ" = "g", "Ğ" = "G",
    "ü" = "u", "Ü" = "U",
    "ö" = "o", "Ö" = "O",
    "ç" = "c", "Ç" = "C"
  )
  
  for (char in names(replacements)) {
    text <- gsub(char, replacements[[char]], text)
  }
  
  return(text)
}

#' Map of problematic Turkish city names
#' 
#' Maps Turkish city names that cause encoding issues to their ASCII equivalents
#' @export
turkish_city_map <- list(
  "Kahramanmaraş" = "Kahramanmaras",
  "Şanlıurfa" = "Sanliurfa",
  "Afyonkarahisar" = "Afyonkarahisar",
  "Çorum" = "Corum",
  "Kırşehir" = "Kirsehir",
  "Kırıkkale" = "Kirikkale",
  "Düzce" = "Duzce"
)

#' Get list of cities that need special handling
#' 
#' @return Character vector of city names that need special handling
#' @export
get_problematic_cities <- function() {
  return(names(turkish_city_map))
}
