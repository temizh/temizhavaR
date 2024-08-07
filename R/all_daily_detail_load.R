#' Hourly detail load from database
#'
#' @param parameter The name of the parameter to return. All parameters are returned if not provided.
#' @export


all_daily_detail_load_from_database <- function(parm) {

  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")
  query <- "SELECT * FROM daily_detail"
  query_result <- dbGetQuery(mydb, query)
  dbDisconnect(mydb)

  # Tarih formatını standardize eder
  query_result$Tarih <- as.Date(query_result$Tarih)

  print("Orijinal veri boyutu:")
  print(dim(query_result))

  print("Orijinal sütun adları:")
  print(names(query_result))

  print("Veri özeti:")
  print(summary(query_result))

  print("NA değerler:")
  print(colSums(is.na(query_result)))

  if (!missing(parm)) {
    # parm değerindeki tırnak işaretlerini ve boşlukları kaldırır
    parm <- gsub("^\"|\"$", "", parm)
    parm <- trimws(parm)

    # Parametre seçimini daha sıkı hale getirir
    if (!(parm %in% names(query_result))) {
      stop(paste("Parametre '", parm, "' veri setinde bulunamadı. Mevcut sütunlar:", paste(names(query_result), collapse=", ")))
    }

    query_result <- query_result %>%
      select(Istasyon, Tarih, all_of(parm))
  }

  print("Seçilen sütunlar:")
  print(names(query_result))

  print("Seçilen veri özeti:")
  print(summary(query_result))

  print("Seçilen verideki NA değerler:")
  print(colSums(is.na(query_result)))

  return(query_result)
}
