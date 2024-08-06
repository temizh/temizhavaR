#' Hourly detail load from database
#'
#' @param parameter The name of the parameter to return. All parameters are returned if not provided.
#' @export


all_daily_detail_load_from_database <- function(parm) {
  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")
  query <- "SELECT * FROM daily_detail"
  query_result <- dbGetQuery(mydb, query)
  query_result$Tarih <- as.POSIXct(query_result$Tarih, format = "%Y-%m-%d %H:%M:%S")
  dbDisconnect(mydb)

  # Sütun adlarını yazdırır
  print("Orijinal sütun adları:")
  print(names(query_result))

  if (!missing(parm)) {
    # parm değerindeki tırnak işaretlerini ve boşlukları kaldırır
    parm <- gsub("^\"|\"$", "", parm)
    parm <- trimws(parm)

    # Eğer parm sütun adları arasında yoksa, benzer sütun adını bulmaya çalışır
    if (!(parm %in% names(query_result))) {
      similar_cols <- names(query_result)[grep(parm, names(query_result), ignore.case = TRUE)]
      if (length(similar_cols) > 0) {
        parm <- similar_cols[1]
        warning(paste("Tam eşleşme bulunamadı. Benzer sütun kullanılıyor:", parm))
      } else {
        stop(paste("Parametre '", parm, "' veri setinde bulunamadı. Mevcut sütunlar:", paste(names(query_result), collapse=", ")))
      }
    }

    query_result <- query_result %>%
      select(Istasyon, Tarih, all_of(parm))
  }

  # Seçilen sütunları yazdır
  print("Seçilen sütunlar:")
  print(names(query_result))

  return(query_result)
}
