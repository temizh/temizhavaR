#' Hourly detail load from database
#'
#' @param parameter The name of the parameter to return. All parameters are returned if not provided.
#' @export
all_daily_detail_load_from_database <- function(parm, verbose = FALSE) {
  conn <- create_postgres_conn()
  if (is.null(conn)) {
    stop("Failed to connect to PostgreSQL database")
  }

  tryCatch({
    query <- "SELECT * FROM daily_detail"
    query_result <- dbGetQuery(conn, query)
    
    query_result$Tarih <- as.Date(query_result$Tarih)

    if (verbose) {
      print("Orijinal veri boyutu:")
      print(dim(query_result))

      print("Orijinal sütun adları:")
      print(names(query_result))

      print("Veri özeti:")
      print(summary(query_result))

      print("NA değerler:")
      print(colSums(is.na(query_result)))
    }

    if (!missing(parm)) {
      parm <- gsub("^\"|\"$", "", parm)
      parm <- trimws(parm)

      if (!(parm %in% names(query_result))) {
        stop(paste("Parametre '", parm, "' veri setinde bulunamadı. Mevcut sütunlar:", paste(names(query_result), collapse=", ")))
      }

      query_result <- query_result %>%
        select(Istasyon_original, Tarih, all_of(parm))
    }

    if (verbose) {
      print("Seçilen sütunlar:")
      print(names(query_result))

      print("Seçilen veri özeti:")
      print(summary(query_result))

      print("Seçilen verideki NA değerler:")
      print(colSums(is.na(query_result)))
    }

    return(query_result)
  }, error = function(e) {
    message("Error executing query: ", e$message)
    return(NULL)
  }, finally = {
    disconnect_postgres(conn)
  })
}
