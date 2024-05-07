#' Station Average
#'
#' @param data station based air parameters data.
#' @export



  station_average <- function(data, parameter_name) {
    mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

    query <- paste0("SELECT Istasyon, AVG(", parameter_name, ") AS average FROM daily_detail GROUP BY Istasyon")

    query_result <- dbGetQuery(mydb, query)

    dbDisconnect(mydb)

    return(query_result)
  }
