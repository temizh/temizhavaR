#' Station Average for hourly data
#'
#' @param data station based air parameters data.
#' @export



hourly_station_average <- function(parameter_name, threshold = 90) {
  
  conn <- create_postgres_conn()

  query <- paste0("SELECT Istasyon_modified, AVG(", parameter_name, ") AS average FROM hourly_detail WHERE Istasyon_modified IN
                  (SELECT Istasyon_modified FROM (SELECT Istasyon_modified,
                  SUM(CASE WHEN ", parameter_name, " IS NOT NULL THEN 1 ELSE 0 END) AS non_null_count
                  FROM hourly_detail GROUP BY Istasyon_modified)
                  WHERE non_null_count >= 8761 * ", threshold / 100, ")
                  GROUP BY Istasyon_modified")

  query_result <- dbGetQuery(conn, query)

  disconnect_postgres(conn)

  return(query_result)
}
