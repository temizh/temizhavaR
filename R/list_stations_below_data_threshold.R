#' Returns the list of stations with yearly average below a specified threshold for a given parameter.
#'
#' @name list_stations_below_data_threshold
#' @param parameter_name The name of the parameter for stations.
#' @param threshold The threshold value for determining stations with yearly averages below it (default is 40).
#' @return A dataframe containing the list of stations with yearly averages below the specified threshold for the given parameter.
#' @export

library(DBI)

list_stations_below_data_threshold <- function(parameter_name, threshold = 40) {
  mydb <- create_postgres_conn()
  if (is.null(mydb)) {
    stop("Unable to connect to database")
  }

  query <- paste0("SELECT Istasyon_modified, AVG(", parameter_name, ") AS yearly_average
                  FROM daily_detail
                  WHERE Istasyon_modified IN
                    (SELECT Istasyon_modified
                    FROM
                      (SELECT Istasyon_modified,
                      SUM(CASE WHEN ", parameter_name, " IS NOT NULL THEN 1 ELSE 0 END) AS non_null_count
                      FROM daily_detail
                      GROUP BY Istasyon_modified)
                    WHERE non_null_count >= 365 * 0.9)
                  GROUP BY Istasyon_modified
                  HAVING AVG(", parameter_name, ") < ", threshold)

  query_result <- dbGetQuery(mydb, query)

  disconnect_postgres(mydb)

  return(query_result)
}
