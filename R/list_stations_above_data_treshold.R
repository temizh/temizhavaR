#' Returns the list of stations with yearly average above a specified threshold for a given parameter.
#'
#' @param parameter_name The name of the parameter for stations.
#' @param threshold The threshold value for determining stations with yearly averages above it (default is 40).
#' @return A dataframe containing the list of stations with yearly averages above the specified threshold for the given parameter.
#' @export


list_stations_above_data_threshold <- function(parameter_name, threshold = 40) {
  mydb <- dbConnect(RSQLite::SQLite(), "temiz-hava.sqlite")

  query <- paste0("SELECT Istasyon, AVG(", parameter_name, ") AS yearly_average
                  FROM daily_detail
                  WHERE Istasyon IN
                    (SELECT Istasyon
                    FROM
                      (SELECT Istasyon,
                      SUM(CASE WHEN ", parameter_name, " IS NOT NULL THEN 1 ELSE 0 END) AS non_null_count
                      FROM daily_detail
                      GROUP BY Istasyon)
                    WHERE non_null_count >= 365 * 0.9)
                  GROUP BY Istasyon
                  HAVING AVG(", parameter_name, ") > ", threshold)

  query_result <- dbGetQuery(mydb, query)

  dbDisconnect(mydb)

  query_result %>%
    arrange(desc(yearly_average)) %>%
    mutate(yearly_average = round(yearly_average, 2))
}
