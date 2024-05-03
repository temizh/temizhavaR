#' Create Dygraphs
#'
#' @param query_result Dataframe containing data from database query.
#' @param station_name specified station name.
#' @export



create_station_graph <- function(query_result, x_column, ...) {

  dygraph_obj <- dygraph(query_result, x = x_column)

  for (series_name in list(...)) {
    dygraph_obj <- dygraph_obj %>%
      dySeries(series_name, label = series_name)
  }

  grafik <- dygraph_obj %>%
    dyOptions(colors = rainbow(length(list(...)))) %>%
    dyLegend(labelsSeparateLines = TRUE) %>%
    dyRangeSelector()

  return(grafik)
}
