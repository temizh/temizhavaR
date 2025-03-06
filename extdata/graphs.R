# This script is used to generate the graphs

# Load the necessary libraries
library(temizhavaR)
library(dygraphs)

# Load the data
conn <- create_postgres_conn()
data <- tbl(conn, "daily_detail") %>% collect() %>% as.data.frame()

# Parameters to be graphed
parameters <- c("PM10", "SO2", "NO2", "CO", "O3", "PM25", "NOX")

# Station to be graphed
station_name <- "Sinop"

# Create the graph
graph <- create_daily_time_series_graph(data, station_name, parameters)

# Close the connection
disconnect_postgres(conn)