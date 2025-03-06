# This script is used to generate the graphs

# Load the necessary libraries
library(temizhavaR)
library(dygraphs)
library(ggplot2)

# Parameters to be graphed
parameters <- c("PM10", "SO2")

# Station to be graphed
station_name <- "Sinop"

# File name for the graph
file_path <- options()$temizhavaR.base_dir

# Generate timestamp for file name
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

start_date <- as.Date("2014-01-01")
end_date <- as.Date("2020-01-31")

# frequency <- "hourly"
frequency <- "daily"

# is_panel <- FALSE
is_panel <- TRUE

# Create the file path
file_full_path <- paste0(file_path, "/graph/", station_name, "_", timestamp, ".png")

print(file_full_path)

# Get the data
conn <- create_postgres_conn()
data <- tbl(conn, paste0(frequency, "_detail")) %>%
  filter(Istasyon == station_name) %>%
  filter(Tarih >= start_date & Tarih <= end_date) %>%
  collect() %>%
  as.data.frame()

# Create the graph
create_daily_time_series_graph(data, file_full_path, parameters, is_panel)

# Close the connection
disconnect_postgres(conn)