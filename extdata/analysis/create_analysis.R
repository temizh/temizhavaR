source('extdata/api/logic.R')

create_analysis(
  start_date = "2014-01-01 00:00",
  end_date = "2025-01-01 00:00",
  schema_name = "seasonal_2024_30062025",
  folder_id = "1FWb8LmnpCbTZalT4EYwT0y_q7w8c9jB0",
  daily = FALSE,
  hourly = FALSE,
  aqi_analysis = TRUE,
  save_to_drive = FALSE,
  parameters = c("PM10", "PM25", "SO2", "NO2", "O3", "CO"),
  stations = c()
)