source('extdata/api/logic.R')

create_analysis(
  start_date = "2013-01-01 00:00",
  end_date = "2025-01-01 00:00",
  schema_name = "daily02062025",
  folder_id = "1rmVHxw4tvoUx-s1nHnzH-dJ2KJ4gSald",
  daily = TRUE,
  hourly = FALSE,
  aqi_analysis = FALSE,
  save_to_drive = TRUE,
  parameters = c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "O3"),
  stations = c()
)