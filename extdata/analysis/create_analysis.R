source('extdata/api/logic.R')

create_analysis(
  start_date = "2024-01-01 00:00",
  end_date = "2025-01-01 00:00",
  schema_name = "seasonal_2024_25062025",
  folder_id = "1LKBgjYX1IX3n4d14OaoCfxzDG3ZUdNDT",
  daily = TRUE,
  hourly = TRUE,
  aqi_analysis = FALSE,
  save_to_drive = TRUE,
  parameters = c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "O3"),
  stations = c()
)