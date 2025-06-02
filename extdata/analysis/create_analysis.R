source('extdata/api/logic.R')

create_analysis(
  start_date = "2013-01-01 00:00",
  end_date = "2025-01-01 00:00",
  schema_name = "o3_rolling",
  folder_id = "1FelmRwkwm8PfsLbej5A1nZ4sz_tXpL5h",
  daily = FALSE,
  hourly = TRUE,
  aqi_analysis = FALSE,
  save_to_drive = TRUE,
  parameters = c("O3"),
  stations = c()
)