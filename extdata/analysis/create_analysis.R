source('extdata/api/logic.R')

create_analysis(
  start_date = "2013-01-01 00:00",
  end_date = "2025-01-01 00:00",
  schema_name = "",
  folder_id = "1yHuToYM--n3trPwWFEc4ZKfeS_7PTlBG",
  daily = FALSE,
  hourly = TRUE,
  aqi_analysis = FALSE,
  save_to_drive = FALSE,
  parameters = c("O3"),
  stations = c()
)