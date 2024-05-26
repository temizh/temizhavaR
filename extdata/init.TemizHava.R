raw_dir <- "C:/TemizHava_raw_data2023/"
output_dir <- paste0(raw_dir, "__results/")
if (!exists(output_dir))
  dir.create(output_dir)
result_pm10_hourly_excel_file <- paste0(output_dir, "results_PM10_hourly.xlsx")
result_pm10_daily_excel_file <- paste0(output_dir, "results_PM10_daily.xlsx")
