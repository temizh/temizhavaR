library(temizhavaR)
library(dplyr)
library(dbplyr)
library(openxlsx)

con <- create_postgres_conn()

schemas <- c(
    "seasonal_2022_30062025",
    "seasonal_2023_30062025",
    "seasonal_2024_30062025"
)

years <- c("2022", "2023", "2024")

# Start with NULL and merge by Istasyon
data <- NULL

for (i in seq_along(schemas)) {
  schema <- schemas[i]
  year <- years[i]
  
  query <- paste0("SELECT * FROM ", schema, ".o3_11")
  temp_data <- dbGetQuery(con, query)
  
  # Make sure column names are: Istasyon, <year>
  colnames(temp_data) <- c("Istasyon", year)
  
  if (is.null(data)) {
    data <- temp_data
  } else {
    data <- full_join(data, temp_data, by = "Istasyon")
  }
}

data <- data %>%
  mutate(across(c("2022", "2023", "2024"), ~ as.numeric(.))) %>%
  mutate(mean = rowMeans(across(c("2022", "2023", "2024")), na.rm = TRUE)) %>%
  mutate(
    mean_strict = if_else(
      !is.na(`2022`) & !is.na(`2023`) & !is.na(`2024`),
      mean,
      NA
    )
  ) %>%
  arrange(Istasyon)

write.xlsx(data, "extdata/O3.xlsx", rowNames = FALSE)