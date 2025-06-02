library(temizhavaR)
library(dplyr)
library(dbplyr)
library(DBI)
library(openxlsx)

get_data <- function() {
  con <- create_postgres_conn()

  data <- tbl(con, "hourly_detail") %>%
    filter(Tarih >= "2024-01-01" & Tarih <= "2025-01-01") %>%
    rename(Istasyon = "Istasyon_modified") %>%
    filter(Istasyon == "Eskişehir-Vişnepark") %>%
    mutate(Yıl = year(Tarih)) %>%
    mutate(Ay = month(Tarih)) %>%
    mutate(Gün = day(Tarih)) %>%
    mutate(Saat = hour(Tarih)) %>%
    mutate(Dakika = minute(Tarih)) %>%
    mutate(Saat = ifelse(Dakika > 30, Saat + 1, Saat)) %>%
    select(Istasyon, Yıl, Ay, Gün, Tarih, Saat, O3)

  data
}

test_rolling_hours <- function() {

  data <- get_data()

  rolling_hours <- 8

  data <- data %>%
    distinct(Istasyon, Yıl, Ay, Gün, Tarih, .keep_all = TRUE) %>%
    mutate (
      avg_8hr = sql(paste0('AVG("O3") OVER (PARTITION BY "Istasyon" ORDER BY "Tarih" ROWS BETWEEN ', (rolling_hours - 1), ' PRECEDING AND CURRENT ROW)'))
    ) %>%
    group_by(Istasyon, Yıl, Ay, Gün) %>%
    mutate(max_8hr = max(avg_8hr, na.rm = TRUE)) %>%
    mutate(exceeds = ifelse(max_8hr > 120, TRUE, FALSE)) %>%
    ungroup() %>%
    select(Tarih, Yıl, O3, avg_8hr, max_8hr, exceeds)

  result <- data %>%
    filter(max_8hr > 120) %>%
    group_by(Yıl) %>%
    summarise(result = n()/24, .groups = "drop")

  # write to xlxs file
  write.xlsx(data, "extdata/test_rolling_hours.xlsx", rowNames = FALSE)
  write.xlsx(result, "extdata/test_rolling_hours_result.xlsx", rowNames = FALSE)
}

test_aot <- function() {

  data <- get_data()

  months <- c(4, 5, 6, 7, 8, 9) # April to September
  parameter <- "O3"
  days <- 183

  data <- data %>%
    filter(Saat > 8 & Saat <= 20) %>%
    filter(Ay %in% months) %>%
    group_by(Tarih, .add = TRUE) %>%
    mutate(
      more_than_80 = .data[[parameter]] > 80,
      difference = .data[[parameter]] - 80,
      AOT40 = sum(ifelse(more_than_80, difference, 0))
    ) %>%
    ungroup() %>%
    select(Tarih, Yıl, O3, more_than_80, difference, AOT40)

  result <- data %>%
    group_by(Yıl) %>%
    summarise(
      result = sum(AOT40, na.rm = TRUE) * days * 12 / n(),
      .groups = "drop_last"
    )
  
  # write to xlxs file
  write.xlsx(data, "extdata/test_aot.xlsx", rowNames = FALSE)
  write.xlsx(result, "extdata/test_aot_result.xlsx", rowNames = FALSE)
}

test_aot()
