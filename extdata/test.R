# Libraries
library(temizhavaR)
library(DBI)
library(tidyr)
library(pillar)
library(dbplyr)
library(dplyr)
library(slider)
library(googledrive)
library(googlesheets4)
library(lubridate)

con <- create_postgres_conn()

data <- tbl(con, "hourly_detail")

# print(head(data))

data <- data %>%
    filter(Istasyon_modified %in% c("Eskişehir-Vişnepark", "İzmir-Bornova", "İstanbul-Silivri-MTHM", "İstanbul-Ümraniye"))

data <- data %>%
    mutate(Yıl = year(Tarih)) %>%
    mutate(Saat = hour(Tarih)) %>%
    group_by(Istasyon_modified, Yıl) %>%
    filter(Saat > 8 & Saat < 20) %>%
    filter(is.na(.data[["O3"]]) == TRUE) %>%
    summarise(
        result = (n() / (11 * 365)) * 100,
        .groups = "drop_last"
    ) %>%
    select(
        Istasyon_modified,
        Yıl,
        result
    )

print(head(data))