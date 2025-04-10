library(DBI)
library(dplyr)
library(dbplyr)
library(temizhavaR)

# Locations with corrected cities
fix <- data.frame(
  Istasyon_modified = c("Kırıkkale-BulvarPark"),
  city = c("Kırıkkale")
)

# Load the database
con <- create_postgres_conn()


# Loop over the fix and update each record
for (i in 1:nrow(fix)) {
  dbExecute(con, sprintf(
    "UPDATE location
     SET \"Sehir\" = '%s'
     WHERE \"Istasyon_modified\" = '%s'",
    as.character(fix$city[i]),
    as.character(fix$Istasyon_modified[i])
  ))
}