# This scipt manually forces UTC time zone to all data in hourly table
# Be careful! This will overwrite the existing data in the hourly_detail table.

library(temizhavaR)
library(lubridate)

force_utc_timezone <- function() {
  con <- create_postgres_conn()

  # Fetch all data from the hourly_detail table
  data <- tbl(con, "hourly_detail") %>%
    collect() %>%
    mutate(Tarih = force_tz(Tarih, tzone = "UTC"))

  # Update the hourly_detail table with the modified data
  dbWriteTable(con, "hourly_detail", data, overwrite = TRUE, row.names = FALSE)
  message("Timezone of Tarih column in hourly_detail table has been set to UTC successfully.")
  dbDisconnect(con)
}

# Run the function to force UTC timezone
force_utc_timezone()