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

# Save the current working directory
old_wd <- getwd()

# Dynamically change to the script's location
script_path <- dirname(normalizePath(sys.frame(1)$ofile))  # Works in RScript
setwd(script_path)

# Load functions (relative to the script's location)
source("create_daily_intermediate_analysis.R")
source("create_hourly_intermediate_analysis.R")
source("create_daily_analysis_views.R")
source("create_hourly_analysis_views.R")
source("create_AQI_analysis.R")
source("save_views_to_drive.R")

# Restore the previous working directory
setwd(old_wd)

# Configuration
start_year <- 2014
end_year <- 2024

timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

# schema_name <- paste0("analysis_", timestamp)
schema_name <- "aot_analysis_2"

# Google Drive
# Create a folder in Google Drive and get the folder ID
folder_id <- "1ODA3VAy5zMIkgRAwyidA2S0XJS3pJ7tV"

# Database
con <- create_postgres_conn()

# Ensure schema exists (create if not)
dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS ", DBI::dbQuoteIdentifier(con, schema_name)))

# Disconnect
dbDisconnect(con)

# Create intermediate analysis
# create_daily_intermediate_analysis(start_year, end_year, schema_name)
create_hourly_intermediate_analysis(start_year, end_year, schema_name)

# Create views
# create_daily_analysis_views(start_year, end_year, schema_name)
create_hourly_analysis_views(start_year, end_year, schema_name)

# Calculate AQI
if(FALSE) {
  create_AQI_analysis(start_year, end_year, schema_name)
}

# Save views to drive as XLSX
save_views_to_drive(schema_name, folder_id)