# Libraries
library(temizhavaR)
library(DBI)
library(tidyr)
library(pillar)
library(dbplyr)

# Save the current working directory
old_wd <- getwd()

# Dynamically change to the script's location
script_path <- dirname(normalizePath(sys.frame(1)$ofile))  # Works in RScript
setwd(script_path)

# Load functions (relative to the script's location)
source("create_daily_intermediate_analysis.R")
source("create_daily_analysis_views.R")

# Restore the previous working directory
setwd(old_wd)

# Configuration
start_year <- 2014
end_year <- 2024

timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")

schema_name <- paste0("analysis_", timestamp)

# Database
con <- create_postgres_conn()

# Ensure schema exists (create if not)
dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS ", DBI::dbQuoteIdentifier(con, schema_name)))

# Disconnect
dbDisconnect(con)

# Create intermediate analysis
create_daily_intermediate_analysis(start_year, end_year, schema_name)

# Create views
create_daily_analysis_views(start_year, end_year, schema_name)