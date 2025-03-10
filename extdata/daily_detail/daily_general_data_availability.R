library(temizhavaR)
library(tidyverse)
library(DBI)
library(RPostgres)
library(utils)
library(openxlsx)
library(googledrive)
library(googlesheets4)

setup_google_auth <- function(email = NULL) {
  tryCatch({
    drive_auth(email = email, scopes = "https://www.googleapis.com/auth/drive")
    gs4_auth(token = drive_token())
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

export_to_google_sheets <- function(data, folder_id, view_name) {
  tryCatch({
    ss <- gs4_create(view_name)
    sheet_write(data, ss = ss, sheet = 1)
    drive_mv(file = as_id(ss), path = as_id(folder_id))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

generate_parameter_combinations <- function(parameter_list) {
  combinations <- list()
  for (i in 1:length(parameter_list)) {
    combs <- combn(parameter_list, i, simplify = FALSE)
    combinations <- c(combinations, combs)
  }
  return(combinations)
}

analyze_parameter_combination <- function(parameters, data_type = "daily", 
                                         threshold = 90, min_years = 0,
                                         season = NULL, until_year = 2023,
                                         conn = NULL) {
  local_connection <- FALSE
  if (is.null(conn)) {
    conn <- create_postgres_conn()
    local_connection <- TRUE
  }
  
  parameter_data <- list()
  
  for (param in parameters) {
    query <- sprintf('
    WITH filtered_data AS (
      SELECT 
        d."Istasyon_modified", 
        EXTRACT(YEAR FROM d."Tarih")::integer AS year,
        EXTRACT(MONTH FROM d."Tarih")::integer AS month,
        d."%s",
        l."Id" as location_id
      FROM %s_detail d
      LEFT JOIN location l ON d."Istasyon_modified" = l."Istasyon_modified"
      WHERE EXTRACT(YEAR FROM d."Tarih") <= %d
    ', param, data_type, until_year)
    
    if (!is.null(season)) {
      if (season == "summer") {
        query <- paste0(query, " AND EXTRACT(MONTH FROM d.\"Tarih\") IN (4, 5, 6, 7, 8, 9)")
      } else if (season == "winter") {
        query <- paste0(query, " AND EXTRACT(MONTH FROM d.\"Tarih\") IN (1, 2, 3, 10, 11, 12)")
      }
    }
    
    query <- paste0(query, ")")
    
    query <- paste0(query, sprintf('
    SELECT 
      filtered_data."Istasyon_modified",
      filtered_data.location_id,
      filtered_data.year::text as year,
      COUNT(*) AS total_entries,
      SUM(CASE WHEN filtered_data."%s" IS NOT NULL THEN 1 ELSE 0 END) AS available_entries,
      (SUM(CASE WHEN filtered_data."%s" IS NOT NULL THEN 1 ELSE 0 END)::float / COUNT(*)::float * 100) AS percentage
    FROM filtered_data
    GROUP BY filtered_data."Istasyon_modified", filtered_data.location_id, filtered_data.year
    ORDER BY filtered_data."Istasyon_modified", filtered_data.year
    ', param, param))
    
    param_data <- dbGetQuery(conn, query)
    
    if (nrow(param_data) > 0) {
      wide_data <- param_data %>%
        mutate(Value = ifelse(percentage >= threshold, floor(percentage), 0)) %>%
        select(Istasyon_modified, location_id, year, Value) %>%
        pivot_wider(names_from = year, values_from = Value, values_fill = 0)
      
      overall_stats <- param_data %>%
        group_by(Istasyon_modified) %>%
        summarise(
          `Genel Veri Mevcudiyeti (Yüzde)` = round(sum(available_entries) / sum(total_entries) * 100, 2),
          `Veri Eşiği Geçen Yıl Sayısı` = sum(percentage >= threshold)
        )
      
      parameter_data[[param]] <- wide_data %>%
        left_join(overall_stats, by = "Istasyon_modified")
    } else {
      parameter_data[[param]] <- data.frame()
    }
  }
  
  if (any(sapply(parameter_data, nrow) == 0)) {
    if (local_connection) disconnect_postgres(conn)
    return(data.frame())
  }
  
  all_stations <- unique(unlist(lapply(parameter_data, function(df) df$Istasyon_modified)))
  
  year_columns <- names(parameter_data[[1]])
  year_columns <- year_columns[!year_columns %in% c("Istasyon_modified", "location_id", 
                                                  "Genel Veri Mevcudiyeti (Yüzde)", 
                                                  "Veri Eşiği Geçen Yıl Sayısı")]
  
  result <- data.frame(Istasyon_modified = all_stations, stringsAsFactors = FALSE)
  result$location_id <- NA
  
  for (year in year_columns) {
    result[[year]] <- 0
    
    for (station in all_stations) {
      availabilities <- sapply(parameter_data, function(df) {
        station_row <- which(df$Istasyon_modified == station)
        if (length(station_row) == 0) {
          return(0)
        }
        
        year_val <- tryCatch({
          val <- df[station_row, year]
          if (is.list(val)) val <- val[[1]]
          if (is.null(val)) return(0)
          as.numeric(val)
        }, error = function(e) {
          return(0)
        })
        
        val <- ifelse(is.na(year_val), 0, year_val)
        return(val)
      })
      
      if (any(availabilities >= threshold)) {
        valid_avail <- availabilities[availabilities > 0]
        if (length(valid_avail) > 0) {
          mean_availability <- floor(mean(valid_avail))
          result[result$Istasyon_modified == station, year] <- mean_availability
        }
      }
    }
  }
  
  result$`Eşiği Geçen Yıl Sayısı` <- apply(result[, year_columns], 1, function(x) sum(x > 0))
  
  result <- result %>% 
    filter(`Eşiği Geçen Yıl Sayısı` >= min_years) %>%
    arrange(desc(`Eşiği Geçen Yıl Sayısı`))
  
  if (local_connection) disconnect_postgres(conn)
  
  return(result)
}

generate_view_name <- function(parameters, data_type, threshold) {
  param_str <- paste(parameters, collapse = "_")
  view_name <- paste0(data_type, "_", param_str, "_", threshold)
  view_name <- gsub("[^a-zA-Z0-9_]", "", view_name)
  if (nchar(view_name) > 63) {  
    view_name <- substr(view_name, 1, 63)
  }
  return(view_name)
}

export_to_excel <- function(data, output_dir = "output", view_name) {
  tryCatch({
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    file_name <- file.path(output_dir, paste0(view_name, ".xlsx"))
    
    openxlsx::write.xlsx(data, file_name)
    
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

create_parameter_view <- function(conn, view_name, data, export_excel = FALSE, 
                                export_google = FALSE, output_dir = "output",
                                google_folder_id = NULL) {
  if (export_google && !is.null(google_folder_id)) {
    return(export_to_google_sheets(data, google_folder_id, view_name))
  } else if (export_excel) {
    return(export_to_excel(data, output_dir, view_name))
  }
  
  tryCatch({
    data <- data %>%
      rename(overall = `Eşiği Geçen Yıl Sayısı`)
    
    view_name <- tolower(view_name)
    temp_table_name <- paste0("temp_", view_name)
    
    dbExecute(conn, sprintf("DROP VIEW IF EXISTS %s CASCADE;", view_name))
    
    dbExecute(conn, sprintf("DROP TABLE IF EXISTS %s CASCADE;", temp_table_name))
    
    dbWriteTable(conn, temp_table_name, data, row.names = FALSE, overwrite = TRUE)
    
    view_sql <- sprintf("CREATE OR REPLACE VIEW %s AS SELECT * FROM %s;", 
                       view_name, temp_table_name)
    dbExecute(conn, view_sql)
    
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

process_parameter_combinations <- function(parameter_list, data_type = "daily", 
                                        threshold = 90, min_years = 5,
                                        season = NULL, until_year = 2023,
                                        min_combination_size = 1, 
                                        max_combination_size = length(parameter_list),
                                        export_excel = FALSE,
                                        export_google = FALSE,
                                        output_dir = "output",
                                        google_folder_id = NULL) {
  if (export_google) {
    if (!setup_google_auth() || is.null(google_folder_id)) {
      stop("Google Drive authentication failed or no folder ID provided")
    }
  }
  
  combinations <- list()
  for (i in min_combination_size:min(max_combination_size, length(parameter_list))) {
    combs <- combn(parameter_list, i, simplify = FALSE)
    combinations <- c(combinations, combs)
  }
  
  conn <- if (!export_excel && !export_google) create_postgres_conn() else NULL
  if (!export_excel && !export_google && is.null(conn)) {
    stop("Failed to connect to database")
  }
  
  created_items <- 0
  for (params in combinations) {
    if (length(params) < min_combination_size || length(params) > max_combination_size) {
      next
    }
    
    view_name <- generate_view_name(params, data_type, threshold)
    
    result <- analyze_parameter_combination(
      parameters = params,
      data_type = data_type,
      threshold = threshold,
      min_years = min_years,
      season = season,
      until_year = until_year,
      conn = conn
    )
    
    if (nrow(result) == 0) {
      next
    }
    
    success <- create_parameter_view(conn, view_name, result, 
                                   export_excel = export_excel, 
                                   export_google = export_google,
                                   output_dir = output_dir,
                                   google_folder_id = google_folder_id)
    if (success) created_items <- created_items + 1
    
    Sys.sleep(0.5)
  }
  
  if (!export_excel && !export_google) disconnect_postgres(conn)
  
  return(created_items)
}

process_parameter_combinations(
  parameter_list = c("PM10", "PM25", "SO2", "CO", "NO2", "NOX", "NO", "O3"),
  data_type = "daily",
  threshold = 90,
  min_years = 0,
  until_year = 2023,
  min_combination_size = 2,
  max_combination_size = 8,
  export_google = TRUE,
  google_folder_id = "1pMfjB_2C6Z0BUCglduSXQZexmh9nQ1rH"
)

