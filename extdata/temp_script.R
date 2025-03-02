library(temizhavaR)

conn <- create_postgres_conn()

#list_stations_with_parameter("PM10", data_type = "hourly", threshold = 90)

query <- sprintf('SELECT * FROM hourly_detail WHERE "%s" IS NOT NULL', "PM10")
data <- dbGetQuery(conn, query)

data <- data %>%
    mutate(Date = as.Date(as.numeric(Tarih), origin = "1900-01-01")) %>%  # Convert to date
    mutate(Year = format(Date, "%Y"))  # Extract year

# Count total and available data per station per year
data_summary <- data %>%
    group_by(Istasyon, Year) %>%
    summarise(
    total_entries = 365 * 24,  # Total entries
    available_entries = sum(!is.na(.data[[parameter_name]])),  # Count non-NA values
    percentage = (available_entries / total_entries) * 100  # Calculate percentage
    ) %>%
    ungroup()

print(data_summary)

# Filter stations based on threshold
filtered_data <- data_summary %>%
    filter(percentage >= threshold) %>%  # Apply threshold
    select(Istasyon, Year) %>%
    mutate(Value = "x")  # Mark presence

# Convert to wide format
wide <- filtered_data %>%
    pivot_wider(names_from = Year, values_from = Value, values_fill = "-")