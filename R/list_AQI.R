#' list AQI by year for every station
#'
#' @param parameter The parameter to be analyzed.
#' @param data_type The type of data to be analyzed. Options are "daily" and "hourly".
#' @param theeshold The concentration threshold for the annual average. Stations with an annual average
#' below this value will be returned. (e.g., 40 µg/m³)
#' @param until_year The year until which the data will be analyzed. Default is 2023.
#' @export

list_AQI <- function(parameter, data_type, threshold = 0, until_year = 2023) {

	AQI_values <- data.frame(
		index = c("iyi", "orta", "hassas", "sağlıksız", "kötü", "tehlikeli"),
		lower_limit = c(0, 51, 101, 151, 201, 301),
		SO2 = c(0, 101, 251, 501, 851, 1101),
		NO2 = c(0, 101, 201, 501, 1001, 2001),
		CO = c(0, 5501, 10001, 16001, 24001, 32001),
		PM25 = c(0, 10, 20, 25, 50, 75),
		O3 = c(0, 121, 161, 181, 241, 701),
		PM10 = c(0, 51, 101, 261, 401, 521)
	)

	hours_to_average <- data.frame(
		parameter = c("SO2", "NO2", "CO", "PM25", "O3", "PM10"),
		hours = c(1, 1, 8, 24, 8, 24)
	)

	# Function to get AQI and parameter limits
	get_aqi_limits <- function(value, parameter, return_type) {
		if (!(parameter %in% colnames(AQI_values))) {
			stop("Invalid parameter. Choose from: ", paste(names(AQI_values)[-c(1, 2)], collapse = ", "))
		}
		
		# Identify the AQI category the value falls into
		row <- which(AQI_values[[parameter]] <= value)
		if (length(row) == 0 || max(row) == nrow(AQI_values)) {
			stop("Value exceeds the defined range for ", parameter)
		}
		
		# Lower and upper rows for interpolation
		lower_row <- max(row)
		upper_row <- lower_row + 1
		
		# Extract limits
		aqi_lower <- AQI_values$lower_limit[lower_row]
		aqi_upper <- AQI_values$lower_limit[upper_row] - 1
		param_lower <- AQI_values[[parameter]][lower_row]
		param_upper <- AQI_values[[parameter]][upper_row] - 1

		if(return_type == "min") {
			return(param_lower)
		} else if(return_type == "max") {
			return(param_upper)
		} else if(return_type == "aqi_min") {
			return(aqi_lower)
		} else if(return_type == "aqi_max") {
			return(aqi_upper)
		} else {
			return(NA)
		}
	}

	process_data <- function(data, total_amount) {
		if (nrow(data) == 0) {
			warning("No data found for the given parameter")
			return(data.frame(Istasyon = character(0), Year = character(0)))
		}

		# Convert to date and extract year
		data <- data %>%
			mutate(Date = as.POSIXct(Tarih, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%  # Convert to date
			mutate(Year = format(Date, "%Y")) %>% # Extract year
			filter(Year <= until_year)  # Filter data until specified year

		#average_data <- rollmean(.data[[parameter]], k = hours_to_average$hours[hours_to_average$parameter == parameter], fill = NA, align = "right")

		data <- data %>%
			mutate(average = rollmean(get(parameter), k = hours_to_average$hours[hours_to_average$parameter == parameter], fill = NA, align = "right")) %>%
			# get lower value for parameter for corresponding value
			mutate(min = get_aqi_limits(get(parameter), parameter, "min")) %>%
			# get upper value for parameter for corresponding value
			mutate(max = get_aqi_limits(get(parameter), parameter, "max")) %>%
			# get lower AQI value for parameter for corresponding value
			mutate(AQI_min = get_aqi_limits(get(parameter), parameter, "aqi_min")) %>%
			# get upper AQI value for parameter for corresponding value
			mutate(AQI_max = get_aqi_limits(get(parameter), parameter, "aqi_max")) %>%
			# calculate AQI
			mutate(AQI = ((average - min) * (AQI_max - AQI_min) / (max - min)) + AQI_min)

		# remane Istasyon_modified to Istasyon
		data <- data %>%
			rename(Istasyon = Istasyon_modified)

		# Calculate average AQI per year
		data_summary <- data %>%
			group_by(Istasyon, Year) %>%
			summarise(
				average = mean(average, na.rm = TRUE),
				AQI_mean = mean(AQI, na.rm = TRUE)
			) %>%
			ungroup()

		# wide format
		wide <- data_summary %>%
			pivot_wider(names_from = Year, values_from = AQI_mean, values_fill = NA) %>%
			select(Istasyon, sort(names(.)[-1]))  # Order columns

		return(wide)
	}

  if (data_type == "daily") {
    conn <- create_postgres_conn()
    query <- sprintf('SELECT * FROM daily_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data, 365)
  } else if (data_type == "hourly") {
    conn <- create_postgres_conn()
    query <- sprintf('SELECT * FROM hourly_detail WHERE "%s" IS NOT NULL', parameter_name)
    data <- dbGetQuery(conn, query)
    disconnect_postgres(conn)
    result <- process_data(data, 365 * 24)
  }

  return(result)
}