#' calculate AQI
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to be analyzed.
#' @export

calculate_AQI <- function(data, parameter) {

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

  data <- data %>%
    mutate(average = sql(
      paste0('AVG("', parameter, '") OVER (ORDER BY "Tarih" ROWS BETWEEN ', (hours_to_average$hours[hours_to_average$parameter == parameter] - 1), ' PRECEDING AND CURRENT ROW)')
    )) %>% 
    # get lower value for parameter for corresponding value
    mutate(min = sql(
      paste0('CASE 
        WHEN "', parameter, '" < ', AQI_values[[parameter]][1], ' THEN ', AQI_values[[parameter]][1], '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][2], ' THEN ', AQI_values[[parameter]][2], '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][3], ' THEN ', AQI_values[[parameter]][3], '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][4], ' THEN ', AQI_values[[parameter]][4], '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][5], ' THEN ', AQI_values[[parameter]][5], '
        ELSE ', AQI_values[[parameter]][6], '
        END'))) %>%
    # get upper value for parameter for corresponding value
    mutate(max = sql(
      paste0('CASE 
        WHEN "', parameter, '" < ', AQI_values[[parameter]][1], ' THEN ', AQI_values[[parameter]][2], '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][2], ' THEN ', AQI_values[[parameter]][3], '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][3], ' THEN ', AQI_values[[parameter]][4], '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][4], ' THEN ', AQI_values[[parameter]][5], '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][5], ' THEN ', AQI_values[[parameter]][6], '
        ELSE ', AQI_values[[parameter]][6] + 1, '
        END'))) %>%
    # get lower AQI value for parameter for corresponding value
    mutate(AQI_min = sql(
      paste0('CASE 
        WHEN "', parameter, '" < ', AQI_values[[parameter]][1], ' THEN ', AQI_values$lower_limit[1], '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][2], ' THEN ', AQI_values$lower_limit[2], '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][3], ' THEN ', AQI_values$lower_limit[3], '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][4], ' THEN ', AQI_values$lower_limit[4], '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][5], ' THEN ', AQI_values$lower_limit[5], '
        ELSE ', AQI_values$lower_limit[6], '
        END'))) %>%
    # get upper AQI value for parameter for corresponding value
    mutate(AQI_max = sql(
      paste0('CASE 
        WHEN "', parameter, '" < ', AQI_values[[parameter]][1], ' THEN ', AQI_values$lower_limit[2] - 1, '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][2], ' THEN ', AQI_values$lower_limit[3] - 1, '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][3], ' THEN ', AQI_values$lower_limit[4] - 1, '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][4], ' THEN ', AQI_values$lower_limit[5] - 1, '
        WHEN "', parameter, '" < ', AQI_values[[parameter]][5], ' THEN ', AQI_values$lower_limit[6] - 1, '
        ELSE ', AQI_values$lower_limit[6] - 1, '
        END'))) %>%
    # calculate AQI
    mutate(AQI = ((average - min) * (AQI_max - AQI_min) / (max - min)) + AQI_min)

  data <- data %>%
    select(Tarih, Istasyon, AQI)

  return(data)
}