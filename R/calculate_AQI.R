#' calculate AQI
#'
#' @param data A grouped data frame containing the data (in tbl format)
#' @param parameter The parameter to be analyzed.
#' @export

calculate_AQI <- function(data, parameter) {

  AQI_values <- data.frame(
		index = c("iyi", "orta", "hassas", "sağlıksız", "kötü", "tehlikeli", "çok tehlikeli"),
		lower_limit = c(0, 50, 100, 150, 200, 300, 500),
		SO2 = c(0, 20, 40, 125, 190, 275, 2630),
		NO2 = c(0, 10, 25, 60, 100, 150, 3852),
		#CO = c(0, 5500, 10000, 16000, 24000, 32000, 57960),
		PM25 = c(0, 5, 15, 50, 90, 140, 500),
		O3 = c(0, 60, 100, 120, 160, 180, 1184),
		PM10 = c(0, 15, 45, 120, 195, 270, 604)
	)

	hours_to_average <- data.frame(
		parameter = c("SO2", "NO2", "CO", "PM25", "O3", "PM10"),
		hours = c(1, 1, 1, 1, 1, 1)
	)

  max_threshold <- AQI_values[[parameter]][7]

  data <- data %>%
    arrange(Istasyon, Tarih) %>%
    group_by(Istasyon) %>%
    mutate(average = sql(
      paste0('AVG("', parameter, '") OVER (PARTITION BY "Istasyon" ORDER BY "Tarih" ROWS BETWEEN ', (hours_to_average$hours[hours_to_average$parameter == parameter] - 1), ' PRECEDING AND CURRENT ROW)')
    )) %>% 
    # get lower value for parameter for corresponding value
    mutate(min = sql(
      paste0('CASE 
        WHEN "average" <= ', AQI_values[[parameter]][2], ' THEN ', AQI_values[[parameter]][1], '
        WHEN "average" <= ', AQI_values[[parameter]][3], ' THEN ', AQI_values[[parameter]][2], '
        WHEN "average" <= ', AQI_values[[parameter]][4], ' THEN ', AQI_values[[parameter]][3], '
        WHEN "average" <= ', AQI_values[[parameter]][5], ' THEN ', AQI_values[[parameter]][4], '
        WHEN "average" <= ', AQI_values[[parameter]][6], ' THEN ', AQI_values[[parameter]][5], '
        ELSE ', AQI_values[[parameter]][6], '
        END'))) %>%
    # get upper value for parameter for corresponding value
    mutate(max = sql(
      paste0('CASE 
        WHEN "average" <= ', AQI_values[[parameter]][2], ' THEN ', AQI_values[[parameter]][2], '
        WHEN "average" <= ', AQI_values[[parameter]][3], ' THEN ', AQI_values[[parameter]][3], '
        WHEN "average" <= ', AQI_values[[parameter]][4], ' THEN ', AQI_values[[parameter]][4], '
        WHEN "average" <= ', AQI_values[[parameter]][5], ' THEN ', AQI_values[[parameter]][5], '
        WHEN "average" <= ', AQI_values[[parameter]][6], ' THEN ', AQI_values[[parameter]][6], '
        ELSE ', AQI_values[[parameter]][7], '
        END'))) %>%
    # get lower AQI value for parameter for corresponding value
    mutate(AQI_min = sql(
      paste0('CASE 
        WHEN "average" <= ', AQI_values[[parameter]][2], ' THEN ', AQI_values$lower_limit[1], '
        WHEN "average" <= ', AQI_values[[parameter]][3], ' THEN ', AQI_values$lower_limit[2], '
        WHEN "average" <= ', AQI_values[[parameter]][4], ' THEN ', AQI_values$lower_limit[3], '
        WHEN "average" <= ', AQI_values[[parameter]][5], ' THEN ', AQI_values$lower_limit[4], '
        WHEN "average" <= ', AQI_values[[parameter]][6], ' THEN ', AQI_values$lower_limit[5], '
        ELSE ', AQI_values$lower_limit[6], '
        END'))) %>%
    # get upper AQI value for parameter for corresponding value
    mutate(AQI_max = sql(
      paste0('CASE 
        WHEN "average" <= ', AQI_values[[parameter]][2], ' THEN ', AQI_values$lower_limit[2], '
        WHEN "average" <= ', AQI_values[[parameter]][3], ' THEN ', AQI_values$lower_limit[3], '
        WHEN "average" <= ', AQI_values[[parameter]][4], ' THEN ', AQI_values$lower_limit[4], '
        WHEN "average" <= ', AQI_values[[parameter]][5], ' THEN ', AQI_values$lower_limit[5], '
        WHEN "average" <= ', AQI_values[[parameter]][6], ' THEN ', AQI_values$lower_limit[6], '
        ELSE ', AQI_values$lower_limit[7], '
        END'))) %>%
    # calculate AQI
    # mutate(AQI = ifelse((.data[[parameter]] > AQI_values[[parameter]][6]), 500, (((average - min) * (AQI_max - AQI_min) / (max - min)) + AQI_min))) %>%
    mutate(AQI = ifelse((average >= !!max_threshold), 500, (((average - min) * (AQI_max - AQI_min) / (max - min)) + AQI_min))) %>%
    ungroup()

  data <- data %>%
    select(Tarih, Istasyon, !!parameter, AQI, average, min, max, AQI_min, AQI_max)

  return(data)
}