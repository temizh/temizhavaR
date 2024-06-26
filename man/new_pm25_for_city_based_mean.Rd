% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_PM2.5_for_city_based_mean.R
\name{new_pm25_for_city_based_mean}
\alias{new_pm25_for_city_based_mean}
\title{Calculate PM2.5 Values from PM10 Yearly Averages for Stations with Less Than 75\% Data}
\usage{
new_pm25_for_city_based_mean()
}
\arguments{
\item{parameter_name}{The name of the parameter (e.g., "PM10") for which the PM2.5 values are calculated.}
}
\value{
A data frame containing the calculated PM2.5 values for stations with less than 75\% data availability.
}
\description{
This function calculates the PM2.5 values from the PM10 yearly averages for stations with less than 75\% data availability.
}
