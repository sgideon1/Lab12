#' @Description Extract hourly mu value for station pairs
#' @param estimates data frame: start end hour mu_hat
#' @param origin origin station
#' @param destination destination station
#' @return numeric vector with hourly rates

Extract_hour <- function(estimates, origin, destination) {
  rates <- estimates %>%
    filter(start_station == origin,
           end_station == destination) %>%
    arrange(hour) %>%
    pull(mu_hat) 
  
  return(rates) }

## SIMULATE ONE PAIR

#' @Description Simulates all trips for one station pair
#' @param origin origin station
#' @param destination destination station
#' @return Dataframe: time(hour), origin, destination 

Sim_station_pair <- function(hourly_rates, origin, destination) {
  
  # if no hourly rates exist for a pair, return 0
  if (length(hourly_rates) == 0) { 
    return(data.frame(
      time = numeric(0),
      origin = character(0),
      destination = character(0))) }
  
  # if fewer than 24hrs, fill missing hours with 0
  if (length(hourly_rates) < 24) {
    hourly_rates <- c(hourly_rates, rep(0, 24 - length(hourly_rates))) }
  
  # compute lambda max (maximum hourly rates across 24 hrs)
  lambda_max <- max(hourly_rates) 
  if (lambda_max == 0) {
    return(data.frame(
      time = numeric(0),
      origin = character(0),
      destination = character(0))) }