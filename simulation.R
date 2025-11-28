#####SIMULATION#################################################################

source("estimation")

## EXTRACT HOUR

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
  
  ## GENERATE CANDIDATE ARRIVAL TIMES from poisson lambda max on int. 0 to 24
  t <- 0 # current simulated time
  candidates <- c() # to store all generated event times
  
  while (t < 24) { # go until full day is simulated
    e <- rexp(1, rate = lambda_max) # inter-arrival time ~ Exp(lambda max)
    t <- t + e 
    if (t < 24) candidates <- c(candidates, t)} # save if within day
  
  ## THINNING - accept or reject an event based on lambda t
  accepted <- c() # store accepted event times
  for (ti in candidates) {
    h <- floor(ti) # hour of day (0-23)
    lambda_ti <- hourly_rates[h + 1] #non-homogenous pois process rate at specific hour
    p <- lambda_ti / lambda_max #acceptance probability
    if (runif(1) < p) accepted <- c(accepted, ti) } # accept event
  
  # if no events are accepted return empty df
  if (length(accepted) == 0) {
    return(data.frame(
      time = numeric(0),
      origin = character(0),
      destination = character(0))) }
  
  # return clean df with accepted simulated trips
  return(data.frame(
    time = accepted,
    origin = rep(origin, length(accepted)),
    destination = rep(destination, length(accepted)))) }


## SIMULATE FULL DAY

#' @Description Simulates a full day of trip arrivals across station pairs
#' @param estimates Arrival rate table: start, end, hour, mu hat
#' @return Data frame of simulated trips for full day

Sim_full_day <- function(estimates) {
  station_pairs <- unique(estimates[, c("start_station", "end_station")])
  all_trips <- lapply(1:nrow(station_pairs), function(i) {
    
    a <- station_pairs$start_station[i]
    b <- station_pairs$end_station[i]
    hourly_rates <- Extract_hour(estimates, a, b)
    Sim_station_pair(hourly_rates, a, b)})
  
  all_trips <- do.call(rbind, all_trips)
  all_trips <- all_trips[order(all_trips$time), ]
  return(all_trips)}

arrival_rates <- estimate_arrival_rates(bike_data)
simulated_day <- Sim_full_day(arrival_rates)
head(simulated_day)

simulated_day$clock <- format(as.POSIXct("2025-01-01 00:00:00") + simulated_day$time * 3600, "%H:%M")
head(simulated_day[, c("origin","destination","clock")])
