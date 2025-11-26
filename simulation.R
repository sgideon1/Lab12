#####SIMULATION#################################################################

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

Extract_hour(arrival_rates, 23, 21) # test
Extract_hour(arrival_rates, 12, 7) # test
Extract_hour(arrival_rates, 7, 11) # test


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

#Sim_station_pair(rep(0, 24), "A", "B") # test for sim_station_pair
#Sim_station_pair(rep(1, 24), "A", "B")  # test for sim_station_pair

rates_test <- c(0, 0, 10, 0, 0) # testing for lambda max (lambda max = 10 at hr 2)
Sim_station_pair(rates_test, "A", "B")
floor(Sim_station_pair(rates_test, "A", "B")$time) # outputs twos


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

full_day_test <- data.frame( # test full day simulation, only includes stations 1-3
  start_station = c(1,1,1,2,2,2),
  end_station = c(2,2,2,3,3,3),
  hour = c(0,1,2,0,1,2),
  mu_hat = c(5,0,5,2,2,2))

sim_test <- Sim_full_day(full_day_test)

# convert time to actual clock times (not seconds), POSIX format
sim_test$clock <- format(as.POSIXct("2025-01-01 00:00:00") + sim_test$time * 3600,"%H:%M")
head(sim_test[, c("origin","destination","clock")])



