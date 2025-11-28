#####OPTIMIZATION###############################################################
set.seed(123)
n_days <- 5
demand_list <- lapply(seq_len(n_days), function(d) Sim_full_day(arrival_rates))


#' @description Takes a single day's simulated trip requests and a starting 
#'allocation of bikes across stations, then walks through trips in time order.
#'For each  request, the function checks whether a bike is available at the 
#'origin station; if so, the trip occurs, the bike is moved to the destination,
#'and the trip is marked as successful. Otherwise, the trip is marked as unhappy.
#' @param demand data frame of simulated demand for
#'  one day with columns time, origin, destination
#' @param placement named numeric vector; names are station IDs, 
#' values are starting bike counts
#' @return data frame equal to demand with an added column happy 
#' (1 if trip occurs, 0 otherwise)
Sim_trips_one_day <- function(demand_day, placement) {
  bikes <- placement
  demand_day$happy <- integer(nrow(demand_day))
  
  # loops through arrivals in time order, updating bike counts as trips occur
  for (i in 1:nrow(demand_day)) {
    o <- as.character(demand_day$origin[i])
    d <- as.character(demand_day$destination[i])
    
    if (bikes[o] > 0) {
      demand_day$happy[i] <- 1
      bikes[o] <- bikes[o] - 1
      bikes[d] <- bikes[d] + 1
    } else {
      demand_day$happy[i] <- 0
    }
  }
  demand_day
}

#' @description Evaluate a bike placement over multiple simulated demand days
#' @param demand_list list of data frames;
#'  each element is one day of simulated demand
#' @param placement named numeric vector of starting bike counts at each station
#' @return a numeric scalar which is average number of unhappy customers per day
evaluate_placement <- function(demand_list, placement) {
  total_unhappy <- 0
  # apply the trip simulation to each day and accumulate 
  # the number of failed trips
  for (d in 1:length(demand_list)) {
    trips_d <- Sim_trips_one_day(demand_list[[d]], placement)
    total_unhappy <- total_unhappy + sum(trips_d$happy == 0)
  }
  
  total_unhappy / length(demand_list)
}

#' @description Allocates a fixed fleet of bikes across stations using a greedy heuristic.
#' Bikes are added one at a time; at each step, the function temporarily places
#' the next bike at each station in turn, evaluates the resulting placement
#' using simulated demand, and permanently assigns the bike to the station
#' that yields the lowest average number of unhappy customers.
#' @param demand_list list of simulated demand days
#' (outputs of Sim_full_day)
#' @param estimates data frame of arrival rate estimates 
#' with start_station and end_station
#' @param total_bikes integer; total number of bikes available to place
#' @return list with elements placement (named integer vector of 
#' bikes per station)
optimize_bikes_greedy <- function(demand_list, estimates, total_bikes) {
  stations <- unique(c(estimates$start_station,
                       estimates$end_station))
  
  placement <- rep(0, length(stations))
  names(placement) <- stations
  avg_unhappy_hist <- numeric(total_bikes)
  # at every step tries placing the next bike at every station 
  # and picks the best option
  for (k in 1:total_bikes) {
    best_station <- stations[1]
    best_unhappy <- 10000000000
    # test adding one bike to each station in turn 
    # and evaluate how many customers stay unhappy
    for (s in stations) {
      candidate <- placement
      candidate[s] <- candidate[s] + 1
      
      u <- evaluate_placement(demand_list, candidate)
      if (u < best_unhappy) {
        best_unhappy <- u
        best_station <- s
      }
    }
    # the best station for this bike and record of the average unhappiness
    placement[best_station] <- placement[best_station] + 1
    avg_unhappy_hist[k] <- best_unhappy
  }
  
  list(
    placement = placement,
    avg_unhappy = avg_unhappy_hist
  )
}

# greedy placement for several fleet sizes
fleet_sizes <- c(50, 100, 150)
results <- lapply(fleet_sizes, function(K) {
  optimize_bikes_greedy(demand_list, arrival_rates, total_bikes = K)
})
names(results) <- paste0("K", fleet_sizes)
results