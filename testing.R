## MAIN TESTS

### Estimation

## estimate_arrival_rates test
estimation_sim <- data.frame(
  start_time = c("2024-01-01 07:00:00", "2024-01-02 10:00:00"),
  end_time = c("2024-01-01 07:30:00", "2024-01-02 10:30:00"),
  start_station = c("C", "A"),
  end_station = c("A", "C")) 

estimate_arrival_rates(estimation_sim)

### Simulation

## Sim_full_day
full_day_test <- data.frame( # test full day simulation, only includes stations 1-3
  start_station = c(1,1,1,2,2,2),
  end_station = c(2,2,2,3,3,3),
  hour = c(0,1,2,0,1,2),
  mu_hat = c(5,0,5,2,2,2))

sim_test <- Sim_full_day(full_day_test)
sim_test$clock <- format(as.POSIXct("2025-01-01 00:00:00") + sim_test$time * 3600,"%H:%M")
head(sim_test[, c("origin","destination","clock")])

## UTILITY/HELPER TESTS

### Simulation

## Extract_hour
Extract_hour(arrival_rates, 23, 21) # test
Extract_hour(arrival_rates, 12, 7) # test
Extract_hour(arrival_rates, 7, 11) # test

### Sim_station_pairs
Sim_station_pair(rep(0, 24), "A", "B") # test for sim_station_pair
Sim_station_pair(rep(1, 24), "A", "B")  # test for sim_station_pair
# (within sim_station pairs) Testing lambda max
rates_test <- c(0, 0, 10, 0, 0) # testing for lambda max (lambda max = 10 at hr 2)
Sim_station_pair(rates_test, "A", "B")
floor(Sim_station_pair(rates_test, "A", "B")$time) # outputs twos

### Optimization
test_demand <- data.frame(
  time = c(0, 1, 2),
  origin = c("A", "A", "A"),
  destination = c("B", "B", "B")
)
# case 1: only 1 bike at A -> first trip happy, rest unhappy
placement_1 <- c(A = 1, B = 0)
trips_1 <- Sim_trips_one_day(test_demand, placement_1)

stopifnot(
  all(trips_1$happy %in% c(0, 1)),
  trips_1$happy[1] == 1,
  trips_1$happy[2] == 0,
  trips_1$happy[3] == 0
)

# case 2: 3 bikes at A, every trip should be happy
placement_2 <- c(A = 3, B = 0)
trips_2 <- Sim_trips_one_day(test_demand, placement_2)

stopifnot(
  all(trips_2$happy == 1),
  sum(trips_2$happy) == nrow(test_demand)
)

## evalute_placement
# Builds a list of 2 “days,” each identical to test_demand.
test_demand_list <- list(test_demand, test_demand) 

# with 1 bike at A, each day -> 2 unhappy trips, avg should be 2
eval_1 <- evaluate_placement(test_demand_list, placement_1)
stopifnot(eval_1 == 2)

# with 3 bikes at A, no one unhappy, avg should be 0
eval_2 <- evaluate_placement(test_demand_list, placement_2)
stopifnot(eval_2 == 0)

## optimize bikes greedy
est_small <- data.frame(
  start_station = c("A", "A"),
  end_station = c("B", "B"),
  hour = c(0, 1),
  mu_hat = c(5, 5)
)

set.seed(123)
res_small <- optimize_bikes_greedy(
  demand_list = test_demand_list,
  estimates = est_small,
  total_bikes = 3
)

# checking the structure
stopifnot(
  # all 3 bikes are allocated somewhere
  sum(res_small$placement) == 3,
  # names in placement correspond to the stations in est_small
  all(names(res_small$placement) %in% c("A","B")),
  # one average-unhappiness value for each step of adding a bike (k = 1, 2, 3).
  length(res_small$avg_unhappy) == 3,
  # average unhappiness never increases as bikes are added
  all(diff(res_small$avg_unhappy) <= 0)
)

# with this test demand, more bikes should go to A than B
stopifnot(res_small$placement["A"] >= res_small$placement["B"])


