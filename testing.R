# MAIN TEST

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

### Optimization



# UTILITY/HELPER TESTS

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
