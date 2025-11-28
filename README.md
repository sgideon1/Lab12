## Bike Share Simulation & Optimization

**Author:** Dane Elliott & Sophie Gideon  
**Date:** 12/01/2025 
**Purpose:** To build and test a workflow for estimating bike share station 
demand, simulate daily trips, and optimizing bike allocations.

---
## Quick Start

1. Install required packages:  
```r
   install.packages(c("dplyr", "tidyr", "lubridate"))
2. Load the code

source("estimation.R")
source("simulation.R")
source("optimization.R")
source("util.R")

3. Prepare data
bike_data <- read.csv("sample_bike.csv")

4. Run the pipleine
arrival_rates <- estimate_arrival_rates(bike_data)
demand_list <- lapply(1:5, function(d) Sim_full_day(arrival_rates))
result <- optimize_bikes_greedy(demand_list, arrival_rates, total_bikes = 100)
```

---
## Repo Structure

Lab12/
├── estimation.R # code to estimate hourly arrival rates
├── simulation.R # code to simulate trips (single pair + full day)
├── optimization.R # code to allocate bikes using greedy heuristic
├── util.R # helper functions 
├── testing.R # basic tests to check correctness of all modules
├── sample_bike.csv # example dataset
└── README.md # this file

---
## Modules Overview

# Estimation

estimate_arrival_rates(data) converts raw trip logs into hourly rate estimates 
(mu_hat) between station pairs

# Simulation

Extract_hour(estimates, origin, dest) pulls out the hourly rate vector for a 
station pair.

Sim_station_pair(hourly_rates, origin, dest) uses thinning to simulate trip 
times for one pair.

Sim_full_day(estimates) runs through all station pairs and returns a combined
day of demand.

# Optimization

Sim_trips_one_day(demand_day, placement) marks each trip as successful or 
failed given starting bike counts.

evaluate_placement(demand_list, placement) computes average failed 
trips per day.

optimize_bikes_greedy(demand_list, estimates, total_bikes) uses a greedy 
method to allocate a fixed fleet of bikes across stations
to minimize failures

--- 
## Testing

If you run source("testing.R") and get no errors, the basic logic is working.

--- 
## Results

The results file consists of final plots and tables produced from the results 
of the final pipeline. These outputs visually demonstrate the
optimal bike placement and the number of unhappy customers by fleet size.

---
## Significance

Bike share systems face variable demand across stations and hours.
This project brings together demand estimation, simulation of realistic 
events, and optimizing limited resources. 

## License & Credits
This code is for academic use only.
