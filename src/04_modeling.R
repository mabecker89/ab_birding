#-------------------------------------------------------------------------------

# Title: 04_modeling
# Date: April 2020

# Description: Model site choice

#-------------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)

#-------------------------------------------------------------------------------

# Import data

# Person-trips
df_pt <- read_csv("./data/processed/ab-ebdusers-person-trips.csv")

# Travel costs
df_travel_costs <- read_csv("./data/processed/ab-ebd-travel-costs.csv")

# User postal codes
df_pc_sub <- read_csv("./data/processed/ab-ebdusers-pc-locations.csv")

# Euclidean distances
df_euc_dist <- read_csv("./data/processed/pc-hotspot-euclidean-distances.csv")

# All hotspot locations
df_hot_loc <- read_csv("./data/processed/ab-ebd-hotspot-locations.csv")

#-------------------------------------------------------------------------------
travel_cutoff_lo = 5
travel_cutoff_hi = 120

# Retrieve relevant person-trips (have postal codes, between 5 - 120 km)

df_pt_rel <- df_pt %>%
  left_join(df_pc_sub, by = "observer_id") %>%
  # Filter out trips where we don't have postal code info; might want these later though.
  filter(!is.na(postal_code)) %>%
  left_join(df_euc_dist, by = c("postal_code", "locality_id")) %>%
  filter(euc_distance_km <= travel_cutoff_hi & euc_distance_km >= travel_cutoff_lo) %>%
  select(observer_id, observation_date, locality_id, locality) # missing Kanaskis travel costs. 

# Calculate trip counts by month-year 

df_trip_counts <- df_pt_rel %>%
  mutate(month = as.character(month(observation_date, label = TRUE)),
         year = year(observation_date)) %>%
  group_by(observer_id, locality_id, month, year) %>%
  tally()

#-------------------------------------------------------------------------------

# Construct large (!) choice set matrix for modeling

# Vector of hotspots - 1,227 total.
locality_id <- df_hot_loc %>% pull(locality_id)

# Vector of unique users - 404 total.
observer_id <- df_pt_rel %>% select(observer_id) %>% distinct() %>% pull()

# Earliest trips
earliest <- df_pt_rel %>%
  group_by(observer_id) %>%
  summarise(earliest_trip = floor_date(min(observation_date), unit = "month")) 

# Vector of years
year <- seq(2009, 2020, 1)
# Vector of months
month <- month.abb

# Travel costs
df_costs <- df_travel_costs %>% select(locality_id, postal_code, cost_total)

# Construct matrix
df_modeling <- crossing(observer_id, locality_id) %>%
  left_join(df_pc_sub, by = "observer_id") %>%
  select(-c(latitude, longitude)) %>%
  left_join(df_euc_dist, by = c("postal_code", "locality_id")) %>%
  # Only keep combos we're interested (i.e. 1-200km)
  filter(euc_distance_km <= travel_cutoff_hi & euc_distance_km >= travel_cutoff_lo) %>%
  crossing(month, year) %>%
  # Truncate at Jan 2020
  filter(!(year == "2020" & !month == "Jan")) %>%
  left_join(df_costs, by = c("postal_code", "locality_id")) %>%
  select(observer_id, locality_id, month, year, cost_total) %>%
  left_join(df_trip_counts, by = c("observer_id", "locality_id", "month", "year")) %>%
  mutate(n_trips = ifelse(is.na(n), 0, n)) %>%
  select(-n) %>%
  mutate(month = factor(month, levels = month.abb),
         year = factor(year)) %>%
  arrange(observer_id, locality_id, month, year) %>%
  # Truncate choices by when each observer's earliest trip was
  mutate(choice_date = as.Date(paste0("01", " ", month, " ", year), format = "%d %b %Y")) %>%
  left_join(earliest, by = "observer_id") %>%
  mutate(relevant_choice = ifelse(earliest_trip <= choice_date, 1, 0)) %>%
  filter(relevant_choice == "1") %>%
  select(observer_id:n_trips)

# Investigate people with missing travel cost data
df_test = df_modeling %>%
  filter(is.na(cost_total)) %>%
  distinct(observer_id) %>%
  left_join(df_pc_sub, by = "observer_id")

hist(df_modeling$cost_total)

df_test = df_modeling %>%
group_by(n_trips) %>%
  summarise(count = n()) 










