#-------------------------------------------------------------------------------

# Title: 04_modeling
# Date: April 2020

# Description: Model site choice

#-------------------------------------------------------------------------------

# Load packages
library(readr)
library(dplyr)

#-------------------------------------------------------------------------------

# Import data

# Person-trips
df_pt <- read_csv("./data/processed/ab-ebdusers-person-trips.csv")

# Travel costs
df_travel_costs <- read_csv("./data/processed/ab-ebd-travel-costs.csv")

# User postal codes
df_pc_sub <- read_csv("./data/processed/ab-ebdusers-pc-locations.csv")

#-------------------------------------------------------------------------------

# Fill in travel cost information for the trips dataframe

df_pt_tcost <- df_pt %>%
  left_join(df_pc_sub, by = "observer_id") %>%
  # Filter out trips where we don't have cost (i.e. postal code) info; might want these later though.
  filter(!is.na(postal_code)) %>%
  left_join(df_travel_costs, by = c("locality_id", "postal_code")) %>%
  filter(!is.na(cost_total)) %>%
  select(observer_id, observation_date, locality_id, locality, cost_total)

write_csv(df_pt_tcost, "./data/processed/ab-ebdusers-person-trips-travel-costs.csv")











