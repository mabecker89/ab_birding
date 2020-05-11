#-------------------------------------------------------------------------------

# Title: 03_travel-costs
# Date: April 2020

# Description: Calculate travel costs, plus opportunity cost of time

#-------------------------------------------------------------------------------

# Load packages
library(readr)
library(dplyr)

#-------------------------------------------------------------------------------

# Import data

# Income data from 2016 general census
df_income <- read_csv("./data/base/ab-pc-income.csv")

# Driving distance and time from script 02
df_drive_dist_labs <- read_csv("./data/processed/ab-ebd-pc-hotspot-driving-dist-time_20-80km.csv") %>%
  bind_rows(read_csv("./data/processed/ab-ebd-pc-hotspot-driving-dist-time_5-20km.csv"),
            read_csv("./data/processed/ab-ebd-pc-hotspot-driving-dist-time_80-120km.csv"))

# Set parameters
yearly_hours <- 2040
opp_time <- 1/2
vehicle_cost <- 0.3 # Might want to put some more effort into this one.

#-------------------------------------------------------------------------------

# Calculate travel costs (opportunity cost of time + driving costs)

df_travel_costs <- df_drive_dist_labs %>%
  left_join(df_income, by = "postal_code") %>% # note: missing income info for 7 postal codes (we'll ignore).
  mutate(cost_time = 2 * opp_time * (med_net_15 / yearly_hours) * hours,
         cost_driving = 2 * vehicle_cost * km,
         cost_total = cost_time + cost_driving) %>%
  select(locality_id, postal_code, cost_time, cost_driving, cost_total)

write_csv(df_travel_costs, "./data/processed/ab-ebd-travel-costs.csv")







