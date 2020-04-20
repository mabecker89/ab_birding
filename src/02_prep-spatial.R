#-------------------------------------------------------------------------------

# Title: 02_prep-spatial
# Date: April 2020

# Description: Spatial operations to prepare data for analyses

#-------------------------------------------------------------------------------

# Load packages
library(sf)
library(lwgeom)
library(tibble)
library(tidyr)

#-------------------------------------------------------------------------------

# Import data

sf_pc_sub <- read_csv("./data/processed/ab-ebdusers-pc-locations-distinct.csv") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

sf_hot_loc <- read_csv("./data/processed/ab-ebd-hotspot-locations.csv") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


#-------------------------------------------------------------------------------

# Calculate euclidean distances

pcodes <- sf_pc_sub$postal_code
hotspots <- sf_hot_loc$locality_id

dist <- sf_pc_sub %>%
  st_distance(sf_hot_loc) %>%
  as_tibble() %>%
  mutate_all(.funs = as.numeric) %>%
  mutate(postal_code = pcodes) %>%
  select(postal_code, everything()) %>%
  pivot_longer(cols = V1:last_col(), names_to = "locality_id", values_to = "distance") %>%
  mutate(locality_id = rep(hotspots, length(pcodes)),
         distance_km = distance / 1000) %>%
  select(postal_code, locality_id, distance_km)

dist_u80 <- dist %>%
  filter(distance_km <= 80,
         distance_km >= 20)


