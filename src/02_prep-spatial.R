#-------------------------------------------------------------------------------

# Title: 02_prep-spatial
# Date: April 2020

# Description: Spatial operations to prepare data for analyses

#-------------------------------------------------------------------------------

# Load packages
library(sf)
library(tibble)
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(keyring)

#-------------------------------------------------------------------------------

# Import data

# Clean postal codes from ebd users
df_pc_sub <- read_csv("./data/processed/ab-ebdusers-pc-locations.csv")

# All hotspot locations
df_hot_loc <- read_csv("./data/processed/ab-ebd-hotspot-locations.csv")

# Person trips (defined in 01)
df_pt <- read_csv("./data/processed/ab-ebdusers-person-trips.csv")

#-------------------------------------------------------------------------------

# Step 1. 

# Calculate euclidean distances between postal codes and hotspots

df_pt_pc <- df_pt %>%
  left_join(df_pc_sub, by = "observer_id") %>%
  filter(!is.na(postal_code))

sf_pc <- df_pt_pc %>%
  select(postal_code, longitude, latitude) %>%
  distinct() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

sf_hot <- df_hot_loc %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

pcodes <- sf_pc$postal_code
hotspots <- sf_hot$locality_id

# Calculate distance 'matrix' from every postal code to every hotspot.
df_euc_dist <- sf_pc %>%
  st_distance(sf_hot) %>%
  as_tibble() %>%
  mutate_all(.funs = as.numeric) %>%
  mutate(postal_code = pcodes) %>%
  select(postal_code, everything()) %>%
  pivot_longer(cols = V1:last_col(), names_to = "locality_id", values_to = "euc_distance") %>%
  mutate(locality_id = rep(hotspots, length(pcodes)),
         euc_distance_km = euc_distance / 1000) %>%
  select(postal_code, locality_id, euc_distance_km)

write_csv(df_euc_dist, "./data/processed/pc-hotspot-euclidean-distances.csv")

# Distances less than 80 and greater than 20 km - i.e., a typical day trip.
df_euc_dist_20_80 <- df_euc_dist %>%
  filter(euc_distance_km <= 80,
         euc_distance_km >= 20)

# Distance between 5 and 20 km
df_euc_dist_5_20 <- df_euc_dist %>%
  filter(euc_distance_km < 20, # just less than (<) because we captured == 20 above.
         euc_distance_km >= 5)

# Distance between 80 and 120 km
df_euc_dist_80_120 <- df_euc_dist %>%
  filter(euc_distance_km <= 120, 
         euc_distance_km > 80) # just greater than (>) because we captured == 80 above.

#-------------------------------------------------------------------------------

# Step 2. 

# Limit number of combinations by aggregating urban (EDM/CGY) postal codes to FSA level

df_fsa_centroid <- df_pt_pc %>%
  select(postal_code, longitude, latitude) %>%
  distinct() %>%
  # Edmonton and Calgary postal codes
  filter(str_detect(postal_code, "^T6|^T5|^T3|^T2")) %>%
  extract(postal_code, into = "fsa", regex = "(^.{3})", remove = FALSE) %>%
  select(fsa, longitude, latitude) %>%
  group_by(fsa) %>%
  nest() %>%
  mutate(longitude = map_dbl(.x = data, .f = ~ mean(.x$longitude)),
         latitude = map_dbl(.x = data, .f = ~ mean(.x$latitude))) %>%
  select(-data)

# All other postal codes
df_pc_other_loc <- df_pt_pc %>%
  select(postal_code, longitude, latitude) %>%
  distinct() %>%
  # Non-Edmonton and Calgary postal codes
  filter(!str_detect(postal_code, "^T6|^T5|^T3|^T2"))

# Prep combinations for {ggmap} mapdist() to calculate driving time/distance
df_prep_20_80 <- df_euc_dist_20_80 %>%
  extract(postal_code, into = "fsa", regex = "(^.{3})", remove = FALSE) %>%
  left_join(df_fsa_centroid, by = "fsa") %>%
  mutate(pc_loc = paste0(latitude, ",", longitude)) %>%
  select(-c(longitude, latitude)) %>%
  left_join(df_pc_other_loc, by = "postal_code") %>%
  mutate(pc_loc = ifelse(pc_loc == "NA,NA", paste0(latitude, ",", longitude), pc_loc)) %>%
  select(-c(longitude, latitude)) %>%
  left_join(df_hot_loc, by = "locality_id") %>%
  mutate(hot_loc = paste0(latitude, ",", longitude)) %>%
  select(pc_loc, hot_loc) %>%
  distinct()

# Prep combinations for {{gmap}} mapdist() -> 5-20km combos.
df_prep_5_20 <- df_euc_dist_5_20 %>%
  extract(postal_code, into = "fsa", regex = "(^.{3})", remove = FALSE) %>%
  left_join(df_fsa_centroid, by = "fsa") %>%
  mutate(pc_loc = paste0(latitude, ",", longitude)) %>%
  select(-c(longitude, latitude)) %>%
  left_join(df_pc_other_loc, by = "postal_code") %>%
  mutate(pc_loc = ifelse(pc_loc == "NA,NA", paste0(latitude, ",", longitude), pc_loc)) %>%
  select(-c(longitude, latitude)) %>%
  left_join(df_hot_loc, by = "locality_id") %>%
  mutate(hot_loc = paste0(latitude, ",", longitude)) %>%
  select(pc_loc, hot_loc) %>%
  distinct()

# Prep combinations for {{gmap}} mapdist() -> 5-20km combos.
df_prep_80_120 <- df_euc_dist_80_120 %>%
  extract(postal_code, into = "fsa", regex = "(^.{3})", remove = FALSE) %>%
  left_join(df_fsa_centroid, by = "fsa") %>%
  mutate(pc_loc = paste0(latitude, ",", longitude)) %>%
  select(-c(longitude, latitude)) %>%
  left_join(df_pc_other_loc, by = "postal_code") %>%
  mutate(pc_loc = ifelse(pc_loc == "NA,NA", paste0(latitude, ",", longitude), pc_loc)) %>%
  select(-c(longitude, latitude)) %>%
  left_join(df_hot_loc, by = "locality_id") %>%
  mutate(hot_loc = paste0(latitude, ",", longitude)) %>%
  select(pc_loc, hot_loc) %>%
  distinct()

#-------------------------------------------------------------------------------

# Step 3.

# Calculate driving time/distance with {ggmap}

# Register googple API key
register_google(key = key_get("apikey", keyring = "google"))

# Wrap mapdist in `safely` so that it doesn't fail when query comes back empty
safe_mapdist <- safely(mapdist)

df_prep <- df_prep_80_120

df_drive_dist <- df_prep %>%
  mutate(distance = map2(.x = pc_loc, .y = hot_loc, .f = ~ safe_mapdist(.x, .y, mode = "driving", override_limit = TRUE)),
         km = map(.x = distance, .f = ~ pluck(.x$result[["km"]])),
         km = round(as.numeric(ifelse(km == "NULL", NA, km)), digits = 2),
         hours = map(.x = distance, .f = ~ pluck(.x$result[["hours"]])),
         hours = round(as.numeric(ifelse(hours == "NULL", NA, hours)), digits = 2))

# Save missing combinations to run later (mid-June)
df_drive_dist %>%
  filter(is.na(km)) %>%
  select(pc_loc, hot_loc) %>%
  write_csv("./data/processed/missing-driving-dist-time_80-120.csv")

#-------------------------------------------------------------------------------

# Step 4.

# Join locality_id and postal code back in

pc_loc <- df_pt_pc %>%
  select(postal_code, longitude, latitude) %>%
  distinct() %>%
  extract(postal_code, into = "fsa", regex = "(^.{3})", remove = FALSE) %>%
  left_join(df_fsa_centroid, by = "fsa") %>%
  mutate(longitude = ifelse(is.na(longitude.y), longitude.x, longitude.y),
         latitude = ifelse(is.na(latitude.y), latitude.x, latitude.y),
         pc_loc = paste0(latitude, ",", longitude)) %>%
  select(postal_code, pc_loc)

hot_loc <- df_hot_loc %>%
  mutate(hot_loc = paste0(latitude, ",", longitude)) %>%
  select(locality_id, hot_loc)

# Join
df_drive_dist_labs <- df_drive_dist %>%
  left_join(pc_loc, by = "pc_loc") %>% # Expands # rows to include all pc combos w/in an FSA
  left_join(hot_loc, by = "hot_loc") %>%
  select(locality_id, hot_loc, postal_code, pc_loc, km, hours)
  
df_drive_dist_labs %>%
  write_csv("./data/processed/ab-ebd-pc-hotspot-driving-dist-time_80-120km.csv")

#-------------------------------------------------------------------------------










