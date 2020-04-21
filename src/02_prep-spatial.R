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
library(readr)
library(dplyr)
library(stringr)
library(purrr)

#-------------------------------------------------------------------------------

# Import data

df_pc_sub <- read_csv("./data/processed/ab-ebdusers-pc-locations.csv")

df_hot_loc <- read_csv("./data/processed/ab-ebd-hotspot-locations.csv")

df_pt <- read_csv("./data/processed/ab-ebdusers-person-trips.csv")


#-------------------------------------------------------------------------------

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
hotspots <- sf_hot_loc$locality_id

df_dist <- sf_pc %>%
  st_distance(sf_hot_loc) %>%
  as_tibble() %>%
  mutate_all(.funs = as.numeric) %>%
  mutate(postal_code = pcodes) %>%
  select(postal_code, everything()) %>%
  pivot_longer(cols = V1:last_col(), names_to = "locality_id", values_to = "euc_distance") %>%
  mutate(locality_id = rep(hotspots, length(pcodes)),
         euc_distance_km = euc_distance / 1000) %>%
  select(postal_code, locality_id, euc_distance_km)

# Distances less than 80 and greater than 20 km
df_dist_20_80 <- df_dist %>%
  filter(euc_distance_km <= 80,
         euc_distance_km >= 20)

#-------------------------------------------------------------------------------

# Do some more prep

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

df_pc_other_loc <- df_pt_pc %>%
  select(postal_code, longitude, latitude) %>%
  distinct() %>%
  # Non-Edmonton and Calgary postal codes
  filter(!str_detect(postal_code, "^T6|^T5|^T3|^T2"))

# Prep for ggmap


df_prep <- df_dist_20_80 %>%
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

# Using ggmap

# Register googple API key
register_google(key = key_get("apikey", keyring = "google"))

# Wrap mapdist in `safely` so that it doesn't fail when query comes back empty
safe_mapdist <- safely(mapdist)

df_distance <- df_prep %>%
  mutate(distance = map2)

test2 <- test1 %>%
  mutate(km = map(.x = distance, .f = ~ pluck(.x$result[["km"]])),
         km = round(as.numeric(ifelse(km == "NULL", NA, km)), digits = 2),
         hours = map(.x = distance, .f = ~ pluck(.x$result[["hours"]])),
         hours = round(as.numeric(ifelse(hours == "NULL", NA, hours)), digits = 2))











