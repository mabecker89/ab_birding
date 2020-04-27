#-------------------------------------------------------------------------------

# Title: 01_tidy
# Date: March 2020

# Description: Cleaning the ebird (Jan 2020) dataset. 

#-------------------------------------------------------------------------------

# Load packages

library(readr)
library(dplyr)
library(lubridate)
library(janitor)

#-------------------------------------------------------------------------------

# Import data

# Raw observations
df_ebd_ab_all <- read_delim("./data/base/ebd_CA-AB_prv_relJan-2020.txt",
                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)

# Alberta users' postal codes
df_pc_all <- read_csv("./data/base/ebd_ab_userspc.csv")

#-------------------------------------------------------------------------------

# Tidying

# Subset observations:
df_ebd_ab_sub <- df_ebd_ab_all %>%
  # Clean up column names
  clean_names(case = "snake") %>%
  # Remove observations pre-2009
  mutate(observation_date = ymd(observation_date)) %>%
  filter(observation_date >= "2009-01-01") %>%
  select(common_name, scientific_name, county, iba_code, bcr_code,
         locality:observation_date, observer_id, sampling_event_identifier,
         group_identifier, trip_comments)

# Retrieve number of checklists and unique species for each locality (hotspot or personal)
df_loc_check <- df_ebd_ab_sub %>%
  filter(locality_type == "H" | locality_type == "P") %>%
  group_by(locality, locality_id) %>%
  summarise(n_checklist = n_distinct(sampling_event_identifier),
            n_species = n_distinct(scientific_name))

# Hotspots
df_hotspots <- df_ebd_ab_sub %>%
  filter(locality_type == "H") %>%
  select(locality, locality_id, longitude, latitude) %>%
  distinct() %>%
  left_join(df_loc_check, by = c("locality", "locality_id")) 
  # Only include hotspots with >= 50 checklists since 2009
  # filter(n_checklist >= 50)

df_hotspots %>%
  select(locality_id, longitude, latitude) %>%
  write_csv("./data/processed/ab-ebd-hotspot-locations.csv")

# Personal
df_personal <- df_ebd_ab_sub %>%
  filter(locality_type == "P") %>%
  select(locality, locality_id, longitude, latitude) %>%
  distinct() %>%
  left_join(df_loc_check, by = c("locality", "locality_id"))

# 'Person-trips' - unique combinations of locality, date, sampling event, and observer
df_pt <- df_ebd_ab_sub %>%
  filter(locality_type == "H") %>%
  select(locality, locality_id, observer_id, observation_date) %>%
  distinct()

df_pt %>%
  write_csv("./data/processed/ab-ebdusers-person-trips.csv")

#-------------------------------------------------------------------------------

# Clean postal code data

df_ab_pc_locations <- read_csv("./data/base/MultipleEnhancedPostalCodeLocations.csv") %>%
  filter(PROV == "AB",
         SLI == "1") %>%
  select(postal_code = POSTALCODE, longitude = HP_LONG, latitude = HP_LAT)
  
df_pc_sub <- df_pc_all %>%
  clean_names(case = "snake") %>%
  mutate(postal_code = toupper(post_code),
         postal_code = str_remove(postal_code, " ")) %>%
  filter(nchar(postal_code) == "6") %>%
  left_join(df_ab_pc_locations, by = "postal_code") %>%
  filter(!is.na(longitude)) %>%
  select(-post_code)

write_csv(df_pc_sub, "./data/processed/ab-ebdusers-pc-locations.csv")

#-------------------------------------------------------------------------------
