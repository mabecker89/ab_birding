# Created: February 2018

# Description: Uses the ebird dataset from Alberta to analyze spatial patterns in 
# birdwatching recreation.   

# NOTES: 

#-----------------------------------------------------------------------------

# Load Packages

library(tidyverse)
library(ggmap)
library(tmap)
library(sf)
library(rgdal)

#-----------------------------------------------------------------------------

# Import data

ab_ebird_all <- read_delim("./Data/Raw/ebd_CA-AB_200901_201711_relNov-2017.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)

ab_users_pc <- read_csv("./Data/Raw/AlbertaUsers.csv")

#-----------------------------------------------------------------------------

# Remove spaces from column names
colnames(ab_ebird_all) <- gsub(" ", "_", colnames(ab_ebird_all))

ab_ebird_tidy <- ab_ebird_all %>%
  select(GLOBAL_UNIQUE_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, COUNTY, IBA_CODE, BCR_CODE, 
         LOCALITY, LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE, OBSERVER_ID, FIRST_NAME, 
         LAST_NAME, SAMPLING_EVENT_IDENTIFIER, NUMBER_OBSERVERS, GROUP_IDENTIFIER, TRIP_COMMENTS) 

# Number of unique observers in Alberta since 2009: 3,982
unique_obsvers <- distinct(ab_ebird_tidy, OBSERVER_ID, .keep_all = FALSE)

# Generate hotspots dataframe
hotspots <- ab_ebird_tidy %>%
  filter(LOCALITY_TYPE == "H") %>%
  distinct(LOCALITY, .keep_all = TRUE) %>%
  select(LOCALITY, LOCALITY_ID, LATITUDE, LONGITUDE)

write_csv(hotspots, "./Data/Clean/AB_Hotspots.csv")

# Generate data table of bird watching 'person-trips'; ie., unique combinations of location, date, and observer. 
person_trips <- ab_ebird_tidy %>%
  distinct(LATITUDE, LONGITUDE, OBSERVATION_DATE, OBSERVER_ID, .keep_all = TRUE) %>%
  select(LATITUDE, LONGITUDE, OBSERVATION_DATE, OBSERVER_ID, NUMBER_OBSERVERS)

person_trips[is.na(person_trips)] <- 1

# Locality Types
loc_type <- ab_ebird_tidy %>%
  group_by(LOCALITY_TYPE) %>%
  summarise(count = n())
  # Looks like the vast majority are of two types: 'hotspot' (n = 1,123,206) and 'personal' (979,802).

checklist_type <- ab_ebird_tidy %>%
  group_by(SAMPLING_EVENT_IDENTIFIER, LOCALITY_TYPE) %>%
  summarise(count = n())

# Maybe we only are interested in hotspot observations at the moment. 
ab_ebird_hs <- ab_ebird_tidy %>%
  filter(LOCALITY_TYPE == "H") 
# Which hotspots are most visited? (or, have had the most observations recorded)
hs_summary <- ab_ebird_hs %>%
  group_by(LOCALITY) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# This is observations. But what about checklists? 

# What is the distribution in the number of observations entered by user?
n_obs <- ab_ebird_tidy %>%
  group_by(OBSERVER_ID) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

hist <- ggplot(data = n_obs, aes(x = count)) +
  geom_histogram(binwidth = 100)

hist

# Let's figure out who are the 'outliers' here.
names <- ab_ebird_tidy %>%
  distinct(OBSERVER_ID, FIRST_NAME, LAST_NAME)

n_obs <- n_obs %>%
  left_join(names, by = "OBSERVER_ID")
# Some observers to leave out: Environment Canada, Beaverhill Bird Observatory, Inglewood Volunteer, Nature Calgary,
#     EdmontonNature Club, Calgary BirdBanding Society, Frank Lake Historic Bird Records, Edmonton Area Land Trust

# How many checklists have been done? 
checklists <- ab_ebird_tidy %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>% # This is the checklist variable 
  summarise(count = n()) %>%
  arrange(desc(count))
# 195,375 checklists in Alberta between 2009-2017. Now, are these what we can consider to be 'trips'? 

# What about checklists specifically in birding hotspots? 
checklists_h <- ab_ebird_tidy %>%
  filter(LOCALITY_TYPE == "H") %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# Only 77,000 checklists occurred in hotspots.

# We want the distribution in the number of checklists recorded by user. 
num_check <- ab_ebird_tidy %>%
  filter(LOCALITY_TYPE == "H") %>%
  group_by(OBSERVER_ID) %>%
  summarise(count = n_distinct(SAMPLING_EVENT_IDENTIFIER)) %>%
  arrange(desc(count))
# Only 2,420 observers went to hotspots ... hmm. This could cut down on our distance calculations.

# Number of checklists per hotspot
check_per_hot <- ab_ebird_tidy %>%
  filter(LOCALITY_TYPE == "H") %>%
  group_by(LOCALITY) %>%
  summarise(count = n_distinct(SAMPLING_EVENT_IDENTIFIER)) %>%
  arrange(desc(count))
# But we don't necessarily want to cut down on the number of hotspots. It's significant that people choose not 
# to visit certain ones, right? Usually cause they're far away!

plot2 <- ggplot(data = check_per_hot, aes(x = count)) +
  geom_histogram(binwidth = 10) +
  scale_x_continuous(limits = c(0,1000))

plot2


  
# Need to figure out if multiple checklists can occur in one day, one outing, etc. Also, how to integrate 'P' 
# (personal) birding locations. Maybe not into our initial site choice model. But we can map 'effort' across the
# landscape. How to do that though? Number of checklists per 10km grid cell? Per NETLOGO grid cell? 

checklists_hp <- ab_ebird_tidy %>%
  filter(LOCALITY_TYPE == "H" | LOCALITY_TYPE == "P") %>%
  group_by(SAMPLING_EVENT_IDENTIFIER, LOCALITY_TYPE) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

plot <- ggplot(data = checklists_hp, aes(x = count, fill = LOCALITY_TYPE)) +
  geom_density() 

plot














  














