# Created: February 2018

# Description: Uses the ebird dataset from Alberta to analyze spatial patterns in 
# birdwatching recreation.   

# NOTES: 

#-----------------------------------------------------------------------------

# Load Packages

library(tidyverse)
library(ggmap)

#-----------------------------------------------------------------------------

# Import Data

ab_ebird_all <- read_delim("./Data/Raw/ebd_CA-AB_200901_201711_relNov-2017.txt", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)

postal_codes <- read_csv("./Data/Raw/AlbertaUsers.csv")

#-----------------------------------------------------------------------------

# Tidy observation data, create dataframe of hotspots. 

colnames(ab_ebird_all) <- gsub(" ", "_", colnames(ab_ebird_all))

ab_ebird_tidy <- ab_ebird_all %>%
  select(GLOBAL_UNIQUE_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, COUNTY, IBA_CODE, BCR_CODE, 
         LOCALITY, LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE, OBSERVER_ID, FIRST_NAME, 
         LAST_NAME, SAMPLING_EVENT_IDENTIFIER, NUMBER_OBSERVERS, GROUP_IDENTIFIER, TRIP_COMMENTS)

hotspots <- ab_ebird_tidy %>%
  filter(LOCALITY_TYPE == "H") %>%
  distinct(LOCALITY, .keep_all = TRUE) %>%
  select(LOCALITY, LOCALITY_ID, LATITUDE, LONGITUDE) # 1,045 hotspots across Alberta

write_csv(hotspots, "./Data/Clean/AB_Hotspots.csv")

#-----------------------------------------------------------------------------

# Tidy postal code data, add locations.

postal_codes <- postal_codes %>%
  mutate_all(funs(toupper)) %>%
  rename(POSTALCODE = POST_CODE)

postal_codes <- as.data.frame(lapply(postal_codes, function(x) {
  gsub(" ","",x) }), stringsAsFactors = FALSE)

postal_codes <- postal_codes %>%
  subset(nchar(as.character(POSTALCODE)) == 6)

postal_codes$OBSERVER_ID <- tolower(postal_codes$OBSERVER_ID)

pc_locations <- read_csv("./Data/Raw/MultipleEnhancedPostalCodeLocations.csv")

ab_pc_locations <- pc_locations %>% 
  filter(PROV == "AB") %>%
  filter(SLI == "1") %>%
  select(POSTALCODE, HP_LONG, HP_LAT)

postal_codes <- postal_codes %>%
  left_join(ab_pc_locations, by = "POSTALCODE")

distinct_pc <- postal_codes %>%
  select(POSTALCODE, HP_LONG, HP_LAT, OBSERVER_ID) %>%
  distinct %>%
  filter(!is.na(HP_LONG)) # 3,599 postal codes. 

remove(pc_locations)
write_csv(distinct_pc, "./Data/Clean/PostalCodes.csv")

#-----------------------------------------------------------------------------
# Notes: Use ArcGIS to calculate point distances between each hotspot and postal
#        code (using AB_Hotspots and PostalCodes csv files). 
#-----------------------------------------------------------------------------

# Create dataframe of euclidean distances between all postal codes and all hotspots.

ab_hotspot_fid <- read_csv("./Data/Raw/ab_hotspot_fid.csv")
postal_code_fid <- read_csv("./Data/Raw/postal_code_fid.csv")
point_distances <- read_csv("./Data/Raw/final_point_distances.txt")

distances <- point_distances %>%
  select(INPUT_FID, NEAR_FID, DISTANCE) %>%
  mutate(OBJECTID = (INPUT_FID + 1)) %>%
  select(OBJECTID, NEAR_FID, DISTANCE) %>%
  left_join(postal_code_fid, by = "OBJECTID") %>%
  mutate(pcLOC = paste(HP_LAT, HP_LONG, sep = ",")) %>%
  select(-c(OBJECTID, HP_LONG, HP_LAT)) %>%
  mutate(OBJECTID = (NEAR_FID + 1)) %>%
  left_join(ab_hotspot_fid, by = "OBJECTID") %>%
  mutate(hotLOC = paste(LATITUDE, LONGITUDE, sep = ",")) %>%
  mutate(DISTANCE_km = (DISTANCE / 1000)) %>%
  select(LOCALITY, LOCALITY_I, hotLOC, POSTALCODE, pcLOC, DISTANCE_km)

#-----------------------------------------------------------------------------

# Filter observations based on whether we have a postal code for the user associated
# with the observation. 

ab_ebird_pcvar <- ab_ebird_tidy %>%
  left_join(distinct_pc, by = "OBSERVER_ID") %>%
  mutate(HAS_PC = ifelse(is.na(POSTALCODE), 0, 1))

ab_ebird_pc <- ab_ebird_pcvar %>%
  filter(HAS_PC == "1") # Lost nearly 50% of observations.

ab_ebird_nopc <- ab_ebird_pcvar %>%
  filter(HAS_PC == "0") # Obs for users w/o postal code. Might be useful later on. 

user_pc <- ab_ebird_pc %>%
  select(POSTALCODE) %>%
  distinct # 873 unique postal codes. 

#-----------------------------------------------------------------------------

# Filter 'distances' combination matrix by the postal codes we have in the dataset

distances_pc <- user_pc %>%
  left_join(distances, by = "POSTALCODE") # 2.8 million down to 912k combinations 

# Postal codes NOT in Edmonton or Calgary, filtered to a euclidean distance less than 80km. 
distances_other <- distances_pc %>%
  filter(!str_detect(POSTALCODE, "^T6|^T5|^T2|^T3")) %>%
  filter(DISTANCE_km <= 80) # 41,670 combinations. 

# Postal codes in Edmonton or Calgary, filtered to a euclidean distance less than 80km.
distances_edmcgy <- distances_pc %>%
  filter(str_detect(POSTALCODE, "^T6|^T5|^T2|^T3")) %>%
  filter(DISTANCE_km <= 80) # 101,594 combinations. 

# Cut down combos in EDM/CGY by aggregating up to the FSA level. 
pc_edmcgy <- distances_edmcgy %>%
  select(POSTALCODE, pcLOC) %>%
  distinct() %>%
  separate(POSTALCODE, into = c("FSA", "OTHER"), sep = 3, remove = FALSE) %>%
  separate(pcLOC, into = c("LATITUDE", "LONGITUDE"), sep = ",") %>%
  select(-OTHER)

write.csv(pc_edmcgy, file = "./Data/Raw/pc_edmcgy.csv", row.names = FALSE)

#-----------------------------------------------------------------------------
# Use ArcGIS to calculate mean centre of each FSA (lat, long)
#-----------------------------------------------------------------------------

fsa_locations <- read_csv(file = "./Data/Raw/fsa_meancenter.csv")

fsa_locations <- fsa_locations %>%
  select(-OBJECTID)

distances_edmcgy <- distances_edmcgy %>%
  separate(POSTALCODE, into = c("FSA", "OTHER"), sep = 3, remove = FALSE) %>%
  left_join(fsa_locations, by = "FSA") %>%
  select(-c(FSA, OTHER, pcLOC)) %>%
  unite("pcLOC", YCoord, XCoord, sep = ",")

simple_edmcgy <- distances_edmcgy %>%
  distinct(hotLOC, pcLOC) # 13,722 combinations.

#-----------------------------------------------------------------------------

# Calculate driving distances and times using ggmap

# library(ggmap)

# register_google(key = '')

# driving_other <- bind_rows(
#   apply(
#     subset(distances_other, select = c("pcLOC", "hotLOC")), 1, function(x) mapdist(x[1], x[2], mode = "driving", override_limit = TRUE)
#   )
# )

# write_csv(driving_other, "./Data/Raw/driving_other.csv")

# driving_edmcgy <- bind_rows(
#   apply(
#     subset(simple_edmcgy, select = c("pcLOC", "hotLOC")), 1, function(x) mapdist(x[1], x[2], mode = "driving", override_limit = TRUE)
#   )
# )

# write_csv(driving_edmcgy, "./Data/Raw/driving_edmcgy.csv")

#-----------------------------------------------------------------------------

# Load Data

driving_edmcgy <- read_csv("./Data/Raw/driving_edmcgy.csv")
driving_other <- read_csv("./Data/Raw/driving_other.csv")

#-----------------------------------------------------------------------------

# What are some issues? 

summary(driving_edmcgy) # 357 NAs

summary(driving_other) # 738 NAs

na_edmcgy <- driving_edmcgy %>%
  filter(is.na(km)) %>%
  select(from, to)

na_other <- driving_other %>%
  filter(is.na(km)) %>%
  select(from, to)

# What are the 'trouble' hotspots?
trouble_hs <- na_edmcgy %>%
  select(to) %>%
  distinct %>%
  rename(hotLOC = to) %>%
  left_join(distances_edmcgy, by = "hotLOC") %>%
  select(hotLOC, LOCALITY) %>%
  distinct # Most of the trouble hotspots are in Kananaskis - and the road is closed until June 14. Calculate then. 

trouble_hs_1 <- na_other %>%
  select(to) %>%
  distinct %>%
  rename(hotLOC = to) %>%
  left_join(distances_other, by = "hotLOC") %>%
  select(hotLOC, LOCALITY) %>%
  distinct # Same issue - except a few extras around the Wood Buffalo, Jasper, and Banff areas. 
           # Might have to use the euclidean distance for those?
           # For WB, driving is not particularly relevant - no roads into these locations. 

#-----------------------------------------------------------------------------

# Putting it together

edmcgy_combos <- driving_edmcgy %>%
  rename(pcLOC = from, hotLOC = to) %>%
  left_join(distances_edmcgy, by = c("pcLOC", "hotLOC"))

other_combos <- driving_other %>%
  rename(pcLOC = from, hotLOC = to) %>%
  left_join(distances_other, by = c("pcLOC", "hotLOC")) %>%
  distinct

all_combos <- edmcgy_combos %>%
  bind_rows(other_combos) %>%
  select(POSTALCODE, pcLOC, LOCALITY, LOCALITY_I, hotLOC, DISTANCE_km, km, hours) 
# Note: 'LOCALITY' is the name of the eBird hotspot. 'DISTANCE_km' is the euclidean distance, 'km' is the driving distance.
#       All NAs related to driving were left in. To be calculated in June.

write_csv(all_combos, "./Data/Clean/all_combos_80km.csv")
# All combinations of postal codes -> hotspots less than 80km (Euclidean distance)

#-----------------------------------------------------------------------------

# Travel Cost

# Settings
yearly_hours <- 2040
opp_time <- 1/2
vehicle_cost <- 0.3

pc_income <- read_csv("./Data/Clean/PostalCodeIncome.csv")

travel_cost <- all_combos %>%
  left_join(pc_income, by = "POSTALCODE") %>%
  mutate(Cost_time = 2 * opp_time * (Med_Net_15 / yearly_hours) * hours,
         Cost_money = 2 * vehicle_cost * km,
         Cost_total = Cost_time + Cost_money) %>%
  select(POSTALCODE, LOCALITY, Cost_total)

write_csv(travel_cost, "./Data/Clean/TravelCost.csv")

#-----------------------------------------------------------------------------

# Birding Trips
# Note: Defining a birding trip as a day trip to a hotspot less than 80km away. 

# Number of observations per trip.
nobs_trip <- ab_ebird_pc %>%
  filter(LOCALITY_TYPE == "H") %>% # hotspots only
  group_by(SAMPLING_EVENT_IDENTIFIER) %>% # grouped by checklists, which aggregates observations
  summarise(OBSERVATIONS = n())

birding_trips <- ab_ebird_pc %>%
  filter(LOCALITY_TYPE == "H") %>% # Have to think carefully about what we'll do about 'P' trips. 
  select(SAMPLING_EVENT_IDENTIFIER, COUNTY, LOCALITY, LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE,
         OBSERVER_ID, NUMBER_OBSERVERS) %>%
  left_join(nobs_trip, by = "SAMPLING_EVENT_IDENTIFIER") %>%
  distinct # 39,582 'trips' (checklists) have been recorded in hotspots by users that we have postal codes for.
  
n_distinct(birding_trips$OBSERVER_ID) # 435 observers ... not a ton. Lots of attrition here. 






