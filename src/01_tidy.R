#-------------------------------------------------------------------------------

# Date: March 2020

# Description: Cleaning the ebird (Jan 2020) dataset

#-------------------------------------------------------------------------------

# Load packages

library(tidyverse)
library(lubridate)
library(leaflet)
library(leaflet.extras)

#-------------------------------------------------------------------------------

# Import data

# Raw observations
df_ebd_ab_all <- read_delim("./data/base/ebd_CA-AB_prv_relJan-2020.txt",
                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)

# Alberta counties
ab_prov <- sf::st_read("./data/base/spatial/ab_counties_md.shp", stringsAsFactors = F, quiet = T) %>%
  sf::st_transform("+init=epsg:4326") %>%
  select(MD_NAME)

#-------------------------------------------------------------------------------

# Tidying

# Subset observations:
df_ebd_ab_sub <- df_ebd_ab_all %>%
  # Remove spaces from column names
  rename_all(~gsub(" ", ".", .x)) %>%
  # Remove observations pre-2009
  mutate(OBSERVATION.DATE = ymd(OBSERVATION.DATE)) %>%
  filter(OBSERVATION.DATE >= "2009-01-01") %>%
  select(COMMON.NAME, SCIENTIFIC.NAME, COUNTY, IBA.CODE, BCR.CODE,
         LOCALITY:OBSERVATION.DATE, OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER,
         GROUP.IDENTIFIER, TRIP.COMMENTS)

# Hotspots
df_hotspots <- df_ebd_ab_sub %>%
  filter(LOCALITY.TYPE == "H") %>%
  select(LOCALITY, LOCALITY.ID, LATITUDE, LONGITUDE) %>%
  distinct()

# Retrieve number of checklists and unique species for each hotspot
df_hotspot_checklists <- df_ebd_ab_sub %>%
  filter(LOCALITY.TYPE == "H") %>%
  group_by(LOCALITY, LOCALITY.ID) %>%
  summarise(n_checklist = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            n_species = n_distinct(SCIENTIFIC.NAME))

# Append checklist and species info to hotspot df
df_hotspots_full <- df_hotspots %>%
  left_join(df_hotspot_checklists, by = c("LOCALITY", "LOCALITY.ID"))
  # Only include hotspots with >= 50 checklists since 2009
  # filter(n_checklist >= 50)

# Join county information to hotspots
df_hs_county <- df_hotspots_full %>%
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  sf::st_join(ab_prov, left = TRUE) %>%
  filter(!is.na(MD_NAME)) %>%
  group_by(MD_NAME) %>%
  top_n(n = 5, wt = n_checklist)

# Let's make a leaflet map!

ab_ebird_hotspots <- ab_prov %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles("Stamen.Terrain") %>%
  addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
  addFullscreenControl() %>%
  addResetMapButton() %>%
  addScaleBar(position = "bottomleft",
              options = scaleBarOptions(imperial = FALSE)) %>%
  
  # Polyline layer for counties
  addPolylines(color = "#070707", weight = 3, smoothFactor = 0.2) %>%
  
  # Circle Markers
  addCircleMarkers(data = df_hs_county, stroke = FALSE, 
                   fillOpacity = 1, radius = 7, color = "#f05c1d",
                   popup = paste("<b>", df_hs_county$MD_NAME, "</b>", "<br>", "<br>",
                                 "Hotspot name:", df_hs_county$LOCALITY, "<br>", 
                                 "Number of checklists:", df_hs_county$n_checklist, "<br>", 
                                 "Number of species seen:", df_hs_county$n_species),
                   group = "Hotspots") %>%
  
  # Layers Control
  addLayersControl(overlayGroups = c("Imagery", "Hotspots"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  hideGroup("Imagery")

htmlwidgets::saveWidget(ab_ebird_hotspots, file = "ab_hs_map.html", selfcontained = TRUE)










