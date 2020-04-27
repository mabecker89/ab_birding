#-------------------------------------------------------------------------------

# Title: 00_map-making
# Date: April 2020

# Description: Making fun maps! 

#-------------------------------------------------------------------------------

# Load packages

library(sf)
library(leaflet)
library(leaflet.extras)
library(dplyr)

#-------------------------------------------------------------------------------

# Let's make a leaflet map!

# Alberta counties
ab_prov <- sf::st_read("./data/base/spatial/ab_counties_md.shp", stringsAsFactors = F, quiet = T) %>%
  sf::st_transform("+init=epsg:4326") %>%
  select(MD_NAME)

# Join county information to hotspots
df_hs_county <- df_hotspots_full %>%
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  sf::st_join(ab_prov, left = TRUE) %>%
  filter(!is.na(MD_NAME)) %>%
  group_by(MD_NAME) %>%
  top_n(n = 5, wt = n_checklist)

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