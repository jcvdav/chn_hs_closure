# Create polygons corresponding to the two closures
# 
# This script creates an sf object containing two polygons.
# It then exports the object as a geopackage.
# 
# The communication (in Chinese) found here contains 
# he coordinates for the polygons. The text reads:
# 
# According to the results of scientific research
# monitoring and expert argumentation, starting from 2020,
# the following voluntary fishing moratorium measures
# will be implemented in key fishing grounds where
# deep-sea fishing vessels in the Southwest Atlantic and
# East Pacific are concentrated:
# - First, from July 1 to September 30, 32° S-44°S, 48°W-60°W in the high seas of the Southwest Atlantic Ocean# 
# - second is between 5°N-5°S and 110°W-95°W from September 1 to November 30
# 

# Load packages
library(sf)
library(rnaturalearth)
library(tidyverse)

## Create two polygons
# From July 1 to Sept 30
closure_1 <- cbind(x = c(-48, -60, -60, -48, -48),
                  y = c(-32, -32, -44, -44, -32)) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(polygon = "Atlantic", geometry = .)

# September 1 to Nov 30
closure_2 <- cbind (x = c(-110, -95, -95, -110, -110),
             y = c(5, 5, -5, -5, 5)) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(polygon = "Pacific", geometry = .)

# Combine them together
closure_areas <- rbind(closure_1, closure_2) %>% 
  st_set_crs(value = 4326) %>% 
  st_make_valid()

# Load Exclusive Economic Zones
eez <- st_read(dsn = file.path(data_path, "marine-regions-eez-v11", "World_EEZ_v11_20191118"),
                layer = "eez_v11") %>% 
  filter(ISO_SOV1 %in% c("ECU", "ARG", "BRA", "URY")) %>%                                       # Keep only relevant nations
  st_combine() %>%                                                                              # Combine and unionize to intersect and erase
  st_union() %>% 
  st_make_valid()

# Load coastline
coast <- ne_countries(continent = "South America", returnclass = "sf") %>%
  st_combine() %>%                                                                              # Combine and unionize to intersect and erase
  st_union() %>% 
  st_make_valid() %>% 
  st_buffer(dist = 1)                                                                           # Add a buffer to make sure coastline is not included


# Erase EEZ and Coastline from the closure polygons
cropped <- st_difference(closure_areas, coast) %>% 
  st_difference(eez) %>%
  st_make_valid()

# Export the polygons
st_write(cropped,
         dsn = file.path(project_path, "data", "processed_data", "high_seas_closure_polygons.gpkg"))

# END SCRIPT
