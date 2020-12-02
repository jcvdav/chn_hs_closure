

# Load packages
library(sf)
library(tidyverse)

#Source
#http://www.gov.cn/zhengce/zhengceku/2020-06/03/content_5516936.htm

# From July 1 to Sept 30, intersecting with Argentinian EEZ
closure_1 <- cbind(x = c(-48, -60, -60, -48, -48),
                  y = c(-32, -32, -44, -44, -32)) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(polygon = "closure 1", geometry = .)

# September 1 to Nov 30 Intersecting with Ecuador'ss EEZ
closure_2 <- cbind (x = c(-110, -95, -95, -110, -110),
             y = c(5, 5, -5, -5, 5)) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(polygon = "closure 2", geometry = .)


closure_areas <- rbind(closure_1, closure_2) %>% 
  st_set_crs(value = 4326) %>% 
  st_make_valid()

eezs <- st_read(dsn = file.path(data_path, "marine-regions-eez-v11", "World_EEZ_v11_20191118"),
                layer = "eez_v11") 

eez <- eezs %>% 
  filter(ISO_SOV1 %in% c("ECU", "ARG", "BRA", "URY")) %>% 
  group_by(ISO_SOV1) %>% 
  summarize(a = 1) %>% 
  ungroup() %>% 
  select(-a) %>% 
  rmapshaper::ms_simplify() %>%
  st_make_valid() %>% 
  st_combine() %>% 
  st_union() %>% 
  st_make_valid()

coast <- rnaturalearth::ne_countries(continent = "South America", returnclass = "sf") %>% 
  st_combine() %>% 
  st_union() %>% 
  st_make_valid() %>% 
  st_buffer(dist = 1)

mapview(list(eez, closure_areas))

cropped <- st_difference(closure_areas, eez) %>% 
  st_difference(coast)

ggplot(cropped) +
  geom_sf()

