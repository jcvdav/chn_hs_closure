# Get vessel tracks

#### SET UP ###########################################################################
# Load packages
library(connections)
library(bigrquery)
library(tidyverse)

# Establish a connection
con <- connection_open(
  bigquery(),
  project = "world-fishing-827",
  dataset = "gfw_research",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE)

# Temprary fix to some stupid dbplyr / google mismatch
options(scipen = 20)                                          # Fix index error

#### CREATE SUBQUERIES (appended with *_sq) #########################################

# Get vessel info for all Chinese squid jiggers (There are 669 vessels)
vessel_info_sq <- tbl(con, "vi_ssvid_v20201209") %>% 
  filter(on_fishing_list_best) %>% 
  filter(best$best_flag == "CHN",
         best$best_vessel_class == "squid_jigger") %>%
  select(ssvid)                                          # Figure out how to get the vessel info later

# Get good segments to eventually filter tracks
good_segs_sq <- tbl(con, "pipe_v20190502_segs") %>% 
  filter(good_seg) %>% 
  select(seg_id)

# Get ssvids for all Chinese squid jiggers who fished within the closures before (There are 513 vessels)
affected_vessels_sq <- tbl(con, "pipe_v20201001_fishing") %>% 
  inner_join(vessel_info, by = "ssvid") %>% 
  filter(nnet_score == 1) %>% 
  filter(between(lon, -60, -48) & between(lat, -44, -32) |
           between(lon, -110, -95) & between(lat, -5, 5)) %>% 
  select(ssvid) %>% 
  distinct() %>% 
  mutate(treatment = "Treated")

# Get ssvids for Chinese squid jiggers that NEVER fished within the closures before (there are 153 vessels)
non_affected_vessels_sq <- vessel_info %>% 
  anti_join(affected_vessels_sq, by = "ssvid") %>% 
  select(ssvid) %>% 
  distinct() %>% 
  mutate(treatment = "Control")

#### COLLECT THE RELEVANT QUERIES ################################################

# Collect affected vessels
affected_vessels <- affected_vessels_sq %>% 
  collect()

# Collect non-affected vessels
non_affected_vessels <- non_affected_vessels_sq %>% 
  collect()

# Create treatment groups
treatment_groups <- rbind(affected_vessels,
                          non_affected_vessels)
  
# Get all activity by Chinese Squid Jiggers, and tag the location as inside / outside the soon-to-be closure areas
vessel_tracks <- tbl(con, "pipe_v20201001_fishing") %>% 
  filter(nnet_score == 1,
         sql("nnet_score IS NOT NULL")) %>% 
  mutate(date = sql("EXTRACT(DATE FROM timestamp)")) %>% 
  mutate(closure = case_when(c(between(lon, -60, -48) & between(lat, -44, -32)) ~ "Atlantic",
                             c(between(lon, -110, -95) & between(lat, -5, 5))  ~ "Pacific",
                             T ~ "None")) %>% 
  inner_join(vessel_info_sq, by = "ssvid") %>%                                                      # Keep only Chinese Squid jiggers
  inner_join(good_segs_sq, by = "seg_id") %>%                                                       # Keep only good segments
  select(date, ssvid, lon, lat, hours, closure) %>% 
  collect()


#### EXPORT DATA #################################################################


saveRDS(treatment_groups,
        file = file.path(project_data_path, "processed_data", "treatment_groups.rds"))

saveRDS(vessel_tracks,
        file = file.path(project_data_path, "processed_data", "vessel_tracks.rds"))










