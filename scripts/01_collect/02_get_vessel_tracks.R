library(connections)
library(bigrquery)
library(tidyverse)

con <- connection_open(bigquery(),
                       project = "world-fishing-827",
                       dataset = "gfw_research",
                       billing = "emlab-gcp",
                       use_legacy_sql = FALSE,
                       allowLargeResults = TRUE)

vessel_info <- tbl(con, "vi_ssvid_v20201209") %>% 
  filter(on_fishing_list_best) %>% 
  filter(best$best_flag == "CHN") %>%
  select(ssvid)

good_segs <- tbl(con, "pipe_v20190502_segs") %>% 
  filter(good_seg) %>% 
  select(seg_id)

vessel_tracks <- tbl(con, "pipe_v20201001_fishing") %>% 
  mutate(lat_bin_center = floor(lat/0.1)*0.1 + 0.05,
         lon_bin_center = floor(lon/0.1)*0.1 + 0.05) %>% 
  mutate(date = sql("EXTRACT(DATE FROM timestamp)")) %>% 
  mutate(closure = case_when(c(between(lon, -60, -48) & between(lat, -44, -32)) ~ "Atlantic",
                            c(between(lon, -110, -95) & between(lat, -5, 5))  ~ "Pacific",
                            T ~ "None")) %>% 
  filter(closure %in% c("Atlantic", "Pacific")) %>% 
  filter(nnet_score == 1) %>% 
  inner_join(good_segs, by = "seg_id") %>% 
  inner_join(vessel_info, by = "ssvid") %>% 
  select(date, ssvid, lon, lat, lon_bin_center, lat_bin_center, hours, closure)

vessel_tracks_collected <- vessel_tracks %>% 
  collect()

closure_ts <- vessel_tracks_collected %>% 
  group_by(date, closure) %>% 
  summarize(hours = sum(hours, na.rm = T)) %>% 
  mutate(yday = lubridate::yday(date),
         month = lubridate::month(date),
         year = as.factor(lubridate::year(date)))

ggplot(closure_ts, aes(x = yday, y = hours, color = year)) +
  geom_point() +
  facet_wrap(~closure, ncol = 1, scales = "free_y")

grid <- vessel_tracks_collected %>% 
  group_by(lon_bin_center, lat_bin_center) %>% 
  summarize(hours = sum(hours, na.rm = T))

ggplot(grid, aes(x = lon_bin_center, y = lat_bin_center, fill = hours)) +
  geom_raster() +
  scale_fill_continuous(trans = "log10")

ggplot(closure_ts, aes(x = month, y = year, fill = hours)) +
  geom_raster() +
  facet_wrap(~closure)








