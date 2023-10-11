################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  bigrquery,
  DBI,
  sf,
  furrr,
  tidyverse
)

# Load data --------------------------------------------------------------------
# Load HS data
hs_closures <- st_read(here("data",
                            "processed_data",
                            "high_seas_closure_polygons.gpkg")) %>% 
  group_by(polygon) %>% 
  group_split()

# Authenticate
bq_auth("juancarlos@ucsb.edu")

# Establish a connection
con <- dbConnect(
  bigquery(),
  project = "world-fishing-827",
  dataset = "gfw_research",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE)


## PROCESSING ##################################################################

# Get vessel info for all Chinese squid jiggers
vessel_info <- tbl(con, "vi_ssvid_v20230501") %>% 
  filter(on_fishing_list_best,
         sql("best.best_flag = 'CHN'"),
         sql("best.best_vessel_class = 'squid_jigger'")) %>%
  select(ssvid)

# Get good segments to eventually filter tracks
good_seg <- tbl(con, "pipe_v20201001_segs") %>%  
  filter(good_seg, good_seg2, !overlapping_and_short) %>% 
  select(seg_id)

# Build query ------------------------------------------------------------------
chn_activity <- tbl(con, "pipe_v20201001_fishing") %>% 
  inner_join(vessel_info, by = "ssvid") %>%
  inner_join(good_seg, by = "seg_id") %>%
  filter(between(lon, -60, -48) & between(lat, -44, -32) |
           between(lon, -110, -95) & between(lat, -5, 5)) %>% 
  mutate(date = sql("EXTRACT(date FROM timestamp)"),
         polygon = ifelse(lon < -80, "Pacific", "Atlantic")) %>% 
  filter(date > "2016-01-01") %>% 
  select(polygon, date, ssvid, lat, lon, nnet_score, night_loitering, hours)

activity_collected <- chn_activity  %>% 
  collect() %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) %>% 
  group_by(polygon) %>% 
  group_split()

# Perform spatial filter in parallel -------------------------------------------
# Declare future strategy
plan(multisession, workers = 2)

# Call filter
activity_spatialy_fltered <- future_map2_dfr(activity_collected,
                                             hs_closures,
                                             st_filter)
# Close the cluster
plan(sequential)

# Build a time series of activity ----------------------------------------------
# Calculate activity for the days in which we see something
base_ts <- activity_spatialy_fltered %>% 
  st_drop_geometry() %>%
  group_by(date, polygon) %>% 
  summarize(n_ssvid = n_distinct(ssvid),
            n_pos = n(),
            total_hours = sum(hours, na.rm = T),
            fishing_hours = sum(ifelse(nnet_score == 1, hours, 0), na.rm = T),
            loitering_hours = sum(ifelse(night_loitering == 1, hours, 0), na.rm = T)) %>% 
  ungroup()

# Declare a range of all dates (2016-2022)
range <- expand_grid(
  date = seq(ymd("2016-01-01"),
             ymd("2022-12-31"),
             by = "1 day"),
  polygon = c("Atlantic", "Pacific")
)

# Combine all days with days for which we observe
ts <- range %>% 
  left_join(base_ts, by = c("date", "polygon")) %>% 
  replace_na(replace = list(n_ssvid = 0,
                            n_pos = 0,
                            total_hours = 0,
                            fishing_hours = 0,
                            loitering_hours = 0)) %>% 
  # Add information about spatial closure and closure period
  mutate(
    period = ifelse(year(date) <= 2019, "Pre", "Post"),                         # Closure went into effect in 2020
    month = lubridate::month(date),
    closed = case_when(
      polygon == "Atlantic" & month %in% 7:9 ~ "Closed",                        # Atlantic closure is from July 1 to Sept 30
      polygon == "Pacific" & month %in% 9:11 ~ "Closed",                        # Pacific closure is from Sept 1 to Nov 30
      T ~ "Open"),
    closed_buffer = case_when(
      polygon == "Atlantic" & month %in% 5:10 ~ "Closed",                       # Window with before (2 months), during (3 months), and after (1 month) for Atlantic
      polygon == "Pacific" & month %in% 7:12 ~ "Closed",                        # Window with before (2 months), during (3 months), and after (1 month) for Pacific
      T ~ "Open"))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = ts,
        file = here("data", "processed_data", "ts_activity_within"))
