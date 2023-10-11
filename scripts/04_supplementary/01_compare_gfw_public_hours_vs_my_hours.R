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

# BigQuery stuff ---------------------------------------------------------------
# Authenticate
bq_auth("juancarlos@ucsb.edu")

# Establish a connection
gfw_public <- dbConnect(
  bigquery(),
  project = "global-fishing-watch",
  dataset = "gfw_public_data",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE)


# Load data --------------------------------------------------------------------
# Query the data for chinese squid jiggers
public_chn_squid_jiggers <- tbl(gfw_public, "fishing_vessels_v2") %>% 
  filter(vessel_class_gfw == "squid_jigger",
         flag_gfw == "CHN") %>% 
  select(mmsi, contains("gfw"), contains("hours")) %>% 
  collect()

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
public_chn_squid_jiggers_long <- public_chn_squid_jiggers %>% 
  pivot_longer(cols = contains("hours"), names_to = "year", values_to = "fishing_hours") %>% 
  mutate(year = as.numeric(str_remove(year, "fishing_hours_"))) %>% 
  select(ssvid = mmsi,
         year,
         public_fishing_hours = fishing_hours)

# In principle, this allows me to check the original calculations of fishing effort by GFW (the data queried here)
# and compare it against what I can get independently.
# In this same script, I need to add:
# - Connection to GFW_research
# Query that gets chinese squid jiggers from gfw_research
# Query that calculates fihing effort as the sum of hours when it is a vessel is loitering at night
# Get this by year
# Plot my calculation vs public version of the data


####
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
gfw_research_squid_jiggers <- tbl(con, "pipe_v20201001_fishing") %>% 
  inner_join(vessel_info, by = "ssvid") %>%
  inner_join(good_seg, by = "seg_id") %>%
  mutate(year = sql("EXTRACT(year FROM timestamp)")) %>% 
  filter(year >= 2016) %>% 
  group_by(ssvid, year) %>% 
  summarize(n_ssvid = n_distinct(ssvid),
            n_pos = n(),
            total_hours = sum(hours, na.rm = T),
            fishing_hours = sum(ifelse(nnet_score == 1, hours, 0), na.rm = T),
            loitering_hours = sum(ifelse(night_loitering == 1, hours, 0), na.rm = T)) %>% 
  collect()

####

combined <- gfw_research_squid_jiggers %>% 
  left_join(public_chn_squid_jiggers_long, by = c("ssvid", "year")) %>% 
  drop_na()

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

model <- lm(loitering_hours ~ public_fishing_hours, data = combined)

label <- paste0(
  "y = ", round(coef(model)[1], 2), "+", round(coef(model)[2], 4), "*x
  R2 = ", round(summary(model)[[9]], 4)
)

ggplot(combined,
       aes(x = public_fishing_hours,
           y = loitering_hours)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Fishing hours from GFW public data",
       y = "Fihshing hours calculated here") +
  theme_bw() +
  annotate(geom = "text",
           x = 1000,
           y = 3000,
           label = label)


## EXPORT ######################################################################

# X ----------------------------------------------------------------------------