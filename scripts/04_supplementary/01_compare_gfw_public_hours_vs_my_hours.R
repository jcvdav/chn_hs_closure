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
  mutate(year = as.numeric(str.remove(year, "fishing_hours_")))

# In principle, this allows me to check the original calculations of fishing effort by GFW (the data queried here)
# and compare it against what I can get independently.
# In this same script, I need to add:
# - Connection to GFW_research
# Query that gets chinese squid jiggers from gfw_research
# Query that calculates fihing effort as the sum of hours when it is a vessel is loitering at night
# Get this by year
# Plot my calculation vs public version of the data

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------