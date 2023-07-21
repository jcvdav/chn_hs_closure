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
# # The communication (in Chinese) found here* contains 
# the coordinates for the polygons. The text reads:
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
# 
# *http://www.gov.cn/zhengce/zhengceku/2020-06/03/content_5516936.htm
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  lubridate,
  tidyverse
)

# Build a function
build_windows <- function(years, start, end, area) {
  expand_grid(year = years,
              start = start,
              end = end) %>% 
    mutate(start = ymd(paste(year, start, 1)),
           end = ymd(paste(year, end, 30)),
           polygon = area) %>% 
    select(-year)
}

## PROCESSING ##################################################################

# Define closure years
years <- 2020:2022
# Build a table of dates ofr each closure --------------------------------------
atlantic_window <- build_windows(years = years, start = 7, end = 9, area = "Atlantic")
pacific_window <- build_windows(years = years, start = 9, end = 11, area = "Pacific")

closure_windows <- bind_rows(atlantic_window,
                          pacific_window) 

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = closure_windows,
        file = here("data", "processed_data", "closure_windows.rds"))
