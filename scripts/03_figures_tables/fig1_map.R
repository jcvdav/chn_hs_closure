################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Uses: Flanders Marine Institute (2020). Union of the ESRI Country shapefile
# and the Exclusive Economic Zones (version 3). Available online at
# https://www.marineregions.org/. https://doi.org/10.14284/403
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  sf,
  rmapshaper,
  rnaturalearth,
  tidyverse
)

# Load data --------------------------------------------------------------------
lims <- c(ymin = -60, ymax = 35, xmin = -120, xmax = -20)

sf_use_s2(F)

hs_closures <- st_read(here("data",
                            "processed_data",
                            "high_seas_closure_polygons.gpkg"))

land <- ne_countries(continent = c("South America", "North America"),
                     returnclass = "sf", scale = "medium") %>% 
  bind_rows(
    ne_countries(country = "France",
                 returnclass = "sf")
  ) %>% 
  st_crop(y = lims)

eez <- st_read(dsn = here("data",
                          "raw_data",
                          "World_EEZ_V11_20191118_LR",
                          "eez_v11_lowres.gpkg")) %>% 
  st_crop(y = lims) %>% 
  ms_simplify()


## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------

bbox <- hs_closures %>% 
  st_buffer(dist = 10) %>% 
  st_transform("ESRI:54009") %>% 
  st_bbox()
  

## VISUALIZE ###################################################################

# Build map --------------------------------------------------------------------
map <- ggplot() +
  geom_sf(data = eez, fill = eez_col, color = eez_col) +
  geom_sf(data = land, fill = land_col, color = land_col) +
  geom_sf(data = hs_closures, aes(fill = polygon), color = "black", linewidth = 0.2) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = polygon_pal) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_sf(crs = "EPSG:8858") +
  theme(legend.position = "None",
        panel.grid.major = element_line(linewidth = 0.5,
                                        color = "gray"))

calendar <- expand_grid(polygon = c("Atlantic", "Pacific"),
                                month = month.abb) %>% 
  mutate(month = fct_relevel(month, month.abb)) %>% 
  mutate(closed = case_when(polygon == "Atlantic" & month %in% c("Jul", "Aug", "Sep") ~ "Closed",
                            polygon == "Pacific" & month %in% c("Sep", "Oct", "Nov") ~ "Closed",
                            T ~ "Open")) %>% 
  ggplot(aes(x = month,
             y = polygon,
             fill = polygon,
             alpha = closed)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = polygon_pal) +
  scale_alpha_manual(values = c(1, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  coord_equal() +
  theme(legend.position = "None",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Month")


fig1 <- plot_grid(map, calendar,
                  ncol = 1,
                  rel_heights = c(4.5, 1),
                  align = "v",
                  labels = "AUTO")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
ggsave(plot = fig1,
       filename = here("results", "img", "fig1_map.pdf"),
       height = 6,
       width = 4,
       units = "in")
