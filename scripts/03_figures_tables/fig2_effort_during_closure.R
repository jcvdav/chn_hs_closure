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
  cowplot,
  tidyverse
)

# Load data --------------------------------------------------------------------

closure_windows <- readRDS(here("data", "processed_data", "closure_windows.rds"))
ts <- readRDS(here("data", "processed_data", "ts_activity_within"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
closure_pal <- c("#C13832", "#D28E00")

ts_plot <- ggplot(data = ts) +
  geom_rect(data = closure_windows,
            mapping = aes(xmin = start, xmax = end, ymin = 0, ymax = Inf),
            linetype = "dashed",
            linewidth = 0.1,
            fill = "gray90") +
  geom_line(mapping = aes(x = date, y = hours, color = polygon)) +
  facet_wrap(~polygon, ncol = 2, scale = "free_y") +
  theme_bw() +
  scale_color_manual(values = closure_pal) +
  labs(x = "Date",
       y = "Hours per day") +
  theme(legend.position = "None",
        strip.background = element_blank())

during_hours <- ts %>% 
  filter(closed == "Closed") %>%  
  mutate(year = lubridate::year(date)) %>% 
  ggplot(aes(x = year, y = hours, fill = polygon)) +
  stat_summary(geom = "col", fun = sum, color = "black", linewidth = 0.2) +
  facet_wrap(~polygon, scales = "free", ncol = 2) +
  theme_bw() +
  scale_fill_manual(values = closure_pal) +
  labs(x = "Year",
       y = "Hours (closure)") +
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text = element_blank())

total_hours <- ts %>% 
  mutate(year = lubridate::year(date)) %>% 
  ggplot(aes(x = year, y = hours, fill = polygon)) +
  stat_summary(geom = "col", fun = sum, color = "black", linewidth = 0.2) +
  facet_wrap(~polygon, scales = "free", ncol = 2) +
  theme_bw() +
  scale_fill_manual(values = closure_pal) +
  labs(x = "Year",
       y = "Hours (all year)") +
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text = element_blank())

cowplot::plot_grid(ts_plot,
                   during_hours,
                   total_hours,
                   ncol = 1,
                   rel_heights = c(0.75, 1, 1),
                   labels = "AUTO")

## Perhaps figure 3 starts here

ts %>% 
  mutate(year = lubridate::year(date),
         day_active = 1 * (hours > 0)) %>% 
  ggplot(aes(x = year, y = day_active, fill = polygon)) +
  stat_summary(geom = "col", fun = sum, color = "black", linewidth = 0.2) +
  facet_wrap(~polygon, scales = "free", ncol = 2) +
  theme_bw() +
  scale_fill_manual(values = closure_pal) +
  labs(x = "Year",
       y = "Days with activity") +
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text = element_blank())

ts %>% 
  mutate(year = lubridate::year(date)) %>% 
  ggplot(aes(x = year, y = hours, fill = polygon, group = year)) +
  geom_boxplot() +
  # stat_summary(geom = "col", fun = sum, color = "black", linewidth = 0.2) +
  facet_wrap(~polygon, scales = "free", ncol = 2) +
  theme_bw() +
  scale_fill_manual(values = closure_pal) +
  labs(x = "Year",
       y = "Fishing hours") +
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text = element_blank())


## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

ts %>% 
  filter(closed_buffer == "Closed",
         polygon == "Atlantic",
         year(date) %in% c(2018, 2020))



