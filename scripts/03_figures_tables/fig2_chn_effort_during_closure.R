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

# Stats for text ---------------------------------------------------------------
#Historical max pre-expansion
ts %>%
  filter(date < ymd("2020-07-01")) %>% 
  group_by(polygon) %>%
  slice_max(loitering_hours)

ts %>%
  filter(closed == "Closed",
         period == "Pre") %>%
  group_by(polygon) %>%
  slice_max(loitering_hours)

ts %>%
  filter(closed == "Closed",
         period == "Pre") %>%
  mutate(year = lubridate::year(date)) %>% 
  group_by(polygon, year) %>%
  summarize(loitering_hours = sum(loitering_hours)) %>%
  group_by(polygon) %>%
  summarize(sd_loitering = sd(loitering_hours),
            loitering_hours = mean(loitering_hours))

ts %>%
  filter(date < ymd("2020-07-01")) %>% 
  group_by(polygon) %>%
  summarize(sd_loitering = sd(loitering_hours),
            loitering_hours = mean(loitering_hours),
            sd_n = sd(n_ssvid),
            n_ssvid = mean(n_ssvid))


ts_plot <- ggplot(data = ts) +
  geom_rect(data = closure_windows,
            mapping = aes(xmin = start,
                          xmax = end,
                          ymin = 0,
                          ymax = Inf,
                          fill = closed),
            linetype = "dashed",
            linewidth = 0.1) +
  geom_vline(xintercept = ymd("2020-06-1"),
             linetype = "dashed") +
  geom_line(mapping = aes(x = date,
                          y = loitering_hours / 1e3,
                          color = polygon)) +
  facet_wrap(~polygon, ncol = 2) +
  scale_fill_manual(values = closure_pal) +
  scale_color_manual(values = polygon_pal) +
  scale_y_continuous(expand = c(expansion(mult = c(0.05, 0.1)))) +
  labs(x = "Date",
       y = "Utilization rate (Thousand vessel-hours per day)",
       fill = "Closure in effect",
       color = "Polygon") +
  theme(legend.position = "None",
        strip.background = element_blank())

during_hours <- ts %>% 
  filter(closed == "Closed") %>%  
  mutate(year = lubridate::year(date)) %>% 
  ggplot(mappin = aes(x = year,
                      loitering_hours / 1e3,
                      fill = polygon)) +
  geom_vline(xintercept = 2019.5,
             linetype = "dashed") +
  stat_summary(geom = "col",
               fun = sum,
               color = "black") +
  facet_wrap(~polygon, ncol = 2) +
  scale_fill_manual(values = polygon_pal) +
  scale_color_manual(values = closure_pal) +
  scale_y_continuous(expand = c(expansion(mult = c(0.05, 0.1)))) +
  labs(x = "Year",
       y = "Utilization (Thousand vessel-hours)") +
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text = element_blank())

p <- cowplot::plot_grid(ts_plot + facet_wrap(~polygon, ncol = 1),
                   during_hours + facet_wrap(~polygon, ncol = 1),
                   ncol = 2,
                   align = "hv",
                   axis = "l",
                   rel_widths = c(2, 1),
                   labels = "AUTO")

ggsave(
  plot = p,
  filename = here("results", "img", "fig2_chn_compliance.pdf"),
  width = 6,
  height = 3,
  units = "in"
)


# Historical max post-expansion
ts %>%
  filter(date >= ymd("2020-07-01")) %>% 
  group_by(polygon) %>%
  slice_max(loitering_hours)

total_hours <- ts %>% 
  mutate(year = lubridate::year(date)) %>% 
  ggplot(aes(x = year,
             loitering_hours / 1e3,
             fill = polygon,
             alpha = closed)) +
  geom_vline(xintercept = 2019.5,
             linetype = "dashed") +
  stat_summary(geom = "col",
               fun = sum,
               color = "black",
               linewidth = 0.2,
               position = "stack") +
  facet_wrap(~polygon, scales = "free", ncol = 2) +
  scale_fill_manual(values = polygon_pal) +
  scale_color_manual(values = closure_pal) +
  scale_alpha_manual(values = c("Closed" = 0.5, "Open" = 1)) +
  scale_y_continuous(expand = c(expansion(mult = c(0, 0.1)))) +
  labs(x = "Year",
       y = "Utilization (Thousand hours)") +
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text = element_blank())

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



