library(tidyverse); library(lubridate)

### Temporal survey data

read.csv("data/temporal-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  group_by(Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>%
  mutate(d = day(Day), 
         m = month(Day)) -> years

years %>%
  group_by(Site, m, d) %>%
  summarise(TM = mean(T)) %>%
  merge(years) %>% 
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) -> time

### Spatial survey data

read.csv("data/spatial-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  merge(read.csv("data/spatial-survey-header.csv")) %>%
  group_by(Plot, Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) -> sites

sites %>%
  group_by(Site, Day) %>%
  summarise(TM = mean(T)) %>%
  merge(sites) %>% 
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) -> space

### Temporal survey deviations

time %>%
  ggplot(aes(TM, T, fill = Site)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.5, shape = 21) +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  scale_fill_manual(values = c("goldenrod", "forestgreen",  "royalblue", "darkorchid")) +
  ggthemes::theme_tufte() + 
  theme(legend.position = c(.82, .15), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black")) +
  labs(title = "(A) Temperature deviations, temporal survey") +
  xlab("Mean daily temperature, 10-year average (ºC)") + 
  ylab("Mean daily temperature, logger record (ºC)") +
  coord_cartesian(xlim = c(-5, 20), ylim = c(-10, 25)) -> f1; f1

### Spatial survey deviations

space %>%
  ggplot(aes(TM, T, fill = Site)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.5, shape = 21) +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  scale_fill_manual(values = c("goldenrod", "forestgreen",  "royalblue", "darkorchid")) +
  ggthemes::theme_tufte() + 
  theme(legend.position = c(.82, .15), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black")) +
  labs(title = "(B) Temperature deviations, spatial survey") +
  xlab("Mean daily temperature, site average (ºC)") + 
  ylab("Mean daily temperature, logger record (ºC)") +
  coord_cartesian(xlim = c(-5, 20), ylim = c(-10, 25)) -> f2; f2

### Join

cowplot::plot_grid(f1, f2, nrow = 2) -> plot1; plot1

### Save figure

ggsave(plot1, file = "results/figures/temperature-deviations.png", 
       path = NULL, scale = 1, width = 127, height = 2*127, units = "mm", dpi = 600)

# ggsave(fig, file = "results/figures/nmds-species.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182 height = 182, units = "mm", dpi = 600, compression = "lzw")
