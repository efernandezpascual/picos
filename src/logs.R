library(tidyverse); library(urca); library(raster); library(tibbletime); library(lubridate)
Sys.setlocale("LC_TIME", "English")

### iButton logs

read.csv("data/spatial-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  merge(read.csv("data/spatial-survey-header.csv")) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) %>%
  ggplot(aes(Time, Temperature, color = Plot)) + 
  facet_wrap(~ Site, nrow = 1) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  labs(title = "(B) Hourly logs, spatial survey (20 iButtons per site, Oct 2018 - Aug 2019)") +
  xlab("Time (4-h recording inverval)") +
  ylab("Temperature (ºC)") +
  ggthemes::theme_tufte() +
  coord_cartesian(ylim = c(-10, 30)) +
  theme(strip.background = element_rect(colour = "grey96", fill = "grey96"),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 11, color = "black")) -> f1; f1

### Geoprecision logs

read.csv("data/temporal-survey-temperatures.csv") %>% 
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) %>%
  ggplot(aes(Time, Temperature)) + 
  facet_wrap(~ Site, nrow = 1) + 
  geom_line(color = "red") +
  labs(title = "(A) Hourly logs, temporal survey (2009-2018)") +
  xlab("Time (1-h recording inverval)") +
  ylab("Temperature (ºC)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  coord_cartesian(ylim = c(-10, 30)) +
  theme(strip.background = element_rect(colour = "grey96", fill = "grey96"),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 11, color = "black")) -> f2; f2

### Combine panels

cowplot::plot_grid(f2, f1, ncol = 1) -> fig

### Save figure

ggsave(fig, file = "results/figures/F2 - Temperature logs.png", 
       path = NULL, scale = 1, width = 182, height = 150, units = "mm", dpi = 600)
# ggsave(fig, file = "results/figures/temperature-logs.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182, height = 160, units = "mm", dpi = 600, compression = "lzw")

