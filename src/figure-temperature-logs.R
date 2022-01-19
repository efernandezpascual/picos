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
  labs(title = "(A) Hourly logs, spatial survey (20 iButtons per site, Oct 2018 - Aug 2019)") +
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
  labs(title = "(B) Hourly logs, temporal survey (2009-2018)") +
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

### Decompose time series

decomposer <- function(x) 
{
  x %>% 
    pull(Temperature) %>%
    ts(start = c(2009, 1), frequency = 24 * 365) -> timeseries
  
  stl(timeseries, s.window = "period") -> fit
  
  cbind(x$Time, as.data.frame(timeseries), as.data.frame(fit$time.series)) -> df
  
  colnames(df) <- c("Time", "Temperature", "Seasonal", "Trend", "Remainder")
  
  df
}

read.csv("data/temporal-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  group_by(Site) %>%
  do(decomposer(.)) %>%
  mutate(Site = as.factor(Site)) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) -> trends

### Plot trends

trends %>%
  ggplot(aes(Time, Trend)) + 
  facet_wrap(~ Site, nrow = 1) + 
  geom_smooth(method = "lm", color = "black", size = .5, linetype = "dashed") +
  geom_line(color = "darkorange") +
  labs(title = "(C) 10-year trend decomposition, temporal survey (2009-2018)") +
  xlab("Time (1-h recording inverval)") +
  ylab("Temperature (ºC)") +
  ggthemes::theme_tufte()  +
  theme(strip.background = element_rect(colour = "grey96", fill = "grey96"),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"))-> f3; f3

### Combine panels

cowplot::plot_grid(f1, f2, f3, ncol = 1) -> fig

### Save figure

ggsave(fig, file = "results/figures/temperature-logs.png", 
       path = NULL, scale = 1, width = 182, height = 182, units = "mm", dpi = 600)
# ggsave(fig, file = "results/figures/temperature-logs.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182, height = 160, units = "mm", dpi = 600, compression = "lzw")

