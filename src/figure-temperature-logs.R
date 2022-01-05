library(tidyverse); library(urca); library(raster); library(tibbletime); library(lubridate)
Sys.setlocale("LC_TIME", "English")

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
  labs(title = "(C) Hourly logs, temporal survey (2009-2018)") +
  xlab("Time (1-h recording inverval)") +
  ylab("Temperature (ºC)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
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
  labs(title = "(D) Hourly logs, spatial survey (20 iButtons per site, Oct 2018 - Aug 2019)") +
  xlab("Time (4-h recording inverval)") +
  ylab("Temperature (ºC)") +
  ggthemes::theme_tufte() +
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
  labs(title = "(B) 10-year trend decomposition, temporal survey (2009-2018)") +
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

### Climographs

read.csv("data/temporal-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  as_tbl_time(index = Time) %>%
  arrange(Time) %>%
  filter_time("2009" ~ "2018") %>%
  mutate(m = month(Time)) %>%
  group_by(Site, m) %>%
  summarise(T = mean(Temperature)) -> ts # Monthly average temperature

read.csv("data/temporal-survey-wp.csv") %>%
  mutate(m = month(Time)) %>%
  group_by(Site, m) %>%
  summarise(WP = mean(WP)) -> wp # Monthly average water potential

merge(ts, wp) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) %>%
  ggplot(aes(x = m)) +
  facet_wrap(~ Site, nrow = 1) +
  geom_bar(aes(y = -WP*10), stat = "identity", position = "dodge", fill = "skyblue", size = 1.1) +
  geom_line(aes(y = T), color = "lightcoral", size = .5) +
  geom_point(aes(y = T), color = "lightcoral", size = .7) +
  scale_y_continuous(sec.axis = sec_axis( trans = ~./10, name = "Water potential (-MPa)")) +
  ggthemes::theme_tufte() +
  theme(strip.background = element_rect(colour = "grey96", fill = "grey96"),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Calendar month (January to December)", y = "Temperature (ºC)") +
  labs(title = "(A) Monthly climatic averages, temporal survey (2009-2018)") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), ) -> f4; f4

### Combine panels

cowplot::plot_grid(f4, f3, f1, f2, ncol = 1) -> fig

### Save figure

ggsave(fig, file = "results/figures/temperature-logs.png", 
       path = NULL, scale = 1, width = 182, height = 240, units = "mm", dpi = 600)
# ggsave(fig, file = "results/figures/temperature-logs.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182, height = 160, units = "mm", dpi = 600, compression = "lzw")

