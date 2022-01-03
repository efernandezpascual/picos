library(tidyverse); library(urca)

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
  geom_line(color = "dodgerblue") +
  geom_smooth(method = "lm", color = "dodgerblue4") +
  xlab("Time (1-h recording inverval)") +
  ylab("Trend (ºC)") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) -> f1; f1

### Save figure

ggsave(f1, file = "results/figures/temperature-changes.png", 
       path = NULL, scale = 1, width = 182, height = 182/3, units = "mm", dpi = 600)
# ggsave(f1, file = "results/figures/temperature-changes.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 127, height = 160, units = "mm", dpi = 600, compression = "lzw")

  