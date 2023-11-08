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

read.csv("data/temporal-survey-temperatures.csv", sep =";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  group_by(Site) %>%
  do(decomposer(.)) %>%
  mutate(Site = as.factor(Site)) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Los Boches",
                            "Hou Sin Tierri","Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri"))  -> trends

### Plot trends
graph_names <- as_labeller (c( "Los Cazadores" ="Los Cazadores \n (Snowbed)", 
                               "Los Boches" = "Los Boches \n (Snowbed)",
                               "Ḥou Sin Tierri" = "Ḥou Sin Tierri \n (Fellfield)", 
                               "Hoyo Sin Tierra" = "Hoyo Sin Tierra \n (Fellfield)"))

trends %>%
  ggplot(aes(Time, Trend)) + 
  facet_wrap(~ Site, nrow = 1, scales = "fixed", labeller = graph_names) + 
  geom_line(color = "firebrick1", alpha = 0.3) +
  geom_smooth(method = "lm", color = "firebrick4") +
  labs(title = "(A) Temporal trends in soil temperature") +
  xlab("Time (1-h recording inverval)") +
  ylab("Trend (ºC)") +
  ggthemes::theme_tufte() +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 7, color = "black"),
        axis.text.y = element_text(size = 11, color = "black")) -> F3A; F3A

# Slopes

glms <- function(x) {
  lm(Trend ~ Time, data = x) -> m1
  broom::tidy(m1)
}

trends %>%
  mutate(Time2 = Time - years(2009)) %>%
  group_by(Site) %>%
  do(glms(.))

trends %>%
  group_by(Site, Year = lubridate::floor_date(Time, "year")) %>%
  summarise(n = mean(Temperature)) %>%
  group_by(Site) %>%
  mutate(cs = n - lag(n)) %>%  
  summarise(n = mean(cs, na.rm = TRUE))

trends %>%
  group_by(Site, Year = lubridate::floor_date(Time, "year")) %>%
  summarise(n = mean(Temperature)) %>%
  group_by(Site) %>%
  summarise(minimo = min(n), maximo = max(n), diferencia = max(n) - min(n))

### Save figure

ggsave(F3A, file = "results/figures/clara changes/F3A (3).png", 
       path = NULL, scale = 1, width = 182, height = 182/3, units = "mm", dpi = 600)
# ggsave(f1, file = "results/figures/temperature-changes.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 127, height = 160, units = "mm", dpi = 600, compression = "lzw")

  