library(tidyverse); library(urca)

read.csv("data/focal-plot-temperatures.csv") -> temperatures

# # El Cable
# 
# temperatures %>% 
#   filter(Site == "El Cable") %>% 
#   pull(Temperature) %>%
#   ts(start = c(2009, 1), frequency = 24 * 365) -> # Convert to time series object
#   timeseries
# 
# fit <- stl(timeseries, s.window = "period") # Better than `decompose` https://stats.stackexchange.com/questions/85987/which-is-better-stl-or-decompose
# plot(fit)
# 
# tseries::adf.test(timeseries) # Time series IS stationary
# Kendall::SeasonalMannKendall(timeseries) # There is a trend in the data https://www.statology.org/mann-kendall-trend-test-r/
# 
# # Hoyo Sin Tierra
# 
# temperatures %>% 
#   filter(Site == "Hoyo Sin Tierra") %>% 
#   pull(Temperature) %>%
#   ts(start = c(2009, 1), frequency = 24 * 365) -> # Convert to time series object
#   timeseries
# 
# fit <- stl(timeseries, s.window = "period") # Better than `decompose` https://stats.stackexchange.com/questions/85987/which-is-better-stl-or-decompose
# plot(fit)
# 
# tseries::adf.test(timeseries) # Time series IS stationary
# Kendall::SeasonalMannKendall(timeseries) # There is a trend in the data https://www.statology.org/mann-kendall-trend-test-r/
# 
# # Cabaña Verónica
# 
# temperatures %>% 
#   filter(Site == "Cabaña Verónica") %>% 
#   pull(Temperature) %>%
#   ts(start = c(2009, 1), frequency = 24 * 365) -> # Convert to time series object
#   timeseries
# 
# fit <- stl(timeseries, s.window = "period") # Better than `decompose` https://stats.stackexchange.com/questions/85987/which-is-better-stl-or-decompose
# plot(fit)
# 
# tseries::adf.test(timeseries) # Time series IS stationary
# Kendall::SeasonalMannKendall(timeseries) # There is a trend in the data https://www.statology.org/mann-kendall-trend-test-r/
# 
# # Horcados Rojos
# 
# temperatures %>% 
#   filter(Site == "Horcados Rojos") %>% 
#   pull(Temperature) %>%
#   ts(start = c(2009, 1), frequency = 24 * 365) -> # Convert to time series object
#   timeseries
# 
# fit <- stl(timeseries, s.window = "period") # Better than `decompose` https://stats.stackexchange.com/questions/85987/which-is-better-stl-or-decompose
# plot(fit)
# 
# tseries::adf.test(timeseries) # Time series IS stationary
# Kendall::SeasonalMannKendall(timeseries) # There is a trend in the data https://www.statology.org/mann-kendall-trend-test-r/
# 
# # Los Boches
# 
# temperatures %>% 
#   filter(Site == "Los Boches") %>% 
#   pull(Temperature) %>%
#   ts(start = c(2009, 1), frequency = 24 * 365) -> # Convert to time series object
#   timeseries
# 
# fit <- stl(timeseries, s.window = "period") # Better than `decompose` https://stats.stackexchange.com/questions/85987/which-is-better-stl-or-decompose
# plot(fit)
# 
# tseries::adf.test(timeseries) # Time series IS stationary
# Kendall::SeasonalMannKendall(timeseries) # There is a trend in the data https://www.statology.org/mann-kendall-trend-test-r/
# 
# # Jou Sin Tierra
# 
# temperatures %>% 
#   filter(Site == "Jou Sin Tierra") %>% 
#   pull(Temperature) %>%
#   ts(start = c(2009, 1), frequency = 24 * 365) -> # Convert to time series object
#   timeseries
# 
# fit <- stl(timeseries, s.window = "period") # Better than `decompose` https://stats.stackexchange.com/questions/85987/which-is-better-stl-or-decompose
# plot(fit)
# 
# tseries::adf.test(timeseries) # Time series IS stationary
# Kendall::SeasonalMannKendall(timeseries) # There is a trend in the data https://www.statology.org/mann-kendall-trend-test-r/
# 
# # Garganta
# 
# temperatures %>% 
#   filter(Site == "Garganta") %>% 
#   pull(Temperature) %>%
#   ts(start = c(2009, 1), frequency = 24 * 365) -> # Convert to time series object
#   timeseries
# 
# fit <- stl(timeseries, s.window = "period") # Better than `decompose` https://stats.stackexchange.com/questions/85987/which-is-better-stl-or-decompose
# plot(fit)
# 
# tseries::adf.test(timeseries) # Time series IS stationary
# Kendall::SeasonalMannKendall(timeseries) # There is a trend in the data https://www.statology.org/mann-kendall-trend-test-r/
# 
# # Urriellu
# 
# temperatures %>% 
#   filter(Site == "Urriellu") %>% 
#   pull(Temperature) %>%
#   ts(start = c(2009, 1), frequency = 24 * 365) -> # Convert to time series object
#   timeseries
# 
# fit <- stl(timeseries, s.window = "period") # Better than `decompose` https://stats.stackexchange.com/questions/85987/which-is-better-stl-or-decompose
# plot(fit)
# 
# tseries::adf.test(timeseries) # Time series IS stationary
# Kendall::SeasonalMannKendall(timeseries) # There is a trend in the data https://www.statology.org/mann-kendall-trend-test-r/

# For all sites

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

temperatures%>%
  filter(Site %in% c("Los Cazadores", "Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches")) %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  group_by(Site) %>%
  do(decomposer(.)) %>%
  mutate(Site = as.factor(Site)) %>%
  mutate(Site = fct_relevel(Site, "El Cable", "Hoyo Sin Tierra", "Cabaña Verónica",
                                    "Horcados Rojos", "Los Boches", "Hou Sin Tierri",
                                    "Los Cazadores", "Urriellu")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) -> trends

ggplot(trends, aes(Time, Temperature)) + 
  facet_wrap(~ Site, nrow = 2) + 
  geom_line(color = "firebrick") +
  labs(title = "10-year temperatures") +
  xlab("Time (h)") +
  ylab("Temperature (ºC)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        axis.title.y = element_text(size = 22, color = "black"),
        axis.title.x = element_text(size = 22, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 14, color = "black")) -> f1; f1

ggsave(f1, file = "results/f1.png", 
       path = NULL, scale = 1, width = 330, height = 185, units = "mm", dpi = 600)

trends %>%
  ggplot(aes(Time, Trend)) + 
  facet_wrap(~ Site, scales = "free", nrow = 2) + 
  geom_line(color = "dodgerblue") +
  geom_smooth(method = "lm", color = "dodgerblue4") +
  labs(title = "10-year trends") +
  xlab("Time (h)") +
  ylab("Trend (ºC)") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        axis.title.y = element_text(size = 22, color = "black"),
        axis.title.x = element_text(size = 22, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 14, color = "black")) -> f2; f2

ggsave(f2, file = "results/f2.png", 
       path = NULL, scale = 1, width = 330, height = 185, units = "mm", dpi = 600)

  