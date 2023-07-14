library(tidyverse); library(urca); library(raster); library(tibbletime); library(lubridate)
Sys.setlocale("LC_TIME", "English")

### iButton logs
graph_names <- as_labeller (c( "Los Cazadores" ="Los Cazadores \n (Snowbed)", 
                               "Los Boches" = "Los Boches \n (Snowbed)",
                               "Ḥou Sin Tierri" = "Ḥou Sin Tierri \n (Fellfield)", 
                               "Hoyo Sin Tierra" = "Hoyo Sin Tierra \n (Fellfield)"))

read.csv("data/spatial-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  merge(read.csv("data/spatial-survey-header.csv")) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Los Boches",
                            "Hou Sin Tierri","Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) %>%
  ggplot(aes(Time, Temperature, color = Plot)) + 
  facet_wrap(~ Site, nrow = 1, labeller = graph_names) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  labs(title = "(B) Spatial survey (20 iButtons per site, Oct 2018 - Aug 2019)") +
  xlab("Time (4-h recording inverval)") +
  ylab("Temperature (ºC)") +
  ggthemes::theme_tufte() +
  coord_cartesian(ylim = c(-10, 30)) +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 11, color = "black")) -> F2B;F2B

### Geoprecision logs
graph_names <- as_labeller (c( "Los Cazadores" ="Los Cazadores \n (Snowbed)", 
                               "Los Boches" = "Los Boches \n (Snowbed)",
                               "Ḥou Sin Tierri" = "Ḥou Sin Tierri \n (Fellfield)", 
                               "Hoyo Sin Tierra" = "Hoyo Sin Tierra \n (Fellfield)"))

read.csv("data/temporal-survey-temperatures.csv", sep= ";") %>% 
  mutate(Time = as.POSIXct(Time, tz = "UTC", format = "%d/%m/%Y %H:%M")) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Los Boches",
                            "Hou Sin Tierri","Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) %>%
  ggplot(aes(Time, Temperature)) + 
  facet_wrap(~ Site, nrow = 1, labeller = graph_names) + 
  geom_line(color = "red") +
  labs(title = "(A) Temporal survey (2009-2018)") +
  xlab("Time (1-h recording inverval)") +
  ylab("Temperature (ºC)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  coord_cartesian(ylim = c(-10, 30)) +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 11, color = "black")) -> F2A; F2A

### Combine panels

cowplot::plot_grid(F2A, F2B, ncol = 1) -> F2; F2

### Save figure

ggsave(F2, file = "results/figures/F2.png", 
       path = NULL, scale = 1, width = 182, height = 140, units = "mm", dpi = 600)
# ggsave(fig, file = "results/figures/temperature-logs.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182, height = 160, units = "mm", dpi = 600, compression = "lzw")

