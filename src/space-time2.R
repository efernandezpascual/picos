library(tidyverse)

### Temporal survey

read.csv("data/temporal-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  filter(Hour %in% c(0, 4, 8, 12, 16, 20)) %>% # Keep same recording hours as ibuttons
  filter(! Month %in% 9) %>% # Remove missing September days
  filter(! (Month %in% 8 & Day > 6)) %>% # Remove missing August days
  filter(! (Month %in% 10 & Day < 3)) %>% # Remove missing October days %>%
  dplyr::select(-c(Month, Day, Hour)) %>%
  mutate(Survey = "Temporal") -> temporal

### Spatial survey

read.csv("data/spatial-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  merge(read.csv("data/spatial-survey-header.csv"))%>%
  mutate(Survey = "Spatial") %>%
  select(Site, Time, Temperature, Survey) -> spatial

## Bind

rbind(temporal, spatial) -> temps

temps %>%  
  mutate(Site = fct_relevel(Site,
                                     "Los Cazadores", "Los Boches",
                                     "Hou Sin Tierri", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Hou Sin Tierri" = "Hou Sin Tierri")) %>%
  ggplot(aes(Survey, Temperature, color = Survey, fill = Survey)) + 
  geom_jitter(shape = 16, position = position_jitter(0.15), alpha = 0.1) +
  geom_violin(alpha = 0.05, draw_quantiles = c(0.25, 0.5, 0.75), color = "black") +
  facet_wrap(~ Site, nrow = 1)  +
  xlab("Survey") +
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
        axis.text.y = element_text(size = 11, color = "black")) -> F3B; F3B

ggsave(F3B, file = "results/figures/F3B - space vs time.png", 
       path = NULL, scale = 1, width = 182, height = 140, units = "mm", dpi = 600)

read.csv("results/supplement/S1 - Bioclimatic indices filtered clara.csv") %>%
  gather(Trait, Value, bio1:GDD) %>%
  filter(Trait %in% c("GDD", "FDD")) %>%
  group_by(Trait, Site, Survey) %>%
  summarise(m = mean(Value), s = sd(Value), n = length(Value),
            error = 1.96 * (s/sqrt(n)),
            lower = m-error, upper = m+error)

# combine plots Fig 3
library(ggpubr)
ggarrange(F3A, F3B, nrow = 2, heights = c(1.5,3))->F3;F3

### Save figure

ggsave(F3, file = "results/figures/F3.png", 
       path = NULL, scale = 1, width = 182, height = 160, units = "mm", dpi = 600)
