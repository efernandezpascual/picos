library(tidyverse); library(tibbletime)

read.csv("data/focal-plot-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) -> temperatures

# Indices for GeoPrecision by complete year

temperatures %>%
  group_by(Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  # mutate(Snow = ifelse((X - N) <= 0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 1, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(Site, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Site, Year = lubridate::floor_date(Month, "year")) %>%
  summarise(bio01 = mean(T), # Annual Mean Temperature
            bio02 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio04 = sd(T) * 100, # Temperature Seasonality (standard deviation ×100)
            bio05 = max(X), # Max Temperature of Warmest Month
            bio06 = min(N), # Min Temperature of Coldest Month
            bio07 = bio05 - bio06, # Temperature Annual Range (BIO5-BIO6)
            bio03 = (bio02 / bio07) * 100, # Isothermality (BIO2/BIO7) (×100)
            Snow = sum(Snow), # Days of snow per year
            FreezeThaw = sum(FreezeThaw), # Days with freeze-thaw cycles per year
            FDD = sum(FDD), # FDD per year
            GDD = sum(GDD)) %>% # GDD per year
  mutate(Year = lubridate::year(Year)) %>% 
  select(Site:bio01, bio05:bio06, Snow, FDD:GDD) %>%
  gather(Trait, Value, bio01:GDD) %>%
  group_by(Site, Trait) %>%
  filter(Value == max(Value) | Value == min(Value)) %>%
  group_by(Site, Trait) %>%
  summarise(Time = max(Value) - min(Value)) -> dfTime

temperatures %>%
  group_by(Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  # mutate(Snow = ifelse((X - N) <= 0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 1, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(Site, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Site, Year = lubridate::floor_date(Month, "year")) %>%
  summarise(bio01 = mean(T), # Annual Mean Temperature
            bio02 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio04 = sd(T) * 100, # Temperature Seasonality (standard deviation ×100)
            bio05 = max(X), # Max Temperature of Warmest Month
            bio06 = min(N), # Min Temperature of Coldest Month
            bio07 = bio05 - bio06, # Temperature Annual Range (BIO5-BIO6)
            bio03 = (bio02 / bio07) * 100, # Isothermality (BIO2/BIO7) (×100)
            Snow = sum(Snow), # Days of snow per year
            FreezeThaw = sum(FreezeThaw), # Days with freeze-thaw cycles per year
            FDD = sum(FDD), # FDD per year
            GDD = sum(GDD)) %>% # GDD per year
  mutate(Year = lubridate::year(Year)) %>% 
  select(Site:bio01, bio05:bio06, Snow, FDD:GDD) %>%
  gather(Trait, Value, bio01:GDD) %>%
  group_by(Site, Trait) %>%
  summarise(Value = mean(Value)) -> dfA
  
temperatures %>%
    group_by(Site, Day = lubridate::floor_date(Time, "day")) %>%
    summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
    mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
    # mutate(Snow = ifelse((X - N) <= 0.5, 1, 0)) %>% # Day is snow day or not
    mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
    mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
    mutate(GDD = ifelse(T >= 1, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
    group_by(Site, Month = lubridate::floor_date(Day, "month")) %>%
    summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
              Snow = sum(Snow), # Snow days per month
              FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
              FDD = sum(FDD), # FDD per month
              GDD = sum(GDD)) %>% # GDD per month
    group_by(Site, Year = lubridate::floor_date(Month, "year")) %>%
    summarise(bio01 = mean(T), # Annual Mean Temperature
              bio02 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
              bio04 = sd(T) * 100, # Temperature Seasonality (standard deviation ×100)
              bio05 = max(X), # Max Temperature of Warmest Month
              bio06 = min(N), # Min Temperature of Coldest Month
              bio07 = bio05 - bio06, # Temperature Annual Range (BIO5-BIO6)
              bio03 = (bio02 / bio07) * 100, # Isothermality (BIO2/BIO7) (×100)
              Snow = sum(Snow), # Days of snow per year
              FreezeThaw = sum(FreezeThaw), # Days with freeze-thaw cycles per year
              FDD = sum(FDD), # FDD per year
              GDD = sum(GDD)) %>% # GDD per year
    mutate(Year = lubridate::year(Year)) %>% 
    select(Site:bio01, bio05:bio06, Snow, FDD:GDD) %>%
    gather(Trait, Value, bio01:GDD) %>%
    group_by(Site, Trait) %>%
    summarise(Value = mean(Value)) -> dfB
  
merge(dfA, dfB, by = "Trait") %>% 
    filter(Site.x != Site.y) %>%
    mutate(Macro = abs(Value.x - Value.y)) %>%
    group_by(Site.x, Trait) %>%
    filter(Macro == max(Macro)) %>%
    arrange(Site.x) %>%
    rename(Site = Site.x) %>%
    select(Site, Trait, Macro) -> dfMacro

# Indices for iButtons

read.csv("data/ibutton-plot-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  merge(read.csv("data/ibutton-plot-header.csv")) %>%
  group_by(Plot, Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 1, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(Plot, Site, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Plot, Site) %>%
  summarise(bio01 = mean(T), # Annual Mean Temperature
            bio02 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio04 = sd(T) * 100, # Temperature Seasonality (standard deviation ×100)
            bio05 = max(X), # Max Temperature of Warmest Month
            bio06 = min(N), # Min Temperature of Coldest Month
            bio07 = bio05 - bio06, # Temperature Annual Range (BIO5-BIO6)
            bio03 = (bio02 / bio07) * 100, # Isothermality (BIO2/BIO7) (×100)
            Snow = sum(Snow), # Days of snow per year
            FreezeThaw = sum(FreezeThaw), # Days with freeze-thaw cycles per year
            FDD = sum(FDD), # FDD per year
            GDD = sum(GDD)) %>% # GDD per year
  select(Site:bio01, bio05:bio06, Snow, FDD:GDD) %>%
  gather(Trait, Value, bio01:GDD) %>%
  group_by(Site, Trait) %>%
  filter(Value == max(Value) | Value == min(Value)) %>%
  group_by(Site, Trait) %>%
  summarise(Micro = max(Value) - min(Value)) -> dfMicro

dfMicro %>%
  merge(dfMacro) %>%
  merge(dfTime) %>%
  mutate(Site = fct_relevel(Site, "El Cable", "Hoyo Sin Tierra", "Cabaña Verónica",
                            "Horcados Rojos", "Los Boches", "Hou Sin Tierri",
                            "Los Cazadores", "Urriellu")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) %>%
  mutate(Trait = fct_recode(Trait, "Annual mean" = "bio01",
                           "Warmest month max" = "bio05",
                           "Coldest month min" = "bio06")) %>%
  gather(Dimension, Value, Micro:Time) %>%
  filter(Dimension != "Macro") %>%
  mutate(Dimension = fct_recode(Dimension, "Space" = "Micro")) %>%
  ggplot(aes(Site, Value, group = Dimension, color = Dimension)) +
  facet_wrap(~ Trait, scales = "free_y") +
  geom_line() +
  geom_point() +
  labs(title = "Temperature variation: in space (iButtons) vs. time (10 years)") +
  xlab("Sites") +
  ylab("Max difference (absolute values)") +
  ggthemes::theme_tufte() +
  scale_color_manual(values = c("purple", "gold")) + 
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        axis.title.y = element_text(size = 22, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 14, color = "black", face = "italic", angle = 50, hjust=1)) -> f4; f4

ggsave(f4, file = "results/f4.png", 
       path = NULL, scale = 1, width = 330, height = 185, units = "mm", dpi = 600)



