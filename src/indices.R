library(tidyverse); library(tibbletime)

merge(read.csv("data/logs.csv"),
      read.csv("data/loggers.csv")) %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  as_tbl_time(index = Time) %>%
  arrange(Brand, Site, Time) -> logs

# End and start dates

logs %>%
  filter(Brand == "GeoPrecision") %>%
  group_by(Site) %>%
  summarise(Min = min(Time), Max = max(Time))

logs %>%
  filter(Brand == "EMS") %>%
  group_by(Site) %>%
  summarise(Min = min(Time), Max = max(Time))

logs %>%
  filter(Brand == "iButtons") %>%
  group_by(Site) %>%
  summarise(Min = min(Time), Max = max(Time))

# Compare temperature GeoPrecision vs. EMS

logs %>%
  filter(Brand %in% c("GeoPrecision", "EMS")) %>%
  filter(Site == c("Urriellu")) %>%
  filter(Time > "2017-01-01") %>%
  ggplot(aes(Time, Temperature, color = Brand)) +
  geom_line() + 
  facet_wrap(~ Site) # Huge differences in Hoyo Sin Tierra

# Indices for GeoPrecision by complete year

logs %>%
  filter(Brand == "GeoPrecision" | (Brand == "EMS" & Site == "Urriellu")) %>% # Use EMS to fill in missing GeoPrecision data at Urriellu
  arrange(Time) %>%
  filter_time("2009" ~ "2018") %>%
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
  select(Site:bio02, bio03, bio04:GDD) -> yearly # Dataset by year

yearly %>%
  gather(Trait, Value, bio01:GDD) %>%
  ggplot(aes(Year, Value, color = Site)) +
  facet_wrap(~ Trait, scales = "free", ncol = 3) +
  geom_line() +
  theme(legend.position = "top") +
  geom_smooth(method = "lm") -> fig1

ggsave(filename = "results/Fig1.pdf", plot = fig1,
       path = NULL, scale = 1, width = 200, height = 287, units = "mm", dpi = 600)

# Indices for GeoPrecision by site

yearly %>%
  group_by(Site) %>%
  summarise(bio01 = mean(bio01), # Annual Mean Temperature
            bio02 = mean(bio02), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio03 = mean(bio03), # Isothermality (BIO2/BIO7) (×100)
            bio04 = mean(bio04), # Temperature Seasonality (standard deviation ×100)
            bio05 = mean(bio05), # Max Temperature of Warmest Month
            bio06 = mean(bio06), # Min Temperature of Coldest Month
            bio07 = mean(bio07), # Temperature Annual Range (BIO5-BIO6)
            Snow = mean(Snow), # Days of snow per year
            FreezeThaw = mean(FreezeThaw), # Days with freeze-thaw cycles per year
            FDD = mean(FDD), # FDD per year
            GDD = mean(GDD)) -> # GDD per year
  geoprecision

# Indices for iButtons

logs %>%
  filter(Brand == "iButtons") %>% 
  group_by(ID, Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 1, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(ID, Site, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(ID, Site) %>%
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
  select(Site:bio02, bio03, bio04:GDD) -> ibuttons # Dataset by year

# Save

geoprecision %>%
  mutate(ID = "GeoPrecision") %>%
  select(ID, Site:GDD) %>%
  rbind(ibuttons) %>%
  write.csv("results/indices.csv", row.names = FALSE)
