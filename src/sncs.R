library(tidyverse); library(tibbletime)

read.csv("data/focal-plot-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) -> temperatures

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
            GDD = sum(GDD)) -> # GDD per year
  micro

write.csv(micro, "results/microtemps.csv", row.names = FALSE)

read.csv("data/ibutton-plot-species.csv")  -> species

micro %>%
  select(Plot, FDD, Snow, bio01) %>%
  merge(species) %>%
  group_by(Taxon) %>%
  summarise(n = length(Plot),
            AnnualTemperature = weighted.mean(bio01, Cover),
            Snow = weighted.mean(Snow, Cover),
            FDD = weighted.mean(FDD, Cover)) %>%
  arrange(FDD) %>%
  write.csv("results/species-snc.csv", row.names = FALSE)
