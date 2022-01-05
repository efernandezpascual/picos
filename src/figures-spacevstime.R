library(tidyverse); library(tibbletime); library(vegan)

### Indices for GeoPrecision by complete year

read.csv("data/temporal-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
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

### Indices for iButtons

read.csv("data/spatial-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  merge(read.csv("data/spatial-survey-header.csv")) %>%
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

### Merge dataframes

dfMicro %>%
  merge(dfTime)  %>% 
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) %>%
  mutate(Trait = fct_recode(Trait, "Annual mean" = "bio01",
                           "Warmest month max" = "bio05",
                           "Coldest month min" = "bio06")) %>%
  gather(Dimension, Value, Micro:Time) %>%
  mutate(Dimension = fct_recode(Dimension, "Space" = "Micro")) %>%
  group_by(Trait, Dimension) %>%
  summarise(M = mean(Value), SD = sd(Value), n = length(Value), SE = SD / sqrt(n)) -> delta1

### Sorensen

sor <- function(x)  
{x %>%
    select(Plot, Taxon, Cover) %>%
    spread(Taxon, Cover, fill = 0) %>%
    column_to_rownames(var = "Plot") %>%
    vegdist(method = "bray", binary = TRUE) %>%
    mean %>%
    data.frame}

read.csv("data/spatial-survey-species.csv") %>%
  merge(read.csv("data/spatial-survey-header.csv")) %>%
  group_by(Site) %>%
  do(sor(.)) %>%
  rename(Space = ".") -> inspace

read.csv("data/temporal-survey-species.csv") %>%
  merge(read.csv("data/temporal-survey-header.csv")) %>%
  separate(Plot, into = c("Plot2", "Year"), sep = "-", remove = FALSE) %>%
  group_by(Site, Plot2) %>%
  do(sor(.)) %>%
  rename(Time = ".") %>%
  group_by(Site) %>%
  summarise(Time = mean(Time)) -> intime

merge(intime, inspace) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) %>%
  gather(Dimension, Value, Time:Space) %>%
  group_by(Dimension) %>%
  summarise(M = mean(Value), SD = sd(Value), n = length(Value), SE = SD / sqrt(n)) %>%
  mutate(Trait = "Sorensen") -> delta2

### Final dataframe

rbind(delta1, delta2) -> delta
  
### Figure temperature

delta %>%
  filter(Trait %in% c("Annual mean", "FDD", "GDD", "Snow", "Sorensen")) %>%
  mutate(Trait = fct_recode(Trait, "MAT max difference (ºC)" = "Annual mean",
                            "FDD max difference (cumulative ºC)" = "FDD",
                            "GDD max difference (cumulative ºC)" = "GDD",
                            "Snow length max difference (days)" = "Snow",
                            "Sørensen dissimilarity index" = "Sorensen")) %>%
  ggplot(aes(Dimension, M, fill = Dimension)) +
  facet_wrap(~ Trait, scales = "free_y", nrow = 1, switch = "y") +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = M - SD, ymax = M + SD), width=.2,
                position=position_dodge(.9)) +
  xlab("Sites") +
  ylab("Max difference (absolute values)") +
  ggthemes::theme_tufte() +
  scale_fill_manual(values = c("purple", "gold")) +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        strip.text = element_text(size = 12),
        strip.placement = "ouside",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) -> f1; f1

### Join

ggsave(f1, file = "results/figures/spacevstime.png", 
       path = NULL, scale = 1, width = 182, height = 90, units = "mm", dpi = 600)



