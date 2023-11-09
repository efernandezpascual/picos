library(tidyverse)
#### Edu's script GDD T>1 #####
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
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            Snw = sum(Snow),
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD)) %>%
  mutate(Year = as.character(lubridate::year(Year))) %>% 
  rename(Plot = Year) %>%
  mutate(Survey = "Temporal") -> 
  dfTime


### Spatial survey data

read.csv("data/spatial-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  merge(read.csv("data/spatial-survey-header.csv")) %>%
  group_by(Plot, Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 1, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(Site, Plot, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Site, Plot) %>%
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            Snw = sum(Snow),
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD)) %>%
  mutate(Survey = "Spatial") -> # GDD per year
  dfSpace

### Merge

rbind(dfSpace, dfTime) -> bioclim

bioclim %>% write.csv("results/supplement/S1 - Bioclimatic indices.csv", row.names = FALSE)

bioclim %>% filter(Survey == "Temporal") %>%
  gather(Trait, Value, bio1:GDD) %>%
  group_by(Trait, Site) %>%
  summarise(m = mean(Value)) %>%
  data.frame

read.csv("data/temporal-survey-temperatures.csv") %>%
  group_by(Site) %>%
  filter(Temperature == max(Temperature) | Temperature == min(Temperature))


##### modification clara GDD T>5 #####
### Temporal + Spatial survey data homogenization with ibuttons ####
read.csv("data/temporal-survey-temperatures.csv", sep = ";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  filter(Hour %in% c(0, 4, 8, 12, 16, 20)) %>% # Keep same recording hours as ibuttons
  filter(! Month %in% 9) %>% # Remove missing September days
  filter(! (Month %in% 8 & Day > 6)) %>% # Remove missing August days
  filter(! (Month %in% 10 & Day < 3)) %>% # Remove missing October days %>%
  dplyr::select(-c(Month, Day, Hour)) %>%
  group_by(Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 5, T, 0)) %>% # Growing degrees day per month 
  group_by(Site, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Site, Year = lubridate::floor_date(Month, "year")) %>%
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            Snw = sum(Snow),
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD)) %>%
  mutate(Year = as.character(lubridate::year(Year))) %>% 
  rename(Plot = Year) %>%
  mutate(Survey = "Temporal") -> 
  dfTime


### Spatial survey data

read.csv("data/spatial-survey-temperatures.csv", sep = ",") %>%
  #mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  merge(read.csv("data/spatial-survey-header.csv")) %>%
  group_by(Plot, Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 5, T, 0)) %>% # Growing degrees day per month 
  group_by(Site, Plot, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Site, Plot) %>%
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            Snw = sum(Snow),
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD)) %>%
  mutate(Survey = "Spatial") -> # GDD per year
  dfSpace

dfSpace %>% 
  gather(Trait, Value, bio1:GDD) %>%
  group_by(Trait) %>%
  summarise(m = mean(Value), min = min(Value), max = max(Value)) 

### Merge

rbind(dfSpace, dfTime) -> bioclim_cl

bioclim_cl %>% write.csv("results/supplement/S1 - Bioclimatic indices filtered clara.csv", row.names = FALSE)

### Table 1: bioclimatic description from temporal survey (no filtered) all year ####
read.csv("data/temporal-survey-temperatures.csv", sep = ";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  dplyr::select(-c(Month, Day, Hour)) %>%
  group_by(Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 5, T, 0)) %>% # Growing degrees day per month 
  group_by(Site, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Site, Year = lubridate::floor_date(Month, "year")) %>%
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            Snw = sum(Snow),
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD)) %>%
  mutate(Year = as.character(lubridate::year(Year))) %>% 
  rename(Plot = Year) %>%
  mutate(Survey = "Temporal") -> 
  dfTime

bioclim_cl %>% write.csv("results/supplement/S1 - Bioclimatic indices no filtered clara.csv", row.names = FALSE) 
rbind(dfSpace, dfTime) -> bioclim_cl

dfTime %>% 
  gather(Trait, Value, bio1:GDD) %>%
  group_by(Trait, Site) %>%
  summarise(m = mean(Value), min = min(Value), max = max(Value)) %>%
  spread(Site, m)
  data.frame

#for extreme scenarios  
dfTime %>% 
    gather(Trait, Value, bio1:GDD) %>%
    group_by(Trait) %>%
    summarise(m = mean(Value), min = min(Value), max = max(Value)) 

read.csv("data/temporal-survey-temperatures.csv", sep = ";") %>%
  group_by(Site) %>%
  filter(Temperature == max(Temperature) | Temperature == min(Temperature))%>%
  spread(Site, Temperature) %>%
  data.frame

### Table 1: bioclimatic description from temporal survey (no filtered) growing season only ####
# growing season determination
library(zoo)
read.csv("data/temporal-survey-temperatures.csv", sep = ";")%>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of time variable
  mutate(Time  = as.POSIXct(Time , tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time )) %>% 
  mutate(Month = lubridate::month(Time )) %>% 
  mutate(Day = lubridate::day(Time )) %>% 
  mutate(Hour = lubridate::hour(Time )) %>% 
  group_by(Site, Year, Day = lubridate::floor_date(Time , "day")) %>%
  summarise(T = mean(Temperature)) %>% # Daily mean
  mutate (data_start = first(Day), data_end = last(Day)) %>% # get starting and ending data points x year
  mutate(t5 = ifelse(T>=5, 1, 0))%>% # days with Tmean>= 5 =1
  mutate(length = rollsumr(t5, k = 3, fill= 0)) %>% #sum the 2 previous rows of t5 
  filter(! (length < 3)) %>% # Filter date  with 2 consecutive days with Tmean>5ºC (Körner limit) 
  group_by(Site, Year) %>% #separate x year
  summarise(GS_start = first (Day), GS_end = last(Day), # get the first and last day of the growing season
            data_start = first(data_start), data_end = last(data_end )) %>% 
  mutate (GS_length = difftime(GS_end , GS_start, units = "days")) %>%
  mutate (data_days = data_end - data_start) %>%
  mutate (site_year = paste (Site, Year))%>%
  data.frame-> grow 

grow %>%
  group_by(Site)%>%
  summarise(mean = mean(GS_length), min = min(GS_length), max = max(GS_length))

write.csv(grow, "results/tables/growing_season.csv")

library (viridis)
x11()
str(grow)
grow %>%
  mutate (Site = as.factor(Site),
          Year = as.factor(Year),
          GS_length = as.numeric(GS_length)) %>%#, levels = c("2009", "2010", "2011", "2012", "2013", "2014",
          #                                  "2015", "2016", "2017", "2018"))) 
  filter (! (data_days <360)) %>% 
  ggplot(aes(x=Year, y=GS_length, color = Site, fill = Site)) +
  geom_bar( stat = "identity", position = position_dodge())  +
  #facet_wrap (~Year) + (~Site)
  scale_fill_viridis (discrete=TRUE) +
  scale_color_viridis (discrete = TRUE) +
  scale_x_discrete(breaks=grow$Year,labels=grow$Year) +
  labs( title = "Growing season length x site",x = "Year", y = " Growing season (days)") + #
  theme_classic(base_size = 14) +
  theme(plot.title = element_text (hjust = 0.5, size = 30),
        strip.text.x = element_text(face = "bold", size = 22),
        strip.text.y = element_text(size = 14, angle = 360),
        legend.position = "right",
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title.y = element_text (size=16), 
        axis.title.x = element_text (size=16), 
        axis.text.x = element_text (size =12))

# bioclimatic indices during growing season
read.csv("data/temporal-survey-temperatures.csv", sep= ";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>% #specify format of time variable
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time)) %>% 
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  #mutate(time= as.Date(time)) %>%
  mutate (site_year = paste (Site, Year))-> df
# function to filter for growing season 
str(grow)
GS_filter <-function (grow) {
  grow %>%
    pull (site_year)%>%
    unique() -> unic
  grow  %>%
    pull(GS_start) -> date1 #convierte columna en vector
  grow %>%
    pull(GS_end) -> date2
  df %>% 
    dplyr::mutate(Time = as.Date(Time)) %>%
    filter(site_year == unic) %>%
    filter(Time >= date1 & Time <= date2)  
}

read.csv("data/temporal-survey-temperatures.csv", sep= ";") %>%
  mutate(Time = strptime(as.character(Time), "%d/%m/%Y %H:%M"))%>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  mutate(Year = lubridate::year(Time)) %>%
  mutate(Month = lubridate::month(Time)) %>% 
  mutate(Day = lubridate::day(Time)) %>% 
  mutate(Hour = lubridate::hour(Time)) %>% 
  mutate (site_year = paste (Site, Year)) %>%
  dplyr::select(-c(Month, Day, Hour)) %>%
  merge(grow) %>%
  filter (! (data_days <300)) %>%
  group_by (Site, Year)%>%
  do (GS_filter(.)) %>% 
  group_by(Site, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  #mutate(Snow = ifelse(X < 0.5 & N > -0.5, 1, 0)) %>% # Day is snow day or not
  #mutate(FreezeThaw = ifelse(X > 0.5 & N < -0.5, 1, 0)) %>% # Day with freeze-thaw cycles
  #mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  #mutate(GDD = ifelse(T >= 5, T, 0)) %>% # Growing degrees day per month 
  group_by(Site, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N))%>%  # Daily mean, max, min
            #Snow = sum(Snow), # Snow days per month
            #FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            #FDD = sum(FDD), # FDD per month
            #GDD = sum(GDD))  # GDD per month
  group_by(Site, Year = lubridate::floor_date(Month, "year")) %>%
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N)) %>%#, # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            #bio7 = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            #Snw = sum(Snow),
            #FDD = abs(sum(FDD)), # FDD per year
            #GDD = sum(GDD)) 
  mutate(Year = as.character(lubridate::year(Year))) %>% 
  rename(Plot = Year) %>%
  mutate(Survey = "Temporal") -> 
  dfTime

dfTime %>% 
  gather(Trait, Value, bio1:bio2) %>%
  group_by(Trait, Site) %>%
  summarise(m = mean(Value)) %>%
  spread(Site, m)
data.frame


