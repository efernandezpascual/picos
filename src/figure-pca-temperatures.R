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
            FDD = abs(sum(FDD)), # FDD per year
            GDD = sum(GDD)) %>%
  mutate(Survey = "Spatial") -> # GDD per year
  dfSpace

### Merge

rbind(dfSpace, dfTime) -> bioclim
bioclim %>% filter(Site == "Los Cazadores") -> bioclim1
bioclim %>% filter(Site == "Hou Sin Tierri") -> bioclim2
bioclim %>% filter(Site == "Los Boches") -> bioclim3
bioclim %>% filter(Site == "Hoyo Sin Tierra") -> bioclim4

### PCA 1

bioclim1[, 3:7] %>%
  FactoMineR::PCA() -> pca1

cbind((bioclim1 %>%  dplyr::select(Plot, Site, Survey)), data.frame(pca1$ind$coord[, 1:2])) -> pcaInds1

pca1$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Site = "Los Cazadores") -> pcaVars1

### PCA 2

bioclim2[, 3:7] %>%
  FactoMineR::PCA() -> pca2

cbind((bioclim2 %>%  dplyr::select(Plot, Site, Survey)), data.frame(pca2$ind$coord[, 1:2])) -> pcaInds2

pca2$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Site = "Hou Sin Tierri") -> pcaVars2

### PCA 3

bioclim3[, 3:7] %>%
  FactoMineR::PCA() -> pca3

cbind((bioclim3 %>%  dplyr::select(Plot, Site, Survey)), data.frame(pca3$ind$coord[, 1:2])) -> pcaInds3

pca3$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Site = "Los Boches") -> pcaVars3

### PCA 4

bioclim4[, 3:7] %>%
  FactoMineR::PCA() -> pca4

cbind((bioclim4 %>%  dplyr::select(Plot, Site, Survey)), data.frame(pca4$ind$coord[, 1:2])) -> pcaInds4

pca4$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Site = "Hoyo Sin Tierra") -> pcaVars4

### Merge PCA results

rbind(pcaInds1, pcaInds2, pcaInds3, pcaInds4) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) -> pcaInds

rbind(pcaVars1, pcaVars2, pcaVars3, pcaVars4) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) -> pcaVars

### Plot PCA

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = 3*Dim.1, yend = 3*Dim.2)) +
  ggthemes::theme_tufte() + 
  facet_wrap(~ Site) + 
  geom_text(aes(label = Plot, color = Survey), size = 4) +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        # legend.text = element_text(size = 12, color = "black"),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black"),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm")) +
  scale_x_continuous(name = "PC1") + 
  scale_y_continuous(name = "PC2") +
  geom_label(data = pcaVars, aes(x = 3*Dim.1, y = 3*Dim.2, label = Variable),  show.legend = FALSE, size = 4) +
  scale_color_manual(values = c("goldenrod", "forestgreen",  "royalblue", "darkorchid")) +
  scale_fill_manual(values = c("goldenrod", "forestgreen",  "royalblue", "darkorchid")) -> f1; f1

### Save figure

ggsave(f1, file = "results/figures/pca-temperatures.png", 
       path = NULL, scale = 1, width = 182, height = 182, units = "mm", dpi = 600)
# ggsave(f1, file = "results/figures/pca-temperatures.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182, height = 182, units = "mm", dpi = 600, compression = "lzw")
