library(tidyverse)

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
  group_by(Plot, Site, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N), # Daily mean, max, min
            Snow = sum(Snow), # Snow days per month
            FreezeThaw = sum(FreezeThaw), # Freeze-thaw days per month
            FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Plot, Site) %>%
  summarise(`Annual\ntemperature` = mean(T), # Annual Mean Temperature
            `Diurnal range` = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            `Annual range` = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            `Freezing\ndegrees-day` = abs(sum(FDD)), # FDD per year
            `Growing\ndegrees-day` = sum(GDD)) -> # GDD per year
  micro

### PCA

micro[, 3:7] %>%
  FactoMineR::PCA() -> pca

cbind((micro %>%  dplyr::select(Plot, Site)), data.frame(pca$ind$coord[, 1:2])) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) -> pcaInds

pca$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") -> pcaVars

### Plot PCA

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = 3*Dim.1, yend = 3*Dim.2)) +
  ggthemes::theme_tufte() + 
  geom_point(aes(fill = Site), size = 6, shape = 22) +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        # legend.text = element_text(size = 12, color = "black"),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black"),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm")) +
  scale_x_continuous(name = paste("Axis 1 (", round(pca$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pca$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  geom_label(data = pcaVars, aes(x = 3*Dim.1, y = 3*Dim.2, label = Variable),  show.legend = FALSE, size = 4) +
  scale_color_manual(values = c("goldenrod", "forestgreen",  "royalblue", "darkorchid")) +
  scale_fill_manual(values = c("goldenrod", "forestgreen",  "royalblue", "darkorchid")) -> f1; f1

### Save figure

ggsave(f1, file = "results/figures/pca-temperatures.png", 
       path = NULL, scale = 1, width = 127, height = 118, units = "mm", dpi = 600)
# ggsave(f1, file = "results/figures/pca-temperatures.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182, height = 182, units = "mm", dpi = 600, compression = "lzw")