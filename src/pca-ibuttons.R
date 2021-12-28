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
  summarise(`Annual\ntemperature` = mean(T), # Annual Mean Temperature
            `Diurnal range` = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            # bio04 = sd(T) * 100, # Temperature Seasonality (standard deviation ×100)
            # bio05 = max(X), # Max Temperature of Warmest Month
            # bio06 = min(N), # Min Temperature of Coldest Month
            `Annual range` = max(X) - min(N), # Temperature Annual Range (BIO5-BIO6)
            # bio03 = (bio02 / bio07) * 100, # Isothermality (BIO2/BIO7) (×100)
            # Snow = sum(Snow), # Days of snow per year
            # FreezeThaw = sum(FreezeThaw), # Days with freeze-thaw cycles per year
            `Freezing\ndegrees-day` = sum(FDD), # FDD per year
            `Growing\ndegrees-day` = sum(GDD)) -> # GDD per year
  micro

micro[, 3:7] %>%
  FactoMineR::PCA() -> pca

cbind((micro %>%  select(Plot, Site)), data.frame(pca$ind$coord[, 1:2])) %>%  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) -> pcaInds

pca$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") -> pcaVars

cent <- aggregate(cbind(Dim.1, Dim.2) ~ Site, data = pcaInds, FUN = mean)
segs <- merge(pcaInds, setNames(cent, c("Site", "oDCA1", "oDCA2")), by = "Site", sort = FALSE)

## Plot PCA

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = 3*Dim.1, yend = 3*Dim.2)) +
  ggthemes::theme_tufte() + 
  geom_segment(data = segs, mapping = aes(xend = oDCA1, yend = oDCA2, color = Site), show.legend = F, alpha = 0.5) +
  geom_label(data = cent, size = 4, aes(color = Site, label = Site), show.legend = T) + 
  
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        axis.title.y = element_text(size = 22, color = "black"),
        axis.title.x = element_text(size = 22, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 14, color = "black")) +
  labs(title = "PCA soil temperatures (n = 80 iButtons)") +
  scale_x_continuous(name = paste("Axis 1 (", round(pca$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pca$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  geom_label(data = pcaVars, aes(x = 3*Dim.1, y = 3*Dim.2, label = Variable),  show.legend = FALSE, size = 4) +
  scale_color_manual(values = c("forestgreen",  "goldenrod", "darkorchid", "royalblue")) -> f1

## Export

ggsave(f1, file = "results/PCA soils.png", 
       path = NULL, scale = 1, width = 330, height = 185, units = "mm", dpi = 600)

