library(tidyverse)

read.csv("results/numerical/indices.csv") -> bioclim
bioclim %>% filter(Site == "Los Cazadores") -> bioclim1
bioclim %>% filter(Site == "Hou Sin Tierri") -> bioclim2
bioclim %>% filter(Site == "Los Boches") -> bioclim3
bioclim %>% filter(Site == "Hoyo Sin Tierra") -> bioclim4

### PCA 1

bioclim1[, 3:8] %>%
  FactoMineR::PCA() -> pca1

cbind((bioclim1 %>%  dplyr::select(Plot, Site, Survey)), data.frame(pca1$ind$coord[, 1:2])) -> pcaInds1

pca1$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Site = "Los Cazadores") -> pcaVars1

### PCA 2

bioclim2[, 3:8] %>%
  FactoMineR::PCA() -> pca2

cbind((bioclim2 %>%  dplyr::select(Plot, Site, Survey)), data.frame(pca2$ind$coord[, 1:2])) -> pcaInds2

pca2$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Site = "Hou Sin Tierri") -> pcaVars2

### PCA 3

bioclim3[, 3:8] %>%
  FactoMineR::PCA() -> pca3

cbind((bioclim3 %>%  dplyr::select(Plot, Site, Survey)), data.frame(pca3$ind$coord[, 1:2])) -> pcaInds3

pca3$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Site = "Los Boches") -> pcaVars3

### PCA 4

bioclim4[, 3:8] %>%
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
