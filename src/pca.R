library(tidyverse)

read.csv("results/supplement/S1 - Bioclimatic indices.csv") -> bioclim

### PCA

bioclim[, 3:8] %>%
  FactoMineR::PCA() -> pca1

cbind((bioclim %>%  dplyr::select(Plot, Site, Survey)), data.frame(pca1$ind$coord[, 1:2])) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) -> pcaInds

pca1$var$coord[, 1:2] %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  mutate(Variable = fct_recode(Variable, "Snow" = "Snw"))-> pcaVars

### Plot PCA

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = 3*Dim.1, yend = 3*Dim.2)) +
  geom_point(aes(fill = Site, shape = Survey), size = 4) +
  geom_label(data = pcaVars, aes(x = 3*Dim.1, y = 3*Dim.2, label = Variable),  show.legend = FALSE, size = 4) +
  ggthemes::theme_tufte() + 
  theme(text = element_text(family = "sans"),
        legend.position = "right", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black"),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm")) +
  guides(fill = guide_legend(override.aes = list(shape = 22))) +
  scale_x_continuous(name = paste("Axis 1 (", round(pca1$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pca1$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = c("gold", "#B3EE3A",  "#40E0D0", "#551A8B")) +
  scale_fill_manual(values = c("gold", "#B3EE3A",  "#40E0D0", "#551A8B")) -> f1; f1

pca1$eig
pca1$var

### Save figure

ggsave(f1, file = "results/figures/F3 - PCA of the bioclimatic indices.png", 
       path = NULL, scale = 1, width = 182, height = 120, units = "mm", dpi = 600)
# ggsave(f1, file = "results/figures/pca-temperatures.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182, height = 182, units = "mm", dpi = 600, compression = "lzw")
