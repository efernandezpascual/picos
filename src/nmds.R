library(tidyverse)

### Vegetation data from the spatial survey

read.csv("data/spatial-survey-species.csv") %>%
  filter(Plot != "A03") %>%
  filter(Plot != "D19")  %>%
  spread(Taxon, Cover, fill = 0) %>%
  arrange(Plot) %>%
  column_to_rownames(var = "Plot") -> species 
### Header data

read.csv("data/spatial-survey-header.csv") %>% 
  mutate(Survey = "Spatial") %>%
  merge(read.csv("results/supplement/S1 - Bioclimatic indices.csv")) -> header 

### Do NMDS

species %>%
  vegan::metaMDS(trymax = 100000, k = 2) -> 
  nmds # Ordination output

### Site scores

vegan::scores(nmds, "sites") %>%
  data.frame() %>%
  rownames_to_column("Plot") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  merge(header) %>%
  dplyr::select(Plot, Site, Survey, Dim.1, Dim.2) %>% 
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) -> sitescores

### Species scores

vegan::scores(nmds, "species") %>%
  data.frame() %>%
  rownames_to_column("Taxon") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  mutate(Species = gsub(" gr\\.", "", Taxon)) %>%
  mutate(Species = gsub("apenninum subsp. ", "", Species)) %>%
  separate(Species, into = c("G", "S")) %>%
  mutate(Species = paste(substr(G, 1, 3), substr(S, 1, 3), sep = "")) %>%
  dplyr::select(Taxon, Species, Dim.1, Dim.2)-> sppscores

### Environmental scores

header %>%
  filter(Plot %in% sitescores$Plot) %>%
  column_to_rownames(var = "Plot") %>%
  dplyr::select(-Site) %>%
  select(bio1:GDD) -> env

env %>% cor() -> biocor

save(biocor, file = "results/numerical/bioclim-correlations.R")

vegan::envfit(nmds, env, permutations = 999, na.rm = TRUE) -> ef1

save(ef1, file = "results/numerical/environmental-fit.R")

ef1$vectors$arrows %>%
  data.frame() %>%
  dplyr::select(NMDS1, NMDS2) %>%
  rownames_to_column(var = "Variable") %>%
  filter(Variable %in% c("GDD", "FDD")) %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) -> envvars

### Plot NMDS sites

ggplot(sitescores, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = envvars, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2)) +
  geom_point(aes(fill = Site, alpha = Survey), color = "black", size = 4, shape = 21) +
  scale_shape_manual(values = c(21, 24)) +
  scale_alpha_manual(values = c(0.4, 1), guide = FALSE) +
  ggrepel::geom_text_repel(data = sppscores, aes(label = Species), color = "grey33", fontface = "italic", size = 4, segment.color = "transparent") +
  geom_label(data = envvars, aes(x = Dim.1, y = Dim.2, label = Variable),  show.legend = FALSE, size = 4) +
  ggthemes::theme_tufte() +
  guides(fill=guide_legend(override.aes=list(shape = 21))) + 
  theme(legend.position = "top", legend.box = "vertical", legend.margin = margin(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black")) +
  coord_cartesian(xlim = c(-1.1, 1), ylim = c(-1, 1)) +
  scale_x_continuous(name = "NMDS1") + 
  scale_y_continuous(name = "NMDS2") +
  scale_fill_manual(values = c("gold", "limegreen",  "deepskyblue1", "darkorchid")) +
  annotate("label", x = -.8, y = 1, label = "Hot & Snowy", fill = "indianred", color = "white", size = 4) +
  annotate("label", x = .7, y = 1, label = "Cold & Snowy", fill = "deepskyblue", color = "white", size = 4) +
  annotate("label", x = -.8, y = -1, label = "Hot & Frozen", fill = "firebrick", color = "white", size = 4) +
  annotate("label", x = .7, y = -1, label = "Cold & Frozen", fill = "slateblue3", color = "white", size = 4) -> f4; f4

### Save figure

ggsave(f4, file = "results/figures/F4 - NMDS.png", 
       path = NULL, scale = 1, width = 182, height = 182, units = "mm", dpi = 600)
# ggsave(fig, file = "results/figures/nmds-species.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182 height = 182, units = "mm", dpi = 600, compression = "lzw")

