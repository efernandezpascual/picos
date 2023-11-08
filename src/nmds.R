library(tidyverse)

### Vegetation data from the spatial survey

read.csv("data/spatial-survey-species.csv") %>%
  filter(Plot != "A03") %>%
  filter(Plot != "D19")  %>%
  spread(Taxon, Cover, fill = 0) %>%
  arrange(Plot) %>%
  column_to_rownames(var = "Plot") -> species 

# ### Header data
# 
# read.csv("data/spatial-survey-header.csv") %>% 
#   mutate(Survey = "Spatial") %>%
#   merge(read.csv("results/supplement/S1 - Bioclimatic indices.csv")) -> header 

# GDD >5
read.csv("data/spatial-survey-header.csv") %>% 
  mutate(Survey = "Spatial") %>%
  merge(read.csv("results/supplement/S1 - Bioclimatic indices filtered clara.csv")) -> header

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
  mutate(Site = fct_recode(Site, "1 Los Cazadores" = "Los Cazadores"))%>%
  mutate(Site = fct_recode(Site, "2 Ḥou Sin Tierri" = "Hou Sin Tierri"))%>%
  mutate(Site = fct_recode(Site, "3 Los Boches" = "Los Boches"))%>%
  mutate(Site = fct_recode(Site, "4 Hoyo Sin Tierra" = "Hoyo Sin Tierra")) -> sitescores

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
write.csv(sppscores, "results/tables/nmds_cl.csv")

### Environmental scores

header %>%
  filter(Plot %in% sitescores$Plot) %>%
  column_to_rownames(var = "Plot") %>%
  dplyr::select(-Site) %>%
  dplyr::select(FDD, GDD, Snw) -> env

env %>% cor() -> biocor

save(biocor, file = "results/numerical/bioclim-correlations_cl.R")

vegan::envfit(nmds, env, permutations = 999, na.rm = TRUE) -> ef1

save(ef1, file = "results/numerical/environmental-fit_cl.R")

ef1$vectors$arrows %>%
  data.frame() %>%
  dplyr::select(NMDS1, NMDS2) %>%
  rownames_to_column(var = "Variable") %>%
  filter(Variable %in% c("GDD", "FDD", "Snw")) %>%
  mutate(Variable = fct_recode(Variable, "Snow" = "Snw")) %>%
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
  labs(title = "(D) NMDS") +
  guides(fill=guide_legend(override.aes=list(shape = 21))) + 
  theme(text = element_text(family = "sans"),
        legend.position = "right", legend.box = "vertical", legend.margin = margin(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, color = "black"),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10, color = "black")) +
  coord_cartesian(xlim = c(-1.1, 1), ylim = c(-1, 1)) +
  scale_x_continuous(name = "NMDS1") + 
  scale_y_continuous(name = "NMDS2") +
  scale_fill_manual(values = c("darkorange1", "dodgerblue4", "limegreen",  "deeppink4")) +
  annotate("label", x = -.8, y = 1, label = "Hot & Snowy", fill = "indianred", color = "white", size = 4) +
  annotate("label", x = .7, y = 1, label = "Cold & Snowy", fill = "deepskyblue", color = "white", size = 4) +
  annotate("label", x = -.8, y = -1, label = "Hot & Freezing", fill = "firebrick", color = "white", size = 4) +
  annotate("label", x = .7, y = -1, label = "Cold & Freezing", fill = "slateblue3", color = "white", size = 4) -> f1d; f1d

### Save figure

ggsave(f1d, file = "results/figures/F4 - NMDS_cl (H).png", 
       path = NULL, scale = 1, width = 182, height = 120, units = "mm", dpi = 600)
# ggsave(fig, file = "results/figures/nmds-species.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182 height = 182, units = "mm", dpi = 600, compression = "lzw")

