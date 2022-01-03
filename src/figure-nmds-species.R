library(tidyverse)

### Vegetation data from the spatial survey

read.csv("data/spatial-survey-species.csv") %>%
  filter(Plot != "A03") %>%
  filter(Plot != "D19") %>%
  spread(Taxon, Cover, fill = 0) %>%
  arrange(Plot) %>%
  column_to_rownames(var = "Plot") -> species

### Do NMDS

species %>%
  vegan::metaMDS(trymax = 100000, k = 2) -> 
  nmds # Ordination output

### Site scores

vegan::scores(nmds, "sites") %>%
  data.frame() %>%
  rownames_to_column("Plot") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  merge(read.csv("data/spatial-survey-header.csv")) %>%
  dplyr::select(Plot, Site, Dim.1, Dim.2) %>% 
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) -> sitescores

### Species scores

read.csv("data/spatial-survey-species.csv") %>%
  group_by(Taxon) %>%
  tally %>%
  filter(n > 80 * .2) %>%
  pull(Taxon) -> sppfreq

vegan::scores(nmds, "species") %>%
  data.frame() %>%
  rownames_to_column("Taxon") %>%
  # filter(Taxon %in% sppfreq) %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  mutate(Species = gsub(" gr\\.", "", Taxon)) %>%
  mutate(Species = gsub("apenninum subsp. ", "", Species)) %>%
  separate(Species, into = c("G", "S")) %>%
  mutate(Species = paste(substr(G, 1, 3), substr(S, 1, 3), sep = "")) %>%
  dplyr::select(Taxon, Species, Dim.1, Dim.2)-> sppscores

### Environmental scores

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
  summarise(FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Plot, Site) %>%
  summarise(`Freezing\ndegrees-day` = sum(FDD), # FDD per year
            `Growing\ndegrees-day` = sum(GDD)) %>%
  group_by() %>%
  arrange(Plot) %>%
  filter(Plot %in% sitescores$Plot) %>%
  column_to_rownames(var = "Plot") %>%
  dplyr::select(-Site) -> env

vegan::envfit(nmds, env, permutations = 999, na.rm = TRUE) -> ef1

ef1$vectors$arrows %>%
  data.frame() %>%
  dplyr::select(NMDS1, NMDS2) %>%
  rownames_to_column(var = "Variable") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) -> envvars

### Plot NMDS sites

ggplot(sitescores, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = envvars, aes(x = 0, y = 0, xend = .7*Dim.1, yend = .7*Dim.2)) +
  geom_point(aes(fill = Site), size = 4, shape = 22) +
  geom_label(data = envvars, aes(x = .7*Dim.1, y = .7*Dim.2, label = Variable),  show.legend = FALSE, size = 2.5) +
  ggthemes::theme_tufte() + 
  guides(fill = guide_legend(ncol = 2)) +
  theme(legend.position = c(.5, .1), 
        legend.title = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10, color = "black")) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
  labs(title = "(A) Spatial survey - sites") +
  scale_x_continuous(name = "NMDS1") + 
  scale_y_continuous(name = "NMDS2") +
  scale_color_manual(values = c("goldenrod", "forestgreen",  "royalblue", "darkorchid")) +
  scale_fill_manual(values = c("goldenrod", "forestgreen",  "royalblue", "darkorchid")) -> f1; f1

### Plot NMDS sites

ggplot(sitescores, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  ggrepel::geom_text_repel(data = sppscores, aes(label = Species), fontface = "italic", size = 2, segment.color = "transparent") +
  ggthemes::theme_tufte() + 
  theme(legend.position = "top", 
        legend.title = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10, color = "black")) +
  coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
  labs(title = "(B) Spatial survey - species") +
  scale_x_continuous(name = "NMDS1") + 
  scale_y_continuous(name = "NMDS2") +
  scale_color_manual(values = c("goldenrod", "forestgreen",  "royalblue", "darkorchid")) +
  scale_fill_manual(values = c("goldenrod", "forestgreen",  "royalblue", "darkorchid")) +
  annotate("label", x = -.7, y = 1, label = "Warm & Frozen", fill = "indianred", color = "white", size = 3) +
  annotate("label", x = .7, y = 1, label = "Cold & Frozen", fill = "deepskyblue", color = "white", size = 3) +
  annotate("label", x = -.7, y = -1, label = "Warm & Insulated", fill = "firebrick", color = "white", size = 3) +
  annotate("label", x = .7, y = -1, label = "Cold & Insulated", fill = "slateblue3", color = "white", size = 3) -> f2; f2

### Combine panels

cowplot::plot_grid(f1, f2, ncol = 2) -> fig

### Save figure

ggsave(fig, file = "results/figures/nmds-species.png", 
       path = NULL, scale = 1, width = 182, height = 182/2, units = "mm", dpi = 600)
# ggsave(fig, file = "results/figures/nmds-species.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 127, height = 160, units = "mm", dpi = 600, compression = "lzw")

