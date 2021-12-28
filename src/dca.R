read.csv("data/ibutton-plot-species.csv") %>%
  group_by(Taxon) %>% 
  tally %>%
  arrange(-n) %>%
  data.frame %>%
  filter(n >2) %>%
  pull(Taxon) -> spp

read.csv("data/ibutton-plot-header.csv") %>%
  select(Plot, Site) %>%
  merge(read.csv("data/ibutton-plot-species.csv")) %>%
  # filter(Site == "Los Boches") %>%
  filter(Plot != "D19") %>%
  select(-Site) %>%
  spread(Taxon, Cover, fill = 0) %>%
  arrange(Plot) %>%
  column_to_rownames(var = "Plot") -> df4

read.csv("data/ibutton-plot-header.csv") %>%
  select(Plot, Site) %>%
  merge(read.csv("data/ibutton-plot-species.csv")) %>%
  # filter(Site == "Los Boches") %>%
  filter(Plot != "D19") %>%
  select(Plot) %>% unique %>%
  arrange(Plot) %>%
  merge(micro) %>%
  select(-c(Site, Plot, `Annual\ntemperature`, `Annual range`, `Diurnal range`)) -> env

vegan::decorana(df4) -> dca1

vegan::envfit(dca1, env) -> ef1

ef1$vectors$arrows %>%
  data.frame() %>%
  select(DCA1, DCA2) %>%
  rownames_to_column(var = "Variable") -> vars

ef1$vectors

vegan::scores(dca1, "species") %>%
  data.frame() %>%
  rownames_to_column("Species") %>%
  filter(Species %in% spp) -> 
  sppscores

vegan::scores(dca1, "sites") %>%
  data.frame %>%
  rownames_to_column(var = "Plot") %>%
  merge(groups) -> df5

ggplot(df5, aes(x = DCA1, y = DCA2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = vars, aes(x = 0, y = 0, xend = 3*DCA1, yend = 3*DCA2)) +
  ggthemes::theme_tufte() + 
  ggrepel::geom_text_repel(data = sppscores, aes(label = Species), fontface = "italic", size = 3) + 
  geom_label(data = vars, aes(x = 3*DCA1, y = 3*DCA2, label = Variable),  show.legend = FALSE, size = 4) +
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
  labs(title = "DCA plant species (n = 78 plots)") +
  annotate("label", x = -2.4, y = 2.4, label = "Hot & Frozen", fill = "indianred", color = "white", size = 6) +
  annotate("label", x = 2.4, y = 2.4, label = "Cold & Frozen", fill = "purple", color = "white", size = 6) +
  annotate("label", x = -2.4, y = -3, label = "Hot & Insulated", fill = "goldenrod", color = "white", size = 6) +
  annotate("label", x = 2.4, y = -3, label = "Cold & Insulated", fill = "royalblue", color = "white", size = 6) -> f1; f1

## Export

ggsave(f1, file = "results/DCA los boches.png", 
       path = NULL, scale = 1, width = 330, height = 185, units = "mm", dpi = 600)
