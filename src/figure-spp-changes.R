### NMDS species scores

read.csv("data/spatial-survey-species.csv") %>%
  filter(Plot != "A03") %>%
  filter(Plot != "D19") %>%
  spread(Taxon, Cover, fill = 0) %>%
  arrange(Plot) %>%
  column_to_rownames(var = "Plot") %>%
  vegan::metaMDS(trymax = 100000, k = 2) %>%
  vegan::scores(., "species") %>%
  data.frame() %>%
  rownames_to_column("Taxon") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  mutate(Species = gsub(" gr\\.", "", Taxon)) %>%
  mutate(Species = gsub("apenninum subsp. ", "", Species)) %>%
  separate(Species, into = c("G", "S")) %>%
  mutate(Species = paste(substr(G, 1, 3), substr(S, 1, 3), sep = "")) %>%
  dplyr::select(Taxon, Species, Dim.1, Dim.2) -> sppscores

### Species changes

read.csv("data/temporal-survey-matrices.csv") %>%
  group_by(Plot, Taxon) %>%
  summarise(Cover = sum(Present)) %>%
  merge(read.csv("data/species-checklist.csv"), all.x = TRUE) %>%
  filter(Lifeform != "Therophyte") %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  group_by(Taxon, Year) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Year, Cover) %>%
  mutate(Change = `19` - `09`) %>%
  mutate(ChangeP = `19` / `09`) %>%
  arrange(Change) %>%
  na.omit %>%
  merge(sppscores) -> changespp

### Plot decreasing

changespp %>%
  filter(Change < -10) %>%
  ggplot(aes(Dim.1, Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = envvars, aes(x = 0, y = 0, xend = .3*Dim.1, yend = .3*Dim.2)) +
  geom_point(aes(size = abs(Change), fill = Change), shape = 21, show.legend = FALSE) +
  geom_label(data = envvars, aes(x = .3*Dim.1, y = .3*Dim.2, label = Variable),  show.legend = FALSE, size = 4) +
  geom_text(aes(y = Dim.2 +.03, label = Species), color =  "white", fontface = "italic", ) +
  geom_text(aes(y = Dim.2 -.03, label = Change), color =  "white") +
  scale_fill_gradient(high = "lightcoral", low = "firebrick2", na.value = NA) +
  scale_size(range = c(22, 34)) +
  coord_cartesian(xlim = c(-.6, .7), ylim = c(-.9, .7)) +
  ggthemes::theme_tufte() + 
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "(B) Decreasing frequency spp.") +
  scale_x_continuous(name = "NMDS1") + 
  scale_y_continuous(name = "NMDS2") +
  theme(legend.position = c(.5, .1), 
        legend.title = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10, color = "black")) -> f2;f2

### Plot increasing

changespp %>%
  ggplot(aes(Dim.1, Change)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(size = abs(Change), fill = Change), shape = 21, show.legend = FALSE) +
  scale_size(range = c(2, 45)) +
  scale_fill_gradient2(midpoint = 0, mid = "white", high = "skyblue", low = "darkred") +
  geom_text(data = filter(changespp, Change < -10 | Change > 10), aes(label = Species), color =  "black", fontface = "italic") +
  ggthemes::theme_tufte() + 
  labs(title = "(C) Increasing frequency spp.") +
  xlab("NMDS1") + 
  ylab("Change (in 10x10 cm presences)") +
  theme(legend.position = c(.5, .1), 
        legend.title = element_blank(),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10, color = "black")) -> f3;f3