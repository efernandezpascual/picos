library(tidyverse)

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

### SNCs

read.csv("data/spatial-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  group_by(Plot, Day = lubridate::floor_date(Time, "day")) %>%
  summarise(T = mean(Temperature), X = max(Temperature), N = min(Temperature), n = length(Time)) %>% # Daily mean, max, min
  mutate(FDD = ifelse(T < 0, T, 0)) %>% # Freezing degrees per day
  mutate(GDD = ifelse(T >= 1, T, 0)) %>% # Growing degrees day per month https://link.springer.com/article/10.1007/s00035-021-00250-1
  group_by(Plot, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(FDD = sum(FDD), # FDD per month
            GDD = sum(GDD)) %>% # GDD per month
  group_by(Plot) %>%
  summarise(FDD = sum(FDD), # FDD per year
            GDD = sum(GDD))  %>%
  select(Plot, FDD, GDD) %>%
  merge(read.csv("data/spatial-survey-species.csv")) %>%
  group_by(Taxon) %>%
  summarise(n = length(Plot),
            GDD = weighted.mean(GDD, Cover),
            FDD = abs(weighted.mean(FDD, Cover))) %>%
  # filter(n > 4) %>%
  arrange(Taxon) -> sncs

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
  merge(sppscores) %>%
  merge(sncs) %>%
  mutate(C = ifelse(Change < -5, "Decreasing frequency", "Minimal changes"),
         C = ifelse(Change > 5, "Increasing frecuency", C))  -> changespp

### Plot

changespp %>%
  ggplot(aes(GDD, FDD, fill = C)) +
  geom_point(aes(size = abs(Change)), shape = 21, show.legend = TRUE) +
  scale_size(range = c(2, 25)) +
  scale_fill_manual(values = c("salmon2", "skyblue", "white")) +
  geom_text(data = filter(changespp, Change < -5 | Change > 5), aes(label = Species), color =  "black", fontface = "italic", angle = 25, size = 3) +
  ggthemes::theme_tufte() + 
  xlab("Growing degrees-day (cumulative > 1ºC)") + 
  ylab("Freezing degrees-day (cumulative < 0ºC, absolute)") +
  guides(size = "none", fill = guide_legend(override.aes = list(size = 5))) +
  theme(legend.position = c(.79, .9), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black")) -> f1;f1

### Save figure

ggsave(f1, file = "results/figures/spp-changes.png", 
       path = NULL, scale = 1, width = 127, height = 100, units = "mm", dpi = 600)
# ggsave(f1, file = "results/figures/pca-temperatures.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182, height = 182, units = "mm", dpi = 600, compression = "lzw")