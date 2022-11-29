library(tPlotyverse)

read.csv("data/spatial-survey-header.csv") -> header
read.csv("data/spatial-survey-species.csv") -> species

read.csv("data/temporal-survey-species.csv") %>%
  select(Taxon) %>%
  unique -> focusspp

species %>%
  group_by(Plot) %>%
  mutate(Total = sum(Cover)) %>%
  mutate(RelCover = Cover / Total) %>%
  arrange(Plot, -Cover) %>%
  mutate(CumCover = cumsum(RelCover)) -> RelCovers

RelCovers %>%
  filter(CumCover <= .80) %>% 
  filter(CumCover == max(CumCover)) %>%
  select(Plot, Cover)  %>%
  rename(T = Cover) -> threshold 

RelCovers %>%
  merge(threshold, all.x = TRUE) %>%
  group_by(Plot) %>%
  arrange(Plot, CumCover) %>%
  filter((is.na(T) & CumCover == min(CumCover)) | Cover >= T) %>%
  group_by(Taxon) %>%
  tally %>%
  arrange(-n) %>%
  filter(! Taxon %in% focusspp$Taxon) -> otherspp

species %>%
  merge(header) %>%
  group_by(Taxon, Site) %>%
  summarise(n = length(Taxon), Cover = sum(Cover)) %>%
  group_by() %>%
  mutate(Group = ifelse(Taxon %in% focusspp$Taxon, "A = Focus spp", "C = Rare spp")) %>%
  mutate(Group = ifelse(Taxon %in% otherspp$Taxon, "B = 80-biomass spp", Group)) %>%
  mutate(y = paste(n, Cover, sep = " | ")) %>%
  select(-c(Cover, n)) %>%
  spread(Site, y, fill = "-") %>%
  merge(otherspp, all.x = TRUE) %>%
  mutate(Group = ifelse(Group == "B = 80-biomass spp", paste(Group, "in", n, "plots"), Group)) %>%
  select(Group, Taxon, `Hoyo Sin Tierra`, `Los Boches`, `Hou Sin Tierri`, `Los Cazadores`) %>% 
  arrange(Group, Taxon) %>%
  write.csv("results/Picos spp to collect.csv", row.names = FALSE) 
