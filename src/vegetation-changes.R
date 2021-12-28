library(tidyverse)

read.csv("data/focal-plot-matrices.csv") -> matrices

matrices %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  group_by(Plot, Taxon) %>%
  summarise(Cover = sum(Present)) %>%
  merge(read.csv("data/checklist.csv"), all.x = TRUE) %>%
  filter(Lifeform != "Therophyte") %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  mutate(Taxon = fct_recode(Taxon,
                            "Ranunculus favargeri" = "Ranunculus parnassiifolius subsp. favargeri",
                            "Dethawia cantabrica" = "Dethawia splendens subsp. cantabrica",
                            "Helianthemum urrielense" = "Helianthemum apenninum subsp. urrielense",
                            "Thymus ligusticus" = "Thymus praecox subsp. ligusticus")) %>%
  group_by(Taxon, Year) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Year, Cover) %>%
  mutate(Change = `19` - `09`) %>%
  arrange(Change) %>%
  filter(Change < -10 | Change > 10) %>%
  na.omit %>%
  ggplot(aes(reorder(Taxon, Change), Change)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_bar(stat = "identity", fill = "limegreen") +
  labs(title = "10-year species changes") +
  xlab("Species") +
  ylab("Change (in 10x10 cm presences)") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        axis.title.y = element_text(size = 22, color = "black"),
        axis.title.x = element_text(size = 22, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        axis.text.x = element_text(size = 20, color = "black", face = "italic", angle = 50, hjust=1)) -> f3; f3

ggsave(f3, file = "results/f3.png", 
       path = NULL, scale = 1, width = 330, height = 185, units = "mm", dpi = 600)

matrices %>%
  group_by(Plot, Taxon) %>%
  summarise(Cover = sum(Present)) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  group_by(Taxon, Year) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Year, Cover) %>%
  mutate(Change = `19` - `09`) %>%
  filter(is.na(Change)) %>%
  arrange(-`09`, `19`) %>%
  select(-Change)

# Traits

matrices %>%
  group_by(Plot, Taxon) %>%
  summarise(Cover = sum(Present)) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  group_by(Taxon, Year) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Year, Cover) %>%
  mutate(Change = `19` - `09`) %>%
  na.omit %>%
  merge(read.csv("data/species-traits.csv")) %>%
  ggplot(aes(SLA, Change)) +
  geom_text(aes(label = Taxon), fontface = "italic") +
  geom_smooth(method = "lm") +
  labs(title = "Species changes vs. SLA") +
  xlab("SLA (cm2 * g-1)") +
  ylab("Change (in 10x10 cm presences)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        axis.title.y = element_text(size = 22, color = "black"),
        axis.title.x = element_text(size = 22, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 10, color = "black", face = "italic", angle = 50, hjust=1)) -> f3b; f3b

ggsave(f3b, file = "results/f3b.png", 
       path = NULL, scale = 1, width = 330, height = 185, units = "mm", dpi = 600)
  


matrices %>%
  group_by(Plot, Taxon) %>%
  summarise(Cover = sum(Present)) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  mutate(Taxon = fct_recode(Taxon,
                            "Ranunculus favargeri" = "Ranunculus parnassiifolius subsp. favargeri",
                            "Dethawia cantabrica" = "Dethawia splendens subsp. cantabrica",
                            "Helianthemum urrielense" = "Helianthemum apenninum subsp. urrielense",
                            "Thymus ligusticus" = "Thymus praecox subsp. ligusticus")) %>%
  group_by(Taxon, Year) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Year, Cover) %>%
  mutate(Change = `19` - `09`) %>%
  arrange(Change)

matrices %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  group_by(Plot, Taxon) %>%
  summarise(Cover = sum(Present)) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  # mutate(Taxon = fct_recode(Taxon,
  #                           "Ranunculus favargeri" = "Ranunculus parnassiifolius subsp. favargeri",
  #                           "Dethawia cantabrica" = "Dethawia splendens subsp. cantabrica",
  #                           "Helianthemum urrielense" = "Helianthemum apenninum subsp. urrielense",
  #                           "Thymus ligusticus" = "Thymus praecox subsp. ligusticus")) %>%
  group_by(Taxon, Year) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Year, Cover) %>%
  mutate(Change = `19` - `09`) %>%
  arrange(-Change) %>%
  merge(read.csv("data/checklist.csv"), all.x = TRUE) %>%
  filter(Lifeform != "Therophyte") %>%
  arrange(-Change) %>%
  select(Taxon, Change, Lifeform, F:T) %>%
  filter(! is.na(Change)) %>%
  tail(4)

matrices %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  group_by(Plot, Taxon, Site) %>%
  summarise(Cover = sum(Present)) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  # mutate(Taxon = fct_recode(Taxon,
  #                           "Ranunculus favargeri" = "Ranunculus parnassiifolius subsp. favargeri",
  #                           "Dethawia cantabrica" = "Dethawia splendens subsp. cantabrica",
  #                           "Helianthemum urrielense" = "Helianthemum apenninum subsp. urrielense",
  #                           "Thymus ligusticus" = "Thymus praecox subsp. ligusticus")) %>%
  group_by(Taxon, Year, Site) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Year, Cover) %>%
  mutate(Change = `19` - `09`) %>%
  arrange(-Change) %>%
  merge(read.csv("data/checklist.csv"), all.x = TRUE) %>%
  filter(Lifeform != "Therophyte") %>%
  arrange(-Change) %>%
  select(Taxon, Site, Change, Lifeform, F:T) %>%
  filter(! is.na(Change)) %>%
  filter(Site == "Hoyo Sin Tierra") %>%
  head(4)

matrices %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  group_by(Plot, Taxon) %>%
  summarise(Cover = sum(Present)) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  # mutate(Taxon = fct_recode(Taxon,
  #                           "Ranunculus favargeri" = "Ranunculus parnassiifolius subsp. favargeri",
  #                           "Dethawia cantabrica" = "Dethawia splendens subsp. cantabrica",
  #                           "Helianthemum urrielense" = "Helianthemum apenninum subsp. urrielense",
  #                           "Thymus ligusticus" = "Thymus praecox subsp. ligusticus")) %>%
  group_by(Taxon, Year) %>%
  summarise(Cover = sum(Cover)) %>%
  merge(read.csv("data/checklist.csv"), all.x = TRUE) %>%
  filter(Lifeform != "Therophyte") %>%
  gather(Trait, Value, F:T) %>%
  group_by(Year, Trait) %>%
  summarise(Value = weighted.mean(Value, Cover, na.rm = TRUE)) %>%
  spread(Year, Value) # Same indicator values


matrices %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Taxon == "Helianthemum apenninum subsp. urrielense") %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  group_by(Site, Plot, Taxon) %>%
  summarise(Cover = sum(Present)) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  # mutate(Taxon = fct_recode(Taxon,
  #                           "Ranunculus favargeri" = "Ranunculus parnassiifolius subsp. favargeri",
  #                           "Dethawia cantabrica" = "Dethawia splendens subsp. cantabrica",
  #                           "Helianthemum urrielense" = "Helianthemum apenninum subsp. urrielense",
  #                           "Thymus ligusticus" = "Thymus praecox subsp. ligusticus")) %>%
  group_by(Taxon, Year, Site) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Year, Cover) %>%
  mutate(Change = `19` - `09`) %>%
  arrange(-Change) %>%
  merge(read.csv("data/checklist.csv"), all.x = TRUE) %>%
  filter(Lifeform != "Therophyte") %>%
  arrange(-Change) %>%
  select(Taxon, Site, Change, Lifeform, F:T) %>%
  filter(! is.na(Change))

matrices %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Taxon == "Salix breviserrata") %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  group_by(Site, Plot, Taxon) %>%
  summarise(Cover = sum(Present)) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  # mutate(Taxon = fct_recode(Taxon,
  #                           "Ranunculus favargeri" = "Ranunculus parnassiifolius subsp. favargeri",
  #                           "Dethawia cantabrica" = "Dethawia splendens subsp. cantabrica",
  #                           "Helianthemum urrielense" = "Helianthemum apenninum subsp. urrielense",
  #                           "Thymus ligusticus" = "Thymus praecox subsp. ligusticus")) %>%
  group_by(Taxon, Year, Site) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Year, Cover) %>%
  mutate(Change = `19` - `09`) %>%
  arrange(-Change) %>%
  merge(read.csv("data/checklist.csv"), all.x = TRUE) %>%
  filter(Lifeform != "Therophyte") %>%
  arrange(-Change) %>%
  select(Taxon, Site, Change, Lifeform, F:T) %>%
  filter(! is.na(Change))


matrices %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08"))  %>%
  spread(Year, Present) %>%
  