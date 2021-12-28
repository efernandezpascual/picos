library(tidyverse)

read.csv("data/focal-plot-matrices.csv") %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  select(Plot, Cell, Taxon, Present, Site) %>%
  merge(read.csv("data/Grid.csv")) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  mutate(Year = fct_recode(Year, "2009" = "09")) %>%
  mutate(Year = fct_recode(Year, "2019" = "19")) %>%
  select(Site, Plot, Year, Taxon, Row, Column, Present) %>%
  arrange(Site, Plot, Year, Taxon, Row, Column, Present) %>%
  filter(Taxon == "Helianthemum apenninum subsp. urrielense") %>%
  # filter(Plot == "HI2") %>%
  ggplot(aes(Column, Row, fill = Present)) +
  facet_grid(Year ~ Plot) +
  geom_tile(fill = "limegreen") +
  labs(title = "10-year changes", subtitle = "Helianthemum apenninum subsp. urrielense") +
  xlab("Rows") +
  ylab("Columns") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        plot.subtitle = element_text(size = 20, color = "black", face = "italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x =element_blank(),
        axis.ticks = element_blank()) -> urrielense; urrielense

ggsave(urrielense, file = "results/urrielense.png", 
       path = NULL, scale = 1,width = (330/3)*2, height = 185, units = "mm", dpi = 600) 

library(tidyverse)

read.csv("data/focal-plot-matrices.csv") %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  select(Plot, Cell, Taxon, Present, Site) %>%
  merge(read.csv("data/Grid.csv")) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  mutate(Year = fct_recode(Year, "2009" = "09")) %>%
  mutate(Year = fct_recode(Year, "2019" = "19")) %>%
  select(Site, Plot, Year, Taxon, Row, Column, Present) %>%
  arrange(Site, Plot, Year, Taxon, Row, Column, Present) %>%
  filter(Taxon == "Minuartia verna") %>%
  # filter(Plot == "HI2") %>%
  ggplot(aes(Column, Row, fill = Present)) +
  facet_grid(Year ~ Plot) +
  geom_tile(fill = "limegreen") +
  labs(title = "10-year changes", subtitle = "Minuartia verna") +
  xlab("Rows") +
  ylab("Columns") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        plot.subtitle = element_text(size = 20, color = "black", face = "italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x =element_blank(),
        axis.ticks = element_blank()) -> minuartia; minuartia

ggsave(minuartia, file = "results/minuartia.png", 
       path = NULL, scale = 1,  width = (330/3)*2, height = 185, units = "mm", dpi = 600) 

read.csv("data/focal-plot-matrices.csv") %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  select(Plot, Cell, Taxon, Present, Site) %>%
  merge(read.csv("data/Grid.csv")) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  mutate(Year = fct_recode(Year, "2009" = "09")) %>%
  mutate(Year = fct_recode(Year, "2019" = "19")) %>%
  select(Site, Plot, Year, Taxon, Row, Column, Present) %>%
  arrange(Site, Plot, Year, Taxon, Row, Column, Present) %>%
  filter(Taxon == "Anthyllis vulneraria") %>%
  # filter(Plot == "HI2") %>%
  ggplot(aes(Column, Row, fill = Present)) +
  facet_grid(Year ~ Plot) +
  geom_tile(fill = "limegreen") +
  labs(title = "10-year changes", subtitle = "Anthyllis vulneraria") +
  xlab("Rows") +
  ylab("Columns") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        plot.subtitle = element_text(size = 20, color = "black", face = "italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x =element_blank(),
        axis.ticks = element_blank()) -> Anthyllis; Anthyllis

ggsave(Anthyllis, file = "results/Anthyllis.png", 
       path = NULL, scale = 1, width = (330/3)*2, height = 185, units = "mm", dpi = 600) 


read.csv("data/focal-plot-matrices.csv") %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  select(Plot, Cell, Taxon, Present, Site) %>%
  merge(read.csv("data/Grid.csv")) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  mutate(Year = fct_recode(Year, "2009" = "09")) %>%
  mutate(Year = fct_recode(Year, "2019" = "19")) %>%
  select(Site, Plot, Year, Taxon, Row, Column, Present) %>%
  arrange(Site, Plot, Year, Taxon, Row, Column, Present) %>%
  filter(Taxon == "Poa alpina") %>%
  # filter(Plot == "HI2") %>%
  ggplot(aes(Column, Row, fill = Present)) +
  facet_grid(Year ~ Plot) +
  geom_tile(fill = "indianred") +
  labs(title = "10-year changes", subtitle = "Poa alpina") +
  xlab("Rows") +
  ylab("Columns") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        plot.subtitle = element_text(size = 20, color = "black", face = "italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x =element_blank(),
        axis.ticks = element_blank()) -> poa; poa

ggsave(poa, file = "results/poa.png", 
       path = NULL, scale = 1, width = 330/2, height = 185, units = "mm", dpi = 600) 

read.csv("data/focal-plot-matrices.csv") %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  select(Plot, Cell, Taxon, Present, Site) %>%
  merge(read.csv("data/Grid.csv")) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  mutate(Year = fct_recode(Year, "2009" = "09")) %>%
  mutate(Year = fct_recode(Year, "2019" = "19")) %>%
  select(Site, Plot, Year, Taxon, Row, Column, Present) %>%
  arrange(Site, Plot, Year, Taxon, Row, Column, Present) %>%
  filter(Taxon == "Ranunculus parnassiifolius subsp. favargeri") %>%
  # filter(Plot == "HI2") %>%
  ggplot(aes(Column, Row, fill = Present)) +
  facet_grid(Year ~ Plot) +
  geom_tile(fill = "indianred") +
  labs(title = "10-year changes", subtitle = "Ranunculus parnassiifolius subsp. favargeri") +
  xlab("Rows") +
  ylab("Columns") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        plot.subtitle = element_text(size = 20, color = "black", face = "italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x =element_blank(),
        axis.ticks = element_blank()) -> ranunculus; ranunculus

ggsave(ranunculus, file = "results/ranunculus.png", 
       path = NULL, scale = 1, width = (330/3)*2, height = 185, units = "mm", dpi = 600) 

read.csv("data/focal-plot-matrices.csv") %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  filter(Site %in% c("Hou Sin Tierri", "Hoyo Sin Tierra", "Los Boches", "Los Cazadores")) %>%
  select(Plot, Cell, Taxon, Present, Site) %>%
  merge(read.csv("data/Grid.csv")) %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  mutate(Year = fct_recode(Year, "2009" = "09")) %>%
  mutate(Year = fct_recode(Year, "2019" = "19")) %>%
  select(Site, Plot, Year, Taxon, Row, Column, Present) %>%
  arrange(Site, Plot, Year, Taxon, Row, Column, Present) %>%
  filter(Taxon == "Carex sempervirens") %>%
  # filter(Plot == "HI2") %>%
  ggplot(aes(Column, Row, fill = Present)) +
  facet_grid(Year ~ Plot) +
  geom_tile(fill = "indianred") +
  labs(title = "10-year changes", subtitle = "Carex sempervirens") +
  xlab("Rows") +
  ylab("Columns") +
  ggthemes::theme_tufte() +
  theme(legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        plot.subtitle = element_text(size = 20, color = "black", face = "italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x =element_blank(),
        axis.ticks = element_blank()) -> Carex; Carex

ggsave(Carex, file = "results/Carex.png", 
       path = NULL, scale = 1, width = (330/3)*2, height = 185, units = "mm", dpi = 600) 
