library(tidyverse); library(vegan)
  
sor <- function(x)  
  {x %>%
  select(Plot, Taxon, Cover) %>%
  spread(Taxon, Cover, fill = 0) %>%
  column_to_rownames(var = "Plot") %>%
  vegdist(method = "bray", binary = TRUE) %>%
  mean %>%
    data.frame}

read.csv("data/ibutton-plot-species.csv") %>%
  merge(read.csv("data/ibutton-plot-header.csv")) %>%
  group_by(Site) %>%
  do(sor(.)) %>%
  rename(Space = ".") -> inspace

read.csv("data/focal-plot-species.csv") %>%
  merge(read.csv("data/focal-plot-header.csv")) %>%
  separate(Plot, into = c("Plot2", "Year"), sep = "-", remove = FALSE) %>%
  group_by(Site, Plot2) %>%
  do(sor(.)) %>%
  rename(Time = ".") %>%
  group_by(Site) %>%
  summarise(Time = mean(Time)) -> intime

merge(intime, inspace) %>%
  mutate(Site = fct_relevel(Site, "El Cable", "Hoyo Sin Tierra", "Cabaña Verónica",
                            "Horcados Rojos", "Los Boches", "Hou Sin Tierri",
                            "Los Cazadores", "Urriellu")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) %>%
  gather(Dimension, Value, Time:Space) %>%
  ggplot(aes(Dimension, Value, fill = Dimension)) + 
  facet_wrap(~ Site, nrow = 1) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Species dissimilarity: in space (iButtons) vs. time (10 years)") +
  xlab("Sites") +
  ylab("Sørensen dissimilarity index (1 - Sørensen)") +
  ggthemes::theme_tufte() +
  scale_fill_manual(values = c("purple", "gold")) + 
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        strip.text = element_text(size = 22), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        title = element_text(size = 24, color = "black", face = "bold"),
        axis.title.y = element_text(size = 22, color = "black"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 14, color = "black", face = "italic", angle = 50, hjust=1)) -> f5; f5

ggsave(f5, file = "results/f5.png", 
       path = NULL, scale = 1, width = 330, height = 185, units = "mm", dpi = 600)
