library(tidyverse)

read.csv("data/ibutton-plot-species.csv") %>%
  merge(read.csv("data/species-traits.csv"), all.x = TRUE) %>%
  mutate(Traited = ifelse(is.na(SLA), "No", "Yes")) %>%
  group_by(Plot, Traited) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Traited, Cover) %>%
  mutate(Coverage = Yes / (No + Yes)) %>% filter(Coverage > .8)
