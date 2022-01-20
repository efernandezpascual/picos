library(tidyverse)

### Species traits

read.csv("data/species-checklist.csv") %>%
  merge(read.csv("results/numerical/snc.csv")) -> traits

traits %>% group_by(Chorology) %>% summarise(GDD = mean(GDD), FDD = mean(FDD), T = mean(T))

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
  mutate(ChangeP = Change / `09`) %>%
  arrange(Change) %>%
  na.omit %>%
  merge(read.csv("data/species-checklist.csv")) %>%
  merge(read.csv("results/numerical/snc.csv")) -> species

### Model total change

lm(Change ~ Chorology + Lifeform + GDD + FDD, data = species) -> m1

summary(m1)

MASS::stepAIC(m1) -> m2

summary(m2)

### Model % change

lm(ChangeP ~ Chorology + Lifeform + GDD + FDD + `09`, data = species) -> m1

summary(m1)

MASS::stepAIC(m1) -> m2

summary(m2)

plot(m2)

