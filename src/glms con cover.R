library(tidyverse)

### Prepare data

read.csv("data/spatial-survey-species.csv") %>%
  group_by(Taxon) %>%
  mutate(n = length(Taxon)) %>%
  filter(n >= 5) %>%
  mutate(Presence = 1) %>%
  select(Plot, Taxon, Cover) %>%
  spread(Taxon, Cover, fill = 0) %>%
  gather(Taxon, Cover, -Plot) %>%
  merge(read.csv("results/supplement/S1 - Bioclimatic indices.csv")) -> presences

### Get GLM coefficients

glms <- function(x) {
  glm(cbind(Cover, 100 - Cover) ~ GDD, family = "binomial", data = x) -> m1
  broom::tidy(m1)
}

presences %>%
  group_by(Taxon) %>%
  do(glms(.)) %>%
  gather(Trait, Value, estimate:p.value) %>%
  mutate(Trait = paste(term, Trait)) %>%
  select(Taxon, Trait, Value) %>%
  spread(Trait, Value) %>%
  select(`Taxon`, 
         `(Intercept) estimate`, `(Intercept) std.error`, `(Intercept) statistic`, `(Intercept) p.value`, 
         `GDD estimate`, `GDD std.error`, `GDD statistic`, `GDD p.value`) -> t1

### Get GLM summary statistics

glms.glance <- function(x) {
  glm(cbind(Cover, 100 - Cover) ~ GDD, family = "binomial", data = x) -> m1
  broom::glance(m1)
}

presences %>%
  group_by(Taxon) %>%
  do(glms.glance(.)) -> t2

### Get McFadden's pseudo R2 (rho2) https://stats.stackexchange.com/questions/82105/mcfaddens-pseudo-r2-interpretation

glms.mcfadden <- function(x) { 
  glm(cbind(Cover, 100 - Cover) ~ GDD, family = "binomial", data = x) -> mod
  glm(cbind(Cover, 100 - Cover) ~ 1, family = "binomial", data = x) -> nullmod
  data.frame(rho2 = 1 - logLik(mod) / logLik(nullmod)) #https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
}

presences %>%
  group_by(Taxon) %>%
  do(glms.mcfadden(.)) %>%
  mutate(rho2 = round(rho2, 2)) -> t3

# ### Scenarios
# 
# merge(
#   read.csv("results/bioclim.csv") %>%
#   group_by() %>%
#   select(GDD) %>%
#   filter(GDD == max(GDD) | GDD == min(GDD)),
#   read.csv("results/bioclim.csv") %>%
#   group_by() %>%
#   select(FDD) %>%
#   filter(FDD == max(FDD) | FDD == min(FDD))) %>%
#   unique -> scenarios
# 
# ### Model predictions https://www.statology.org/r-glm-predict/
# 
# glms.predicts <- function(x, s) { 
#   glm(Presence ~ GDD + FDD, family = "binomial", data = x) -> m1
#   predict(m1, s, type = "response") -> p1
#   data.frame(Predict = p1, GDD = s$GDD, FDD = s$FDD)
# }
# 
# presences %>%
#   group_by(Taxon) %>%
#   do(glms.predicts(., scenarios)) %>%
#   mutate(Scenario = paste(ifelse(GDD == max(GDD), "Hot", "Cold"),
#                            ifelse(FDD == max(FDD), "Frozen", "Snowy"),
#                            sep = " & "),
#          Predict = 100 * (round(Predict, 2))) %>%
#   select(Taxon, Scenario, Predict) %>%
#   spread(Scenario, Predict) -> t4

### Merge

t1 %>%
  merge(t2) %>%
  merge(t3)  -> models

### Species preferences

read.csv("data/spatial-survey-species.csv") %>% 
  merge(read.csv("results/supplement/S1 - Bioclimatic indices.csv")) %>%
  gather(Trait, Value, bio1:GDD) %>%
  group_by(Taxon, Trait) %>%
  summarise(Value = weighted.mean(Value, Cover)) %>%
  spread(Trait, Value) %>%
  merge(models, all.x = TRUE) %>%
  write.csv("results/species-preferences-cover.csv", row.names = FALSE)
