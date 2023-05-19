library(tidyverse)
####Edu's version ####
### Prepare data

read.csv("data/spatial-survey-species.csv") %>%
  group_by(Taxon) %>%
  mutate(n = length(Taxon)) %>%
  filter(n >= 10) %>%
  mutate(Presence = 1) %>%
  select(Plot, Taxon, Presence) %>%
  spread(Taxon, Presence, fill = 0) %>%
  gather(Taxon, Presence, -Plot) %>%
  merge(read.csv("results/supplement/S1 - Bioclimatic indices.csv")) -> presences

### Get GLM coefficients

glms <- function(x) {
  glm(Presence ~ GDD + FDD, family = "binomial", data = x) -> m1
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
         `FDD estimate`, `FDD std.error`, `FDD statistic`, `FDD p.value`, 
         `GDD estimate`, `GDD std.error`, `GDD statistic`, `GDD p.value`) -> t1

### Get GLM summary statistics

glms.glance <- function(x) {
  glm(Presence ~ GDD + FDD, family = "binomial", data = x) -> m1
  broom::glance(m1)
}

presences %>%
  group_by(Taxon) %>%
  do(glms.glance(.)) -> t2

### Get McFadden's pseudo R2 (rho2) https://stats.stackexchange.com/questions/82105/mcfaddens-pseudo-r2-interpretation

glms.mcfadden <- function(x) { 
  glm(Presence ~ GDD + FDD, family = "binomial", data = x) -> mod
  glm(Presence ~ 1, family = "binomial", data = x) -> nullmod
  data.frame(rho2 = 1 - logLik(mod) / logLik(nullmod)) #https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
}

presences %>%
  group_by(Taxon) %>%
  do(glms.mcfadden(.)) %>%
  mutate(rho2 = round(rho2, 2)) -> t3

### Scenarios

merge(
read.csv("results/supplement/S1 - Bioclimatic indices.csv") %>%
  select(Survey, GDD) %>%
  filter(Survey == "Temporal") %>%
  # group_by(Survey) %>%
  filter(GDD == max(GDD) | GDD == min(GDD)),
read.csv("results/supplement/S1 - Bioclimatic indices.csv") %>%
  select(Survey, FDD) %>%
  filter(Survey == "Temporal") %>%
  # group_by(Survey) %>%
  filter(FDD == max(FDD) | FDD == min(FDD)) %>%
  unique) -> scenarios

### Model predictions https://www.statology.org/r-glm-predict/

glms.predicts <- function(x, s) { 
  glm(Presence ~ GDD + FDD, family = "binomial", data = x) -> m1
  predict(m1, s, type = "response") -> p1
  data.frame(Predict = p1, GDD = s$GDD, FDD = s$FDD)
}

presences %>%
  group_by(Taxon) %>%
  do(glms.predicts(., scenarios)) %>%
  mutate(Scenario = paste(ifelse(GDD == max(GDD), "Hot", "Cold"),
                           ifelse(FDD == max(FDD), "Freezing", "Snowy"),
                           sep = " & "),
         Predict = 100 * (round(Predict, 2))) %>%
  select(Taxon, Scenario, Predict) %>%
  spread(Scenario, Predict) -> t4

### Changes 2009 to 2018

read.csv("data/temporal-survey-matrices.csv") %>%
  group_by(Plot, Taxon) %>%
  summarise(Cover = sum(Present)) %>%
  merge(read.csv("data/species-checklist.csv"), all.x = TRUE) %>%
  # filter(Lifeform != "Therophyte") %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  group_by(Taxon, Year) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Year, Cover, fill = 0) %>%
  mutate(`09` = round(((`09` / 800) * 100), 1)) %>%
  mutate(`19` = round(((`19` / 800) * 100), 1)) %>%
  mutate(`Frequency change '09 to '18` = round(`19` - `09`, 1)) %>%
  rename(`Frequency 2009` = `09`,
         `Frequency 2018` = `19`) -> changes

### Merge

t1 %>%
  merge(t2) %>%
  merge(t3) %>%
  merge(t4) %>%
  merge(changes, all.x = TRUE) %>%
  mutate(`Frequency 2009` = ifelse(is.na(`Frequency 2009`), "-", `Frequency 2009`),
         `Frequency 2018` = ifelse(is.na(`Frequency 2018`), "-", `Frequency 2018`),
         `Frequency change '09 to '18` = ifelse(is.na(`Frequency change '09 to '18`), "-", `Frequency change '09 to '18`)) -> models

### Save supplementary

write.csv(models, "results/supplement/S3 - Output of the GLM models of species presence.csv", row.names = FALSE)

### Table 1 - GLMS

models %>%
  filter(`FDD p.value` < 0.05 | `GDD p.value` < 0.05) %>%
  filter(rho2 > 0.15) %>%
  rename(`FDD p` = `FDD p.value`,
         `GDD p` = `GDD p.value`) %>%
  mutate(`FDD p` = round(`FDD p`, 3),
         `GDD p` = round(`GDD p`, 3),
         `FDD p` = ifelse(`FDD p` == 0, "<0.001", `FDD p`),
         `GDD p` = ifelse(`GDD p` == 0, "<0.001", `GDD p`),
         `FDD estimate` = round(`FDD estimate`, 2),
         `GDD estimate` = round(`GDD estimate`, 2)) %>%
  select(Taxon, `GDD estimate`, `GDD p`, `FDD estimate`, `FDD p`,
         rho2, `Cold & Freezing`:`Hot & Snowy`, `Frequency 2009`, `Frequency 2018`) %>%
  write.csv("results/tables/T1 - Summary of the GLM models of species presence.csv", row.names = FALSE)

### Figure 5 extinctions

models %>%
  filter(`FDD p.value` < 0.05 | `GDD p.value` < 0.05) %>%
  filter(rho2 > 0.15) %>%
  select(Taxon, `Cold & Freezing`:`Hot & Snowy`) %>%
  gather(Scenario, Predict, `Cold & Freezing`:`Hot & Snowy`) %>%
  filter(Predict == 0) %>%
  group_by(Scenario) %>%
  tally %>%
  mutate(Scenario = gsub(" & ", "\n", Scenario)) %>%
  ggplot(aes(reorder(Scenario, n), n, fill = Scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  xlab("Scenario") +
  ylab("Number of species extinctions") +
  ggthemes::theme_tufte() +
  scale_fill_manual(values = c("slateblue3", "deepskyblue", "firebrick", "indianred")) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 11, color = "black")) -> f5; f5
rm(f5)
### Save figure

ggsave(f5, file = "results/figures/F5 - Extinctions.png", 
       path = NULL, scale = 1, width = 150/2, height = 150/2, units = "mm", dpi = 600)

### Figure 5 extinctions CLARA
str(models)
level_order <- c("Cold & Snowy", "Cold & Freezing",
                 "Hot & Snowy", "Hot & Freezing")
models %>%
  filter(`FDD p.value` < 0.05 | `GDD p.value` < 0.05) %>%
  filter(rho2 > 0.15) %>%
  dplyr::select(Taxon, `Cold & Freezing`:`Hot & Snowy`) %>%
  gather(Scenario, Predict, `Cold & Freezing`:`Hot & Snowy`) %>%
  #filter(Predict == 0) %>%
  filter(Predict <5) %>%
  #mutate(Scenario = factor(Scenario, levels = c("Cold & Snowy", "Cold & Freezing","Hot & Snowy", "Hot & Freezing"))) %>%
  group_by(Scenario) %>%
  tally %>%
  mutate(Scenario = gsub(" & ", "\n", Scenario)) %>%
  mutate(Scenario = factor(Scenario, levels = c("Cold\nSnowy", "Cold\nFreezing","Hot\nSnowy", "Hot\nFreezing"))) %>%
  ggplot(aes(Scenario, n, fill = Scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "(B) 95% threshold")+
  xlab("Scenario") +
  ylab("Local extinctions") +
  ggthemes::theme_tufte() +
  scale_fill_manual(values = c("deepskyblue","slateblue3", "indianred","firebrick")) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 11, color = "black")) -> F5B; F5B

### Save figure
cowplot::plot_grid(F5A, F5B,  ncol = 2) -> F5
ggsave(F5, file = "results/figures/F5.png", 
       path = NULL, scale = 1, width = 200, height = 120, units = "mm", dpi = 600)

#### clara version: change scenarios and GDD values >5 ####
### Prepare data

read.csv("data/spatial-survey-species.csv") %>%
  group_by(Taxon) %>%
  mutate(n = length(Taxon)) %>%
  filter(n >= 10) %>%
  mutate(Presence = 1) %>%
  select(Plot, Taxon, Presence) %>%
  spread(Taxon, Presence, fill = 0) %>%
  gather(Taxon, Presence, -Plot) %>%
  merge(read.csv("results/supplement/S1 - Bioclimatic indices no filtered clara.csv")) -> presences

### Get GLM coefficients

glms <- function(x) {
  glm(Presence ~ GDD + FDD, family = "binomial", data = x) -> m1
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
         `FDD estimate`, `FDD std.error`, `FDD statistic`, `FDD p.value`, 
         `GDD estimate`, `GDD std.error`, `GDD statistic`, `GDD p.value`) -> t1

### Get GLM summary statistics

glms.glance <- function(x) {
  glm(Presence ~ GDD + FDD, family = "binomial", data = x) -> m1
  broom::glance(m1)
}

presences %>%
  group_by(Taxon) %>%
  do(glms.glance(.)) -> t2

### Get McFadden's pseudo R2 (rho2) https://stats.stackexchange.com/questions/82105/mcfaddens-pseudo-r2-interpretation

glms.mcfadden <- function(x) { 
  glm(Presence ~ GDD + FDD, family = "binomial", data = x) -> mod
  glm(Presence ~ 1, family = "binomial", data = x) -> nullmod
  data.frame(rho2 = 1 - logLik(mod) / logLik(nullmod)) #https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
}

presences %>%
  group_by(Taxon) %>%
  do(glms.mcfadden(.)) %>%
  mutate(rho2 = round(rho2, 2)) -> t3

### Scenarios

merge(
  read.csv("results/supplement/S1 - Bioclimatic indices no filtered clara.csv") %>%
    select(Survey, GDD) %>%
    filter(Survey == "Temporal") %>%
    # group_by(Survey) %>%
    filter(GDD == max(GDD) | GDD == min(GDD)),
  read.csv("results/supplement/S1 - Bioclimatic indices filtered clara.csv") %>%
    select(Survey, FDD) %>%
    filter(Survey == "Temporal") %>%
    # group_by(Survey) %>%
    filter(FDD == max(FDD) | FDD == min(FDD)) %>%
    unique) -> scenarios

### Model predictions https://www.statology.org/r-glm-predict/

glms.predicts <- function(x, s) { 
  glm(Presence ~ GDD + FDD, family = "binomial", data = x) -> m1
  predict(m1, s, type = "response") -> p1
  data.frame(Predict = p1, GDD = s$GDD, FDD = s$FDD)
}

presences %>%
  group_by(Taxon) %>%
  do(glms.predicts(., scenarios)) %>%
  mutate(Scenario = paste(ifelse(GDD == max(GDD), "Hot", "Cold"),
                          ifelse(FDD == max(FDD), "Freezing", "Snowy"),
                          sep = " & "),
         Predict = 100 * (round(Predict, 2))) %>%
  select(Taxon, Scenario, Predict) %>%
  spread(Scenario, Predict) -> t4

### Changes 2009 to 2018

read.csv("data/temporal-survey-matrices.csv") %>%
  group_by(Plot, Taxon) %>%
  summarise(Cover = sum(Present)) %>%
  merge(read.csv("data/species-checklist.csv"), all.x = TRUE) %>%
  # filter(Lifeform != "Therophyte") %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  group_by(Taxon, Year) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Year, Cover, fill = 0) %>%
  mutate(`09` = round(((`09` / 800) * 100), 1)) %>%
  mutate(`19` = round(((`19` / 800) * 100), 1)) %>%
  mutate(`Frequency change '09 to '18` = round(`19` - `09`, 1)) %>%
  rename(`Frequency 2009` = `09`,
         `Frequency 2018` = `19`) -> changes

### Merge

t1 %>%
  merge(t2) %>%
  merge(t3) %>%
  merge(t4) %>%
  merge(changes, all.x = TRUE) %>%
  mutate(`Frequency 2009` = ifelse(is.na(`Frequency 2009`), "-", `Frequency 2009`),
         `Frequency 2018` = ifelse(is.na(`Frequency 2018`), "-", `Frequency 2018`),
         `Frequency change '09 to '18` = ifelse(is.na(`Frequency change '09 to '18`), "-", `Frequency change '09 to '18`)) -> models

### Save supplementary

write.csv(models, "results/supplement/S3 - Output of the GLM models of species presence clara.csv", row.names = FALSE)

### Table 1 - GLMS

models %>%
  filter(`FDD p.value` < 0.05 | `GDD p.value` < 0.05) %>%
  filter(rho2 > 0.15) %>%
  rename(`FDD p` = `FDD p.value`,
         `GDD p` = `GDD p.value`) %>%
  mutate(`FDD p` = round(`FDD p`, 3),
         `GDD p` = round(`GDD p`, 3),
         `FDD p` = ifelse(`FDD p` == 0, "<0.001", `FDD p`),
         `GDD p` = ifelse(`GDD p` == 0, "<0.001", `GDD p`),
         `FDD estimate` = round(`FDD estimate`, 2),
         `GDD estimate` = round(`GDD estimate`, 2)) %>%
  select(Taxon, `GDD estimate`, `GDD p`, `FDD estimate`, `FDD p`,
         rho2, `Cold & Freezing`:`Hot & Snowy`, `Frequency 2009`, `Frequency 2018`) %>%
  write.csv("results/tables/T1 - Summary of the GLM models of species presence clara.csv", row.names = FALSE)

### Figure 5 extinctions CLARA
str(models)
level_order <- c("Cold & Snowy", "Cold & Freezing",
                 "Hot & Snowy", "Hot & Freezing")
models %>%
  filter(`FDD p.value` < 0.05 | `GDD p.value` < 0.05) %>%
  filter(rho2 > 0.15) %>%
  dplyr::select(Taxon, `Cold & Freezing`:`Hot & Snowy`) %>%
  gather(Scenario, Predict, `Cold & Freezing`:`Hot & Snowy`) %>%
  filter(Predict == 0) %>% # 100 % threshold
  #filter(Predict <=5) %>% #95% threshold
  #mutate(Scenario = factor(Scenario, levels = c("Cold & Snowy", "Cold & Freezing","Hot & Snowy", "Hot & Freezing"))) %>%
  group_by(Scenario) %>%
  tally %>%
  mutate(Scenario = gsub(" & ", "\n", Scenario)) %>%
  mutate(Scenario = factor(Scenario, levels = c("Cold\nSnowy", "Cold\nFreezing","Hot\nSnowy", "Hot\nFreezing"))) %>%
  ggplot(aes(Scenario, n, fill = Scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "(A) 0% Occurrence")+
  xlab("Scenario") +
  ylab("Local extinctions") +
  ggthemes::theme_tufte() +
  scale_fill_manual(values = c("deepskyblue","slateblue3", "indianred","firebrick")) +
  scale_y_continuous(limits =c (0,10) ,breaks = seq(0, 10, by = 1)) +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 11, color = "black")) ->  F5A; F5A

### Save figure
cowplot::plot_grid(F5A, F5B,  ncol = 2) -> F5

ggsave(F5, file = "results/figures/F5.png", 
       path = NULL, scale = 1, width = 200, height = 120, units = "mm", dpi = 600)
