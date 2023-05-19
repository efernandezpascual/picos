library(tidyverse)

read.csv("results/supplement/S1 - Bioclimatic indices.csv") -> bioclim

bioclim %>%
  gather(Trait, Value, bio1:GDD) %>%
  group_by(Trait, Site, Survey) %>%
  filter(Trait %in% c("GDD", "FDD")) %>%
  summarise(Delta = max(Value) - min(Value)) -> diffs

diffs %>%
  spread(Survey, Delta) %>%
  filter(Trait == "FDD") -> fdd

t.test(fdd$Spatial, fdd$Temporal, paired = TRUE, alternative = "greater")

diffs %>%
  spread(Survey, Delta) %>%
  filter(Trait == "GDD") -> gdd

t.test(gdd$Spatial, gdd$Temporal, paired = TRUE, alternative = "greater")

diffs %>%
  group_by(Trait, Survey) %>%
  summarise(Mean = mean(Delta), SE = sd(Delta)/sqrt(4)) %>%
  group_by() %>%
  mutate(Trait = fct_relevel(Trait, "GDD", "FDD")) %>%
  ggplot(aes(Survey, Mean, fill = paste(Trait, Survey))) +
  facet_wrap(~ Trait, scales = "free_y", nrow = 1) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.9) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width=.2,
                position=position_dodge(.9)) +
  xlab("Survey") +
  ylab(expression(paste("Degrees-day (absolute ºC)"))) +
  labs(title = "(A) Max differences spatial vs. temporal") +
  ggthemes::theme_tufte() +
  scale_fill_manual(values = c("deepskyblue", "deepskyblue", "brown1", "brown1")) +
  theme(strip.background = element_rect(colour = "grey96", fill = "grey96"),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        strip.text = element_text(size = 12),
        strip.placement = "ouside",
        axis.title.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"))  

### Indices trends figure

read.csv("results/supplement/S1 - Bioclimatic indices.csv") %>%
  filter(Survey == "Temporal") %>%
  group_by(Plot) %>%
  summarise(FDD = mean(FDD), GDD = mean(GDD)) %>%
  gather(Trait, Value, GDD:FDD) %>%
  mutate(Trait = fct_relevel(Trait, "GDD", "FDD")) %>%
  ggplot(aes(as.numeric(Plot), Value, fill = Trait)) +
  facet_wrap(~ Trait, nrow = 2, scales = "free_y") +
  geom_area(alpha = 0.9, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "(B) Temporal trends") +
  xlab("Year") +
  ylab(expression(paste("Degrees-day (absolute ºC)"))) +
  ggthemes::theme_tufte() +
  scale_fill_manual(values = c("brown1", "deepskyblue")) +
  scale_x_continuous(breaks = seq(2009, 2018, by = 1)) +
  theme(strip.background = element_rect(colour = "grey96", fill = "grey96"),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        strip.text = element_text(size = 12),
        strip.placement = "ouside",
        axis.title.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 10, color = "black")) 

### Merge

cowplot::plot_grid(f3a, f3b) -> f3

### Join

ggsave(f3, file = "results/figures/F3 - Space vs time.png", 
       path = NULL, scale = 1, width = 182, height = 90, units = "mm", dpi = 600)


# clara version for appendix
#### clara added ###
read.csv("results/supplement/S1 - Bioclimatic indices filtered clara.csv",sep = ",") -> bioclim
bioclim %>%
  gather(Trait, Value, bio1:GDD) %>%
  group_by(Trait, Site, Survey) %>%
  summarise(Delta = max(Value) - min(Value))-> diffs_cl
str(diffs_cl)
summary(diffs_cl)

diffs_cl %>%
  group_by(Trait, Survey)%>%
  summarise(mean = mean(Delta), se = sd(Delta))

diffs_cl %>%
  spread(Survey, Delta) %>%
  filter(Trait == "bio1") -> bio1

t.test(bio1$Spatial, bio1$Temporal, paired = TRUE, alternative = "greater")

diffs_cl %>%
  spread(Survey, Delta) %>%
  filter(Trait == "bio2") -> bio2

t.test(bio2$Spatial, bio2$Temporal, paired = TRUE, alternative = "greater")

diffs_cl %>%
  spread(Survey, Delta) %>%
  filter(Trait == "bio7") -> bio7

t.test(bio7$Spatial, bio7$Temporal, paired = TRUE, alternative = "greater")

diffs_cl %>%
  spread(Survey, Delta) %>%
  filter(Trait == "Snw") -> snow

t.test(snow$Spatial, snow$Temporal, paired = TRUE, alternative = "greater")

diffs_cl %>%
  spread(Survey, Delta) %>%
  filter(Trait == "GDD") -> GDD

t.test(GDD$Spatial, GDD$Temporal, paired = TRUE, alternative = "greater")

diffs_cl %>%
  spread(Survey, Delta) %>%
  filter(Trait == "FDD") -> FDD

t.test(FDD$Spatial, FDD$Temporal, paired = TRUE, alternative = "greater")

read.csv("results/supplement/S1 - Bioclimatic indices filtered clara.csv") %>%
  filter(Survey == "Temporal") %>%
  group_by(Plot) %>%
  summarise(FDD = mean(FDD), GDD = mean(GDD), Snow = mean(Snw)) %>%
  gather(Trait, Value, Snow:FDD) %>%
  mutate(Trait = fct_relevel(Trait, "GDD", "FDD", "Snow")) %>%
  ggplot(aes(as.numeric(Plot), Value, fill = Trait)) +
  facet_wrap(~ Trait, nrow = 3, scales = "free_y") +
  geom_area(alpha = 0.9, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Temporal trends") +
  xlab("Year") +
  ylab(expression(paste("Number of days                             Degrees-day (absolute ºC)"))) +
  ggthemes::theme_tufte() +
  scale_fill_manual(values = c("brown1", "deepskyblue", "grey")) +
  scale_x_continuous(breaks = seq(2009, 2018, by = 1)) +
  theme(strip.background = element_rect(colour = "grey96", fill = "grey96"),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        strip.text = element_text(size = 12),
        strip.placement = "ouside",
        axis.title.x = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 8, color = "black"),
        axis.text.y = element_text(size = 10, color = "black")) -> S1

ggsave(S1, file = "results/figures/S1 - trends.png", 
       path = NULL, scale = 1, width = 182, height = 90, units = "mm", dpi = 600)

