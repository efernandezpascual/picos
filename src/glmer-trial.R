presences %>% filter(Taxon == "Festuca glacialis") -> spp

glm(Presence ~ GDD + FDD, family = "binomial", data = spp) -> m1
lme4::glmer(Presence ~ GDD + FDD + (1 | Site), family = "binomial", data = spp) -> m2
lme4::glmer(Presence ~ GDD + FDD + (1 | Plot/Site), family = "binomial", data = spp) -> m3

summary(m1)
summary(m2)
summary(m3)

presences %>% filter(Taxon == "Alchemilla catalaunica") -> spp

glm(Presence ~ GDD + FDD, family = "binomial", data = spp) -> m1
lme4::glmer(Presence ~ GDD + FDD + (1 | Site), family = "binomial", data = spp) -> m2
lme4::glmer(Presence ~ GDD + FDD + (1 | Plot/Site), family = "binomial", data = spp) -> m3

summary(m1)
summary(m2)
summary(m3)

presences %>% filter(Taxon == "Helianthemum canum") -> spp

glm(Presence ~ GDD + FDD, family = "binomial", data = spp) -> m1
lme4::glmer(Presence ~ GDD + FDD + (1 | Site), family = "binomial", data = spp) -> m2
lme4::glmer(Presence ~ GDD + FDD + (1 | Plot/Site), family = "binomial", data = spp) -> m3

summary(m1)
summary(m2)
summary(m3)

presences %>% filter(Taxon == "Galium pyrenaicum") -> spp

glm(Presence ~ GDD + FDD, family = "binomial", data = spp) -> m1
lme4::glmer(Presence ~ GDD + FDD + (1 | Site), family = "binomial", data = spp) -> m2
lme4::glmer(Presence ~ GDD + FDD + (1 | Plot/Site), family = "binomial", data = spp) -> m3

summary(m1)
summary(m2)
summary(m3)
