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
  mutate(ChangeP = 100*(`19` / `09` - 1)) %>%
  arrange(Change) %>%
  # filter(`09` > 10) %>%
  na.omit -> changespp

### Plot decreasing

changespp %>%
  mutate(Taxon = fct_recode(Taxon, 
                            "Ranunculus favargeri" = "Ranunculus parnassiifolius subsp. favargeri",
                            "Thymus ligusticus" = "Thymus praecox subsp. ligusticus",
                            "Helianthemum urrielense" = "Helianthemum apenninum subsp. urrielense",
                            "Festuca rubra" = "Festuca gr. rubra",
                            "Dethawia cantabrica" = "Dethawia splendens subsp. cantabrica")) %>%
  ggplot(aes(reorder(Taxon, -ChangeP), ChangeP, fill = ChangeP)) +
  geom_text(aes(label = `09`, y = -110)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_fill_gradient2(high = "turquoise4", low = "firebrick4", na.value = NA, midpoint = 0) +
  xlab(NULL) +
  ylab("Frequency change (%)") +
  ggthemes::theme_tufte() +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, face = "italic"), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 11, color = "black", face = "italic")) -> f1; f1

ggsave(f1, file = "results/figures/spp-changes.png", 
       path = NULL, scale = 1, width = 182, height = 145, units = "mm", dpi = 600)
