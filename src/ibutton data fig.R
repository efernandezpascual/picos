read.csv("data/ibutton-plot-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>% 
  merge(read.csv("data/ibutton-plot-header.csv")) %>%
  mutate(Site = fct_relevel(Site, "El Cable", "Hoyo Sin Tierra", "Cabaña Verónica",
                            "Horcados Rojos", "Los Boches", "Hou Sin Tierri",
                            "Los Cazadores", "Urriellu")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) %>%
  ggplot(aes(Time, Temperature, color = Plot)) + 
  facet_wrap(~ Site, nrow = 2) + 
  geom_line() +
  labs(title = "Microspatial variation in soil temperature (n = 20 iButtons per site)") +
  xlab("Time (4 h)") +
  ylab("Temperature (ºC)") +
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
        axis.text.x = element_text(size = 14, color = "black")) -> f1; f1

ggsave(f1, file = "results/ibutton data.png", 
       path = NULL, scale = 1, width = 330, height = 185, units = "mm", dpi = 600)
