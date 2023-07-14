### Species changes

read.csv("data/temporal-survey-matrices.csv") %>%
  group_by(Plot, Taxon) %>%
  summarise(Cover = sum(Present)) %>%
  merge(read.csv("data/species-checklist.csv", sep = ";"), all.x = TRUE) %>%
  filter(Lifeform != "Therophyte") %>%
  separate(Plot, into = c("Plot", "Year"), sep = "-") %>%
  mutate(Year = fct_recode(Year, "09" = "08")) %>%
  group_by(Taxon, Year) %>%
  summarise(Cover = sum(Cover)) %>%
  spread(Year, Cover) %>%
  mutate(Change = `19` - `09`) %>%
  mutate(ChangeP = 100*(`19` / `09` - 1)) %>%
  arrange(Change) %>%
  filter(`09` > 10) %>% # filter less than 10 initial appearances
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
  theme(text = element_text(family = "sans", size = 20),
        strip.background = element_blank(),
        legend.position = "none", 
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text = element_text(size = 18, face = "italic"), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black", face = "italic")) -> F4; F4

ggsave(F4, file = "results/figures/clara changes/F4(3).png", 
       path = NULL, scale = 1, width = 225, height = 145, units = "mm", dpi = 600)

# figure filtered with less than 10 initial observation + images
library(cowplot)
library(magick)
library(png)
Arm <- png::readPNG("images/Arm.png", native= TRUE)
Hel <- png::readPNG("images/Hel.png", native= TRUE)
Min <- png::readPNG("images/Min.png", native= TRUE)
Poa <- png::readPNG("images/Poa.png", native= TRUE)
Sax <- png::readPNG("images/Sax.png", native= TRUE)
Sal <- png::readPNG("images/Sal.png", native= TRUE)

x11()
ggdraw()+
  draw_plot(F4) +
  draw_image (Arm, scale= 0.2, hjust = -0.225, vjust = -0.35)+
  draw_label ("Armeria cantabrica", size = 10, hjust = -2.1, vjust = -15.5, fontface = "italic", color = "black")+
  draw_image (Min, scale = 0.2, hjust = -0.4, vjust = -0.35)+
  draw_label ("Minuartia verna", size = 10,  hjust = -5.2, vjust = -15.5, fontface = "italic", color = "black")+
  draw_image (Poa, scale = 0.2, hjust = -0.225, vjust = -0.125)+
  draw_label ("Poa alpina", size = 10, hjust = -4.6, vjust = -0.75, fontface = "italic", color = "black")+
  draw_image (Hel, scale = 0.2, hjust = -0.40, vjust= -0.125) +
  draw_label ("Helianthemum urrielense", size = 10, hjust = -2.78, vjust = -0.75, fontface = "italic", color = "black")+
  draw_image (Sal, scale = 0.2, hjust = -0.225, vjust= 0.1) +
  draw_label ("Salix breviserrata", size = 10, hjust = -2.45, vjust = 14.25, fontface = "italic", color = "black")+
  draw_image (Sax, scale = 0.2, hjust = -0.40, vjust= 0.1) +
  draw_label ("Saxifraga conifera", size = 10, hjust = -4.2, vjust = 14.25, fontface = "italic", color = "black")+
  draw_label ("Losers", size = 12, hjust =-4.25, vjust = -25.75, fontface = "bold", color = "firebrick")+ 
  draw_label ("Winners", size = 12, hjust =-6.5, vjust = -25.75, fontface = "bold", color = "deepskyblue") -> F4_img;F4_img

ggsave(F4_img, file = "results/figures/F4_img.png", 
       path = NULL, scale = 1, width = 225, height = 145, units = "mm", dpi = 600)

