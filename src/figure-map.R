library(tidyverse); library(raster)

### Get map of Spain

spaingeo <- getData("GADM", country = "spain", level = 1)
spaindf <- fortify(spaingeo)

spainalt <- getData("alt", country = "spain", level = 1)
altdf  <- data.frame(rasterToPoints(spainalt))
colnames(altdf) = c("lon", "lat", "alt")
altdf %>% filter(alt < 2650) -> altdf

### Site coordinates

rbind(
read.csv("data/focal-plot-header.csv") %>%
  filter(Site %in% c("Hoyo Sin Tierra", 
                     "Los Boches", 
                     "Hou Sin Tierri",
                     "Los Cazadores")) %>%
  group_by(Site) %>%
  summarise(long = mean(Longitude), lat = mean(Latitude)) %>%
  mutate(Plot = "Focal"),
read.csv("data/ibutton-plot-header.csv") %>%
  group_by(Site, Plot) %>%
  summarise(long = mean(Longitude), lat = mean(Latitude)) %>%
  mutate(Plot = "Transect")) %>%
  mutate(Site = fct_relevel(Site, "El Cable", "Hoyo Sin Tierra", "Cabaña Verónica",
                            "Horcados Rojos", "Los Boches", "Hou Sin Tierri",
                            "Los Cazadores", "Urriellu")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) -> header

### Draw map of Spain

header %>%
  filter(Plot == "Focal") %>%
  ggplot(aes(long, lat)) +
  geom_tile(data = altdf, aes(x = lon, y = lat, fill = alt)) +
  scale_fill_gradient(name = "Elevation (m asl)", low = "grey96", high = "chocolate4",
                      breaks = c(0, 2600)) +
  geom_blank(data = spaindf, aes(x = long, y = lat)) +
  geom_map(data = spaindf, map = spaindf,
           aes(group = group, map_id = id),
           fill = NA, color = "black", size = .3) +
  ggrepel::geom_label_repel(aes(label = Site), box.padding = .6, size = 4,
                            family = "serif", fontface = "italic") +
  labs(x = "Longitude (º)", y = "Latitude (º)") +
  geom_point(size = 1, color = "black", fill = "#9a02db", shape = 22) +
  geom_text(aes(x = -5, y = 43.6, label = "Atlantic ocean"), size = 5, family = "serif") +
  geom_text(aes(x = -4.9, y = 43.35, label = "Asturies"), size = 5, family = "serif") +
  geom_text(aes(x = -4.95, y = 43.05, label = "León"), size = 5, family = "serif") +
  geom_text(aes(x = -4.5, y = 43.175, label = "Cantabria"), size = 5, family = "serif") +
  scale_x_continuous(limits = c(-5.2, -4.4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(43, 43.7), expand = c(0, 0)) +
  ggthemes::theme_tufte() +
  theme(panel.background = element_rect(color = "black", fill = "grey96"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.justification = "left",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, 0, 0),
        axis.text = element_blank(),
        axis.ticks = element_blank(),             
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        axis.title = element_blank()) -> g1

### Draw map of Europe

ggplotGrob(
  ggplot() +
    geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), 
                 color = "black", fill = "grey", size = 0.25) +
    geom_path(data = data.frame(long = c(-5.3, -5.3, -4.3, -4.3, -5.3),
                                lat = c(42.9, 43.8, 43.8, 42.9, 42.9)), 
              aes(x = long, y = lat), size = 0.25) +
    labs(x = "Longitude (º)", y = "Latitude (º)") +
    coord_cartesian(xlim = c(-8.8, 25), ylim = c(36.5, 58.5)) +
    ggthemes::theme_map() +
    theme(panel.background = element_rect(color = "grey96", fill = "grey96"),
          legend.position = "top", legend.title = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1))) -> g2

### Put together

g1 +
  annotation_custom(grob = g2, xmin = -4.75, xmax = -4.4,
                    ymin = 43.45, ymax = 43.7) -> plot1a

# ### Add pictures
# 
# img1 <- png::readPNG("map/Los Cazadores.png")
# img2 <- png::readPNG("map/Hou Sin Tierri.png")
# img3 <- png::readPNG("map/Los Boches.png")
# img4 <- png::readPNG("map/Hoyo Sin Tierra.png")
# 
# plot1b <- grid::rasterGrob(img1, img2, img3, img4, width = unit(85, "mm"), height = unit(85, "mm"), just = "centre")
# 
# cowplot::plot_grid(plot1a, plot1b, ncol = 2, labels = c("(a)", "(b)")) -> plot1


ggsave(plot1a, file = "results/f2.png", 
       path = NULL, scale = 1, width = 89, height = 89, units = "mm", dpi = 600)

