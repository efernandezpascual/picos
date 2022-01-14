library(tidyverse); library(raster); library(tibbletime); library(lubridate)

### Raster with elevation for Picos area

raster::raster("../#data/maps/picos/DEM/DEM.TIF") %>% # Map files are in my home drive
  rasterToPoints() %>%
  as.data.frame() %>%
  rename(lon = x, lat = y, alt = DEM) -> dem 

### Site coordinates
# prueba
read.csv("data/temporal-survey-header.csv") %>%
  group_by(Site) %>%
  summarise(long = mean(ED5030TX), lat = mean(ED5030TY)) %>%
  arrange(-lat) %>%
  rownames_to_column(var = "Label") -> tPlots

### Panel A - Map of Europe

rgdal::readOGR(dsn = "../#data/maps/WWF", 
               layer = "wwf_terr_ecos") -> Ecoregions # Map files are in my home drive
rownames(Ecoregions@data) -> Ecoregions@data$id
fortify(Ecoregions, region = "id") -> Ecoregions.points 
plyr::join(Ecoregions.points, Ecoregions@data, by = "id") %>%
  inner_join(read.csv("../#data/maps/Biomes.csv"), by = "BIOME") -> land

read.csv("data/temporal-survey-header.csv") %>%
  summarise(long = mean(Longitude), lat = mean(Latitude)) %>%
  ggplot(aes(long, lat)) +
  geom_polygon(data = land, aes(x = long, y = lat, group = group), 
               color = NA, fill = "gainsboro", size = 0.25, show.legend = FALSE) +
  ggstar::geom_star(size = 4, color = "black", fill = "#9a02db", starshape = 1) +
  labs(x = "Longitude (º)", y = "Latitude (º)") +
  coord_cartesian(xlim = c(-9, 21), ylim = c(36.5, 58.5)) +
  ggthemes::theme_tufte() +
  labs(title = "(A) Study area") +
  theme(panel.background = element_rect(color = NA, fill = "grey96"),
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
        axis.title = element_blank()) -> f1

### Panel B - Map of Picos

tPlots %>%
  ggplot(aes(long, lat)) +
  geom_tile(data = dem, aes(x = lon, y = lat, fill = alt)) +
  scale_fill_gradientn(name = "Elevation (m asl)", colours = terrain.colors(10), breaks = c(100, 2600)) + 
  annotate(geom = "text", x = 351936, y = 4785600, label = "1", hjust = 0, size = 3, family = "serif") +
  annotate(geom = "text", x = 352005, y = 4784363, label = "2", hjust = 0, size = 3, family = "serif") +
  annotate(geom = "text", x = 351877, y = 4783219, label = "3", hjust = 0, size = 3, family = "serif") +
  annotate(geom = "text", x = 352108, y = 4781461, label = "4", hjust = 0, size = 3, family = "serif") +
  labs(x = "Longitude (º)", y = "Latitude (º)") +
  geom_point(size = 2, color = "black", fill = "#9a02db", shape = 21) +
  scale_x_continuous(limits = c(343000, 356000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(4777000, 4790200), expand = c(0, 0)) +
  ggspatial::annotation_scale(height = unit(0.15, "cm"), text_family = "serif") +
  ggthemes::theme_tufte() +
  labs(title = "(B) Survey sites") +
  coord_fixed() +
  theme(plot.title = element_text(vjust = -.3),
        panel.background = element_rect(color = "black", fill = "grey96"),
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
        axis.title = element_blank()) -> f2

### Panel C - The Crosses

img1 <- png::readPNG("map/survey-design2.png")

f3 <- grid::rasterGrob(img1, width = unit(35, "mm"), height = unit(133, "mm"), just = "centre")

### Combine panels

cowplot::plot_grid(f1, f2, ncol = 1, rel_heights = c(57, 76)) -> plot1

cowplot::plot_grid(plot1, f3, ncol = 2, rel_widths = c(60, 35)) -> fig

### Save figure

ggsave(fig, file = "results/figures/survey-sites.png", 
       path = NULL, scale = 1, width = 95, height = 133, units = "mm", dpi = 600)
# ggsave(fig, file = "results/figures/survey-sites.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182, height = 133, units = "mm", dpi = 600, compression = "lzw")
