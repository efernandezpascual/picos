library(tidyverse); library(raster); library(tibbletime); library(lubridate); library(ggpubr)

### Raster with elevation for Picos area

raster::raster("../#data/maps/picos/DEM/DEM.TIF") %>% # Map files are in my home drive
  rasterToPoints() %>%
  as.data.frame() %>%
  rename(lon = x, lat = y, alt = DEM) -> dem

### Raster with elevation for Picos area

# raster::raster("C:/Users/Usuario/OneDrive - Universidad de Oviedo/IMIB/Softwares/GitHub/picos/data/picos/DEM/DEM.TIF") %>% # Map files are in my home drive
#   rasterToPoints() %>%
#   as.data.frame() %>%
#   rename(lon = x, lat = y, alt = DEM) -> dem 


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
# rgdal::readOGR(dsn = "data/maps/WWF", 
#                layer = "wwf_terr_ecos") -> Ecoregions # Map files are in my home drive
rownames(Ecoregions@data) -> Ecoregions@data$id
fortify(Ecoregions, region = "id") -> Ecoregions.points 
plyr::join(Ecoregions.points, Ecoregions@data, by = "id") %>%
  inner_join(read.csv("../#data/maps/Biomes.csv"), by = "BIOME") -> land
# plyr::join(Ecoregions.points, Ecoregions@data, by = "id") %>%
#   inner_join(read.csv("data/maps/Biomes.csv"), by = "BIOME") -> land

read.csv("data/temporal-survey-header.csv") %>%
  summarise(long = mean(Longitude), lat = mean(Latitude)) %>%
  ggplot(aes(long, lat)) +
  geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), 
               color = "black", fill = "gainsboro", size = 0.25, show.legend = FALSE) +
  ggstar::geom_star(size = 4, color = "black", fill = "#9a02db", starshape = 1) +
  labs(x = "Longitude (º)", y = "Latitude (º)") +
  coord_cartesian(xlim = c(-9, 21), ylim = c(36.5, 58.5)) +
  ggthemes::theme_tufte() +
  labs(title = "(A) Study area") +
  theme(text = element_text(family = "sans"),
        panel.background = element_rect(color = "black", fill = NULL),
        panel.border = element_rect(color = "black", fill = NA),
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
        axis.title = element_blank()) -> f1a;  f1a

### Panel B - Map of Picos

tPlots %>%
  ggplot(aes(long, lat)) +
  geom_tile(data = dem, aes(x = lon, y = lat, fill = alt)) +
  scale_fill_gradientn(name = "m asl", colours = terrain.colors(10), breaks = c(100, 2600)) + 
  annotate(geom = "text", x = 351936, y = 4785600, label = "1", hjust = 0, size = 3, family = "serif") +
  annotate(geom = "text", x = 352005, y = 4784363, label = "2", hjust = 0, size = 3, family = "serif") +
  annotate(geom = "text", x = 351877, y = 4783219, label = "3", hjust = 0, size = 3, family = "serif") +
  annotate(geom = "text", x = 352108, y = 4781461, label = "4", hjust = 0, size = 3, family = "serif") +
  labs(x = "Longitude (º)", y = "Latitude (º)") +
  geom_point(size = 2, color = "black", fill = "#9a02db", shape = 21) +
  scale_x_continuous(limits = c(343000, 356000), expand = c(0, 0)) +
  scale_y_continuous(limits = c(4777000, 4790200), expand = c(0, 0)) +
  ggspatial::annotation_scale(height = unit(0.15, "cm"), text_family = "sans") +
  ggthemes::theme_tufte() +
  labs(title = "(B) Survey sites") +
  coord_fixed() +
  theme(text = element_text(family = "sans"),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(color = "black", fill = NULL),
        plot.title = element_text(vjust = -.3),
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
        axis.title = element_blank()) -> f1b; f1b

### Panel C - The Crosses

img1 <- png::readPNG("map/fondo.png")
g <- grid::rasterGrob(img1, interpolate=TRUE)

read.csv("data/spatial-survey-header.csv") %>%
  filter(Site == "Hoyo Sin Tierra") %>%
  dplyr::select(ED5030TX, ED5030TY) %>%
  mutate(Survey = "Spatial") -> spplots

spplots %>%
  summarise(ED5030TX = mean(ED5030TX), 
            ED5030TY = mean(ED5030TY)) %>%
  mutate(Survey = "Temporal") %>%
  rbind(spplots) -> cpplots

cpplots %>%
  ggplot(aes(ED5030TX, ED5030TY)) +
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_point(aes(fill = Survey), shape = 22, size = 3) +
  ggthemes::theme_tufte() +
  labs(title = "(C) Survey plots") +
  scale_fill_manual(values = c("yellow", "purple")) +
  coord_fixed() +
  guides(fill = guide_legend(ncol = 2)) +
  theme(text = element_text(family = "sans"),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(color = "black", fill = NULL),
        plot.title = element_text(vjust = -.3),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = NA),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.justification = "left",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, 0, 0),
        axis.text = element_blank(),
        axis.ticks = element_blank(),             
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        axis.title = element_blank()) -> f1c; f1c


### Combine panels

cowplot::plot_grid(f1a, f1b, f1c,  ncol = 3) -> f1abc
#cowplot::plot_grid(f1abc, f1d,  ncol = 1) -> F1
ggarrange(f1abc, f1d, nrow = 2, heights = c(1.5,2.5))->F1; F1
# x11()
### Save figure

ggsave(F1, file = "results/figures/F1.png", 
       path = NULL, scale = 1, width = 185, height = 160, units = "mm", dpi = 600)
# ggsave(fig, file = "results/figures/survey-sites.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182, height = 133, units = "mm", dpi = 600, compression = "lzw")
