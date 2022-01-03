library(tidyverse); library(raster); library(tibbletime); library(lubridate)

### Raster with elevation for Picos area

raster::raster("../#data/maps/picos/DEM/DEM.TIF") %>% # Map files are in my home drive
  rasterToPoints() %>%
  as.data.frame() %>%
  rename(lon = x, lat = y, alt = DEM) -> dem 

### Site coordinates

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

img1 <- png::readPNG("map/survey-design.png")

f3 <- grid::rasterGrob(img1, width = unit(120, "mm"), height = unit(43, "mm"), just = "centre")

### Panel D - The Climographs

read.csv("data/temporal-survey-temperatures.csv") %>%
  mutate(Time = as.POSIXct(Time, tz = "UTC")) %>%
  as_tbl_time(index = Time) %>%
  arrange(Time) %>%
  filter_time("2009" ~ "2018") %>%
  mutate(m = month(Time)) %>%
  group_by(Site, m) %>%
  summarise(T = mean(Temperature)) -> ts # Monthly average temperature

read.csv("data/temporal-survey-wp.csv") %>%
  mutate(m = month(Time)) %>%
  group_by(Site, m) %>%
  summarise(WP = mean(WP)) -> wp # Monthly average water potential

merge(ts, wp) %>%
  mutate(Site = fct_relevel(Site,
                            "Los Cazadores", "Hou Sin Tierri",
                            "Los Boches", "Hoyo Sin Tierra")) %>%
  mutate(Site = fct_recode(Site, "Ḥou Sin Tierri" = "Hou Sin Tierri")) %>%
  ggplot(aes(x = m)) +
  facet_wrap(~ Site, nrow = 1) +
  geom_bar(aes(y = -WP*10), stat = "identity", position = "dodge", fill = "dodgerblue4", size = 1.1) +
  geom_line(aes(y = T), color = "darkred", size = .5) +
  geom_point(aes(y = T), color = "darkred", size = .7) +
  scale_y_continuous(sec.axis = sec_axis( trans = ~./10, name = "Mean monthly soil water potential (-MPa)")) +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(hjust = -.15, vjust = -1.5),
        axis.ticks.y = element_line(colour = "darkred"),
        axis.ticks.y.right = element_line(colour = "dodgerblue4"),
        axis.text.y = element_text(colour = "darkred"),
        axis.text.y.right = element_text(colour = "dodgerblue4"),
        axis.title.y = element_text(colour = "darkred"),
        axis.title.y.right = element_text(colour = "dodgerblue4"),
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        strip.text = element_text(size = 10, color = "black"),
        axis.text = element_blank(),
        axis.title = element_text(size = 10),
        legend.position = "none", strip.placement = "outside",
        plot.margin = unit(c(0, 0.1, 0, 0), "cm")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Calendar month (January to December)", y = "Mean monthly soil temperature (ºC)") +
  labs(title = "(D) Site soil climate") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12), ) -> f4; f4

### Combine panels

cowplot::plot_grid(f1, f2, ncol = 1, rel_heights = c(57, 76)) -> plot1
cowplot::plot_grid(f3, f4, ncol = 1, rel_heights = c(43, 88)) -> plot2

cowplot::plot_grid(plot1, plot2, ncol = 2, rel_widths = c(60, 120)) -> fig

### Save figure

ggsave(fig, file = "results/figures/survey-sites.png", 
       path = NULL, scale = 1, width = 182, height = 133, units = "mm", dpi = 600)
# ggsave(fig, file = "results/figures/survey-sites.tiff", device = grDevices::tiff, 
#        path = NULL, scale = 1, width = 182, height = 133, units = "mm", dpi = 600, compression = "lzw")
