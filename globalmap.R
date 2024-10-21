
# using geojson to plot global map

library(tidyverse)
library(sf)

# load the world map
DataMap <- st_read("./Data/world.zh.json") |> 
     filter(iso_a3  != "ATA")

# plot the world map
ggplot() +
     geom_sf(data = DataMap, fill = 'grey50', color = 'black', alpha = 0.3) +
     # add point of interest
     geom_point(data = DataMap, aes(x = 116, y = 29), color = 'red', size = 1) +
     scale_x_continuous(limits = c(-180, 180),
                        expand = c(0, 0)) + 
     scale_y_continuous(limits = c(-60, 75)) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "white", color = NA),
           axis.text = element_blank(),
           axis.ticks = element_blank(),
           axis.title = element_blank(),
           plot.title.position = 'plot',
           legend.position = 'bottom')
