# required packages
library(maps)
library(ggplot2)
library(viridis)
library(terra)
library(tidyterra)
library(prettymapr)

load("data/valen/island_lon_lat.RData")
pac <- vect("data/spatial_data/map_shapefiles/pacific_map.shp")
afr <- vect("data/spatial_data/map_shapefiles/africa_map.shp")
ate <- vect("data/spatial_data/map_shapefiles/asia_temperate_map.shp")
atr <- vect("data/spatial_data/map_shapefiles/asia_tropical_map.shp")
aus <- vect("data/spatial_data/map_shapefiles/australia_map.shp")
eur <- vect("data/spatial_data/map_shapefiles/europe_map.shp")
nam <- vect("data/spatial_data/map_shapefiles/northern_america_map.shp")
sam <- vect("data/spatial_data/map_shapefiles/southern_america_map.shp")


# save plot for the pacific centred world map
map_pacific <- map("world2", fill = TRUE, col = "lightgrey")
map_pacific_data <- map_data(map_pacific)

ggplot(island_lon_lat, aes(x = new_lon, y = lat)) +
  geom_polygon(data = map_pacific_data, 
               aes(x = long, y = lat, group = group)) +
  geom_point(size = 5, alpha = 0.8) + 
  geom_spatvector(data = sam, aes(x = lon, y = lat)) 

ggplot(island_lon_lat, aes(x = new_lon, y = lat))

windows()
maps::map("world2", fill = TRUE, col = "grey100", ylim = c(-65,85), xlim = c(10,350))
addnortharrow(pos = "bottomleft", scale = 1, padin = c(0.4, 0.2))
map.axes()
plot(pac, col = "#E090A5", add = TRUE)
plot(afr, col = "plum4", add = TRUE)
plot(ate, col = "#CFDEE7", add = TRUE)
plot(atr, col = "#BFB48F", add = TRUE)
plot(aus, col = "#2D728F", add = TRUE)
plot(eur, col = "#FED766", add = TRUE)
plot(nam, col = "#904E55", add = TRUE)
plot(sam, col = "#83BCA9", add = TRUE)
graphics::points(island_lon_lat[,c("new_lon", "lat")],  pch = 23, bg = "#E090A5", lwd = 2, cex = 3) 
