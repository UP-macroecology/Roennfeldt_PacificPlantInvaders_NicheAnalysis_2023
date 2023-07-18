library(sf)
library(terra)
library(ggplot2)

rm(list = ls())

# pac <- rast("data/spatial_data/pacific_islands_rast.tif")
# pacific <- pac %>%
#   rotate(left = FALSE) %>% 
#   as.polygons(trunc = TRUE)
# 
# plot(pacific)
# 
# writeVector(pacific, "data/spatial_data/pacific_map.shp", overwrite = TRUE)


# mainland regions

# EUROPE
eur <- vect(tdwg[1,]) %>%
  rotate(left = FALSE)
plot(eur)

ext(eur)
# rotate(x, longitude=0, split=FALSE, left=TRUE, normalize=FALSE
# AFRICA
afr <- vect(tdwg[2,])
plot(afr)

# ASIA-TROPICAL

pac <- vect("data/spatial_data/pacific_map.shp")
tdwg <- st_read("data/tdwg/geojson/level1.geojson")[-9,] # without antarctic

tdwg2 <- tdwg %>%
  st_transform("EPSG:8859")
plot(tdwg2[,2:3])

pac_sf <- sf::st_as_sf(pac)
plot(pac_sf)

# change crs of tdwg to a Pacific focus : https://epsg.io/?q=Pacific

st_set_crs(tdwg$geometry, "EPSG:8859") #<- 8859
plot(tdwg[,2:3])


t <- st_transform(tdwg, crs = st_crs(8859))

plot(tdwg$geometry)

#london2 = st_transform(london_geo, "EPSG:27700")