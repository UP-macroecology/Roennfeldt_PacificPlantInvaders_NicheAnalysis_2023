library(terra)
library(maps)
library(dplyr)
# library(ggplot2)
library(prettymapr)

rm(list = ls())

chelsa <- rast("Z:/Arbeit/datashare/data/envidat/biophysical/CHELSA_V2/global/CHELSA_pr_01_1980_V.2.1.tif")
values(chelsa) <- 1 

# load in vector
pac <- vect("data/spatial_data/pacific_islands.shp")
afr <- vect("data/spatial_data/africa.shp")
ate <- vect("data/spatial_data/asia_temperate.shp")
atr <- vect("data/spatial_data/asia_tropical.shp")
aus <- vect("data/spatial_data/australasia.shp")
eur <- vect("data/spatial_data/europe.shp")
nam <- vect("data/spatial_data/northern_america.shp")
sam <- vect("data/spatial_data/southern_america.shp")

# rasterize vector and rotate to be focused on the pacific
pac2 <- rasterize(pac, chelsa, touches = TRUE) %>%
  rotate(left = FALSE) %>% 
  as.polygons(trunc = TRUE)

writeVector(pac2, "data/spatial_data/map_shapefiles/pacific_map.shp", overwrite = TRUE)

afr2 <- rasterize(afr, chelsa, touches = TRUE) %>%
  rotate(left = FALSE) %>% 
  as.polygons(trunc = TRUE)
writeVector(afr2, "data/spatial_data/map_shapefiles/africa_map.shp", overwrite = TRUE)

ate2 <- rasterize(ate, chelsa, touches = TRUE) %>%
  rotate(left = FALSE) %>% 
  as.polygons(trunc = TRUE)
writeVector(ate2, "data/spatial_data/map_shapefiles/asia_temperate_map.shp", overwrite = TRUE)

atr2 <- rasterize(atr, chelsa, touches = TRUE) %>%
  rotate(left = FALSE) %>% 
  as.polygons(trunc = TRUE)
writeVector(atr2, "data/spatial_data/map_shapefiles/asia_tropical_map.shp", overwrite = TRUE)

aus2 <- rasterize(aus, chelsa, touches = TRUE) %>%
  rotate(left = FALSE) %>% 
  as.polygons(trunc = TRUE)
writeVector(aus2, "data/spatial_data/map_shapefiles/australia_map.shp", overwrite = TRUE)

eur2 <- rasterize(eur, chelsa, touches = TRUE) %>%
  rotate(left = FALSE) %>% 
  as.polygons(trunc = TRUE)
writeVector(eur2, "data/spatial_data/map_shapefiles/europe_map.shp", overwrite = TRUE)

nam2 <- rasterize(nam, chelsa, touches = TRUE) %>%
  rotate(left = FALSE) %>% 
  as.polygons(trunc = TRUE)
writeVector(nam2, "data/spatial_data/map_shapefiles/northern_america_map.shp", overwrite = TRUE)

sam2 <- rasterize(sam, chelsa, touches = TRUE) %>%
  rotate(left = FALSE) %>% 
  as.polygons(trunc = TRUE)
writeVector(sam2, "data/spatial_data/map_shapefiles/southern_america_map.shp", overwrite = TRUE)

# add to world map





# load required data
pac <- vect("data/spatial_data/map_shapefiles/pacific_map.shp")
afr <- vect("data/spatial_data/map_shapefiles/africa_map.shp")
ate <- vect("data/spatial_data/map_shapefiles/asia_temperate_map.shp")
atr <- vect("data/spatial_data/map_shapefiles/asia_tropical_map.shp")
aus <- vect("data/spatial_data/map_shapefiles/australia_map.shp")
eur <- vect("data/spatial_data/map_shapefiles/europe_map.shp")
nam <- vect("data/spatial_data/map_shapefiles/northern_america_map.shp")
sam <- vect("data/spatial_data/map_shapefiles/southern_america_map.shp")

region_names <- c("Pacific Islands", "Africa", "temperate Asia", "tropical Asia",
                  "Australasia", "Northern America", "Southern America")
region_col <- c("pink", "plum4", "indianred", "goldenrod1", "darkseagreen3", 
                "cadetblue3", "darkslategray4", "tan3")
windows()
map("world2")
plot(pac, col = "pink", add = TRUE)
plot(afr, col = "plum4", add = TRUE)
plot(ate, col = "indianred", add = TRUE)
plot(atr, col = "goldenrod1", add = TRUE)
plot(aus, col = "darkseagreen3", add = TRUE)
plot(eur, col = "cadetblue3", add = TRUE)
plot(nam, col = "darkslategray4", add = TRUE)
plot(sam, col = "tan3", add = TRUE)



legend("center",
       title = "Regions",
       bg = "snow",
       as.character(region_names), fill = region_col)

mapcol <- 

maps::map("world2", ylim = c(-70,70), xlim = c(125,271), col = mapcol, bg = mapcol)




maps::map("world2",  bg = "snow", col = "white", fill = TRUE, myborder = 0)
addnortharrow(pos = "bottomleft", scale = 0.6)
box(col = "black", which = "plot")



# map.scale(x = 9, y = -78, ratio = FALSE, relwidth = 0.04, cex = 0.9)
plot(pac, col = "pink", add = TRUE)
plot(afr, col = "plum4", add = TRUE)
plot(ate, col = "indianred", add = TRUE)
plot(atr, col = "goldenrod1", add = TRUE)
plot(aus, col = "darkseagreen3", add = TRUE)
plot(eur, col = "cadetblue3", add = TRUE)
plot(nam, col = "darkslategray4", add = TRUE)
plot(sam, col = "tan3", add = TRUE)

maps::map("world2", ylim = c(-26,30), xlim = c(125,271),  bg = "snow", 
          col = "white", fill = TRUE, myborder = 0)
box(col = "black", which = "plot")
plot(pac, col = "pink", add = TRUE)# pac <- rast("data/spatial_data/pacific_islands_rast.tif")
plot(nam, col = "darkslategray4", add = TRUE)
plot(sam, col = "tan3", add = TRUE)
plot(aus, col = "darkseagreen3", add = TRUE)
plot(atr, col = "goldenrod1", add = TRUE)
legend("bottomright",
       title = "Regions",
       bg = "snow",
       as.character(region_names), fill = region_col)





# pacific <- pac %>%
#   rotate(left = FALSE) %>% 
#   as.polygons(trunc = TRUE)
# 
# plot(pacific)
# 
# writeVector(pacific, "data/spatial_data/pacific_map.shp", overwrite = TRUE)

prettymap(map("world2"))
terra::crs(pac)
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