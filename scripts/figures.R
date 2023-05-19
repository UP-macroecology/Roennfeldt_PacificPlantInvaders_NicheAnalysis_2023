library(sf)
library(ggplot2)

# paths work --------------------------------------------------------------

path_user <- "M:/C1"

# Non-native regions ------------------------------------------------------

# load tdwg regions
tdwg1 <- st_read(file.path(path_user,"data/TDWG/wgsrpd-master/geojson/level1.geojson"))

# drop antarctica 
regions <- tdwg1[-9,]

# plot 
ggplot(regions) +
  geom_sf(aes(fill =  LEVEL1_NAM))


scale_x_continuous(breaks=c(100, 150, 200, 250, 300),
                   labels=c("100°E", "150°E", "160°W", "110°W", "60°W"))+
  scale_y_continuous(breaks=c(-40, -20, 0, 20),
                     labels=c("40°S", "20°S", "0°N/S", "20°N"))  