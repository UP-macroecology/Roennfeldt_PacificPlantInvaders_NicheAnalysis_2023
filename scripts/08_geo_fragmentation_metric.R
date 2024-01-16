library(sf)
library(terra)

rm(list = ls())

tdwg <- st_read("data/tdwg/geojson/level1.geojson")[-c(6,9),] # without antarctic

tdwg_poly <- st_cast(tdwg, "POLYGON")
regions_vect <- vect(tdwg_poly) 

area_all <- expanse(regions_vect, unit="m", transform=TRUE)
perim_all <- perim(regions_vect) # in m

all_regions <- subset(st_cast(tdwg, "POLYGON"))[,2:3] %>%
  mutate(area = area_all,
         perimeter =perim_all)

pac_islands <- vect("data/spatial_data/pacific_islands.shp") # island shape files

area_pac <- sum(expanse(pac_islands, unit="m", transform=TRUE))
perimeter_pac <- sum(perim(pac_islands)) # in m


regions <- unique(tdwg$LEVEL1_NAM)

fragmen_df <- data.frame("region" = NULL,
                         "area" = NULL,
                         "perimeter" = NULL,
                         "fragmen_1" = NULL,
                         "fragmen_2" = NULL)

for (region in regions) {
  
  region_df <- subset(all_regions, LEVEL1_NAM == region)
  
  area_total <- sum(region_df$area) # in m
  perimeter_total <- sum(region_df$perimeter)
  
  fragmen_df <- rbind(fragmen_df,
                      data.frame("region" = region,
                                 "area" = area_total,
                                 "perimeter" = perimeter_total,
                                 "fragmen_1" = area_total/perimeter_total,
                                 "fragmen_2" = perimeter_total/area_total))
} # for loop over regions

# add pacific info

fragmen_df <- rbind(fragmen_df,
                    data.frame("region" = "PACIFIC",
                               "area" = area_pac,
                               "perimeter" = perimeter_pac,
                               "fragmen_1" = area_pac/perimeter_pac,
                               "fragmen_2" = perimeter_pac/area_pac))

save(fragmen_df, file = "data/trait_analysis/fragmentation_metric.RData")

