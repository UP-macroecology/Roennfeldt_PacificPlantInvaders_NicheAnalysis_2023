library(sf)
library(terra)
library(landscapemetrics)

rm(list = ls())



# new test session --------------------------------------------------------

# get a raster as reference for the spatial resolution we're working with
chelsa <- rast("Z:/Arbeit/datashare/data/envidat/biophysical/CHELSA_V2/global/CHELSA_pr_01_1980_V.2.1.tif")
# change values to 1 to decrease size
values(chelsa) <- 1 


# load in tdwg regions wwe're working with 
tdwg <- st_read("data/tdwg/geojson/level1.geojson")[-c(6,9),] # without antarctic

eur_vec <- vect(tdwg[1,3])

eur_rast <- rasterize(eur_vec, chelsa, touches = TRUE)
# cut raster to extent of the vector
eur_rast_2 <- crop(eur_rast, ext(eur_vec))

terra::plot(eur_rast, col = "lightgreen")

terra::plot(vect(tdwg[1,3]), add = TRUE)

terra::lines(ext(eur_vec))
terra::lines(ext(eur_rast_2), col = "red")



# metric: CAI_CV, which is a core are metric that is scaled to the mean and thus easily comparable

lsm_c_cai_cv(eur_rast_2)$value
lsm_c_clumpy(eur_rast_2)


# build a loop:

# prepare vector
# prepare raster and crop it to the extent of the vector

# get landscape metrics for the raster
# store results in df


# example from landscapemetrics package:
landscape <- terra::rast(landscapemetrics::landscape)
lsm_c_clumpy(landscape)


lsm_c_cai_cv(landscape)



terra::plot(landscape)



# old version -------------------------------------------------------------




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

