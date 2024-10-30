#' ---------------------------
#
# Purpose of script: identify in which regions the species have naitve occurrences
# Author: Anna Rönnfeldt
# Date Created: 2024-10-28
# Email: roennfeldt@uni-potsdam.de
#
# Notes: Based on criterion 1 of the status assignment
#
#' ---------------------------

library(dplyr)
library(foreach)
library(sf)
library(terra)

library(maps)

path_occ_data <- "X:/roennfeldt/Projects/PhD_C1/data/regional_occs/criterion_1/native/"
# load data ---------------------------------------------------------------

# # occurrence data with status info from criterion 1 available
# load("data/regional_occs/occ_subset_crit_1.RData") 

# species 
load("data/spp_suitable_AC.RData")

# prep spatial data 
pac_islands <- vect("data/spatial_data/pacific_islands.shp") # island shape files

tdwg <- st_read("data/tdwg/geojson/level1.geojson")[-9,] # without antarctic

# prepare individual shapefiles for the 8 different mainland regions
# unique(tdwg$LEVEL1_NAM)

#split the multipolygon up 
tdwg_poly <- st_cast(tdwg, "POLYGON")

# extract polygons per region and combine them in a SpatVector from the terra package
# make sure that overlaps with pac_islands are removed 
# (overkill, as I am doing it will all regions)

# pac_islands <- vect(subset(tdwg_poly, LEVEL1_NAM == "PACIFIC")) 

europe <- vect(subset(tdwg_poly, LEVEL1_NAM == "EUROPE")) %>%
  erase(pac_islands)

africa <- vect(subset(tdwg_poly, LEVEL1_NAM == "AFRICA")) %>%
  erase(pac_islands)

asia_temperate <- vect(subset(tdwg_poly, LEVEL1_NAM == "ASIA-TEMPERATE")) %>%
  erase(pac_islands)

asia_tropical <- vect(subset(tdwg_poly, LEVEL1_NAM == "ASIA-TROPICAL")) %>%
  erase(pac_islands)
australasia <- vect(subset(tdwg_poly, LEVEL1_NAM == "AUSTRALASIA")) %>%
  erase(pac_islands)

northern_america <- vect(subset(tdwg_poly, LEVEL1_NAM == "NORTHERN AMERICA")) %>%
  erase(pac_islands)

southern_america <- vect(subset(tdwg_poly, LEVEL1_NAM == "SOUTHERN AMERICA")) %>%
  erase(pac_islands)

rm(tdwg_poly, tdwg)

# prepare reference crs
# load chelsa tif as reference raster with a 1km resolution
chelsa <- rast("Z:/Arbeit/datashare/data/envidat/biophysical/CHELSA_V2/global/CHELSA_pr_01_1980_V.2.1.tif")
# change values to 1 to decrease size
values(chelsa) <- 1 

crs_chelsa <- crs(chelsa)

rm(chelsa)


# prep storage object -----------------------------------------------------

spp_nat_regions <- as.data.frame(matrix(nrow = 0, ncol = 3))
names(spp_nat_regions) <- c("species", "region", "nat_occs")

# identify native regions -------------------------------------------------

# for spp

spp <- spp_suitable_AC

occ_percentage <- 5


foreach(spp_index = 1:length(spp), .packages = c("dplyr", "terra")) %do% # can be easily modded to be done on the cluster
  try({
    
    print(spp_index)
    
    # load native occurrences
    load(paste0(path_occ_data,"nat_occs",spp[spp_index],".RData")) # object name: nat
    
    # define the minimum number of occurrences needed to make up x % of the overall occurrences
    min_occ <- base::round(nrow(nat)/100*occ_percentage)
    # print(min_occ)
    
    # create SpatVector based on coordinates from the subset
    nat_coords <- terra::vect(data.frame(lon = nat$lon, lat = nat$lat))
    
    # add crs
    terra::crs(nat_coords) <- crs_chelsa
    
    # check how many introduced occurrences intersect with the different regional polygons
    

    ## Pacific Islands --------------------------------------------------------

    over_pac <- terra::intersect(nat_coords, pac_islands)
    nr_pac <- as.numeric(length(over_pac))
    
    if (nr_pac >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "pac",
                                          nat_occs = nr_pac))
    } # end of if condition
    
    rm(over_pac, nr_pac)
    
    ## Africa -----------------------------------------------------------------
    
    over_afr <- terra::intersect(nat_coords, africa)
    nr_afr <- as.numeric(length(over_afr))
    
    if (nr_afr >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "afr",
                                          nat_occs = nr_afr))
    } # end of if condition
    
    rm(over_afr, nr_afr)
    
    ## Europe -----------------------------------------------------------------
    
    over_eur <- terra::intersect(nat_coords, europe)
    nr_eur <- as.numeric(length(over_eur))
    
    if (nr_eur >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "eur",
                                          nat_occs = nr_eur))
    } # end of if condition
    
    rm(over_eur, nr_eur)
    
    ## temp. Asia -------------------------------------------------------------
    
    over_ate <- terra::intersect(nat_coords, asia_temperate)
    nr_ate <- as.numeric(length(over_ate))
    
    if (nr_ate >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "ate",
                                          nat_occs = nr_ate))
    } # end of if condition
    
    rm(over_ate, nr_ate)
    
    ## trop. Asia -------------------------------------------------------------
    
    over_atr <- terra::intersect(nat_coords, asia_tropical)
    nr_atr <- as.numeric(length(over_atr))
    
    if (nr_atr >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "atr",
                                          nat_occs = nr_atr))
    } # end of if condition
    
    rm(over_atr, nr_atr)
    
    ## Australasia -------------------------------------------------------------
    
    over_aus <- terra::intersect(nat_coords, australasia)
    nr_aus <- as.numeric(length(over_aus))
    
    if (nr_aus >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "aus",
                                          nat_occs = nr_aus))
    } # end of if condition
    
    rm(over_aus, nr_aus)
    
    ## Northern America -------------------------------------------------------------
    
    over_nam <- terra::intersect(nat_coords, northern_america)
    nr_nam <- as.numeric(length(over_nam))
    
    if (nr_nam >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "nam",
                                          nat_occs = nr_nam))
    } # end of if condition
    
    rm(over_nam, nr_nam)
    
    ## Southern America -------------------------------------------------------------
    
    over_sam <- terra::intersect(nat_coords, southern_america)
    nr_sam <- as.numeric(length(over_sam))
    
    if (nr_sam >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "sam",
                                          nat_occs = nr_sam))
    } # end of if condition
    
    rm(over_sam, nr_sam)
  }) # end of try



# compile results ---------------------------------------------------------

regions_per_species <- data.frame(species = spp_suitable_AC, nr_regions = NA)

for (spec in spp_suitable_AC) {
  nr_reg <- nrow(spp_nat_regions[spp_nat_regions$species == spec,])
  if(nr_reg > 0) {
    regions_per_species[regions_per_species$species == spec, "nr_regions"] <- nr_reg
  }

}
  
table(regions_per_species$nr_regions)
  

save(spp_nat_regions, file = "results/spp_nat_regions_5_perc.RData")


maps::map("world")
points(nat_coords, col = "green")
points(over_afr, col = "blue")



spp_index <- 294







