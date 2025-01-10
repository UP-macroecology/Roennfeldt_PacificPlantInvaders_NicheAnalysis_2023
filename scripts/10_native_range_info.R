#' ---------------------------
#
# Purpose of script: identify in which regions the species have native occurrences
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
library(stringr)
library(terra)
library(tidyr)
library(maps)



# required paths ----------------------------------------------------------

path_chelsa <- ""


# native region IDs -------------------------------------------------------

## prep data ---------------------------------------------------------------

# species 
load("data/species_selection/spp_suitable_AC.RData")

# prep spatial data 
pac_islands <- vect("data/spatial_data/pacific_islands.shp") # island shape files
tdwg <- st_read("data/spatial_data/tdwg/geojson/level1.geojson")[-9,] # without antarctic

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
chelsa <- rast(paste0(path_chelsa,"/CHELSA_pr_01_1980_V.2.1.tif"))
# change values to 1 to decrease size
values(chelsa) <- 1 

crs_chelsa <- crs(chelsa)

rm(chelsa)


## identify native regions -------------------------------------------------

spp_nat_regions <- as.data.frame(matrix(nrow = 0, ncol = 3))
names(spp_nat_regions) <- c("species", "region", "nat_occs")

spp <- spp_suitable_AC

# threshold to only consider a region with at least X % of the species' occurrences
# as one of the native regions
occ_percentage <- 5


foreach(spp_index = 1:length(spp), .packages = c("dplyr", "terra")) %do% # can be easily modded to be done on the cluster
  try({
    
    print(spp_index)
    
    # load native occurrences
    load(paste0("data/occurrence_data/regional_occs/criterion_1/native/nat_occs",spp[spp_index],".RData")) # object name: nat
    
    # define the minimum number of occurrences needed to make up x % of the overall occurrences
    min_occ <- base::round(nrow(nat)/100*occ_percentage)
    # print(min_occ)
    
    # create SpatVector based on coordinates from the subset
    nat_coords <- terra::vect(data.frame(lon = nat$lon, lat = nat$lat))
    
    # add crs
    terra::crs(nat_coords) <- crs_chelsa
    
    # check how many introduced occurrences intersect with the different regional polygons
    

    ## Pacific Islands --

    over_pac <- terra::intersect(nat_coords, pac_islands)
    nr_pac <- as.numeric(length(over_pac))
    
    if (nr_pac >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "pac",
                                          nat_occs = nr_pac))
    } # end of if condition
    
    rm(over_pac, nr_pac)
   
     
    ## Africa --
    
    over_afr <- terra::intersect(nat_coords, africa)
    nr_afr <- as.numeric(length(over_afr))
    
    if (nr_afr >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "afr",
                                          nat_occs = nr_afr))
    } # end of if condition
    
    rm(over_afr, nr_afr)
    
    
    ## Europe --
    
    over_eur <- terra::intersect(nat_coords, europe)
    nr_eur <- as.numeric(length(over_eur))
    
    if (nr_eur >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "eur",
                                          nat_occs = nr_eur))
    } # end of if condition
    
    rm(over_eur, nr_eur)
    
    
    ## temp. Asia --
    
    
    over_ate <- terra::intersect(nat_coords, asia_temperate)
    nr_ate <- as.numeric(length(over_ate))
    
    if (nr_ate >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "ate",
                                          nat_occs = nr_ate))
    } # end of if condition
    
    rm(over_ate, nr_ate)
    
    
    ## trop. Asia --
    
    over_atr <- terra::intersect(nat_coords, asia_tropical)
    nr_atr <- as.numeric(length(over_atr))
    
    if (nr_atr >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "atr",
                                          nat_occs = nr_atr))
    } # end of if condition
    
    rm(over_atr, nr_atr)
    
    
    ## Australasia --
    
    over_aus <- terra::intersect(nat_coords, australasia)
    nr_aus <- as.numeric(length(over_aus))
    
    if (nr_aus >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "aus",
                                          nat_occs = nr_aus))
    } # end of if condition
    
    rm(over_aus, nr_aus)
    
    
    ## North America --
    
    over_nam <- terra::intersect(nat_coords, northern_america)
    nr_nam <- as.numeric(length(over_nam))
    
    if (nr_nam >= min_occ) {
      spp_nat_regions <- rbind(spp_nat_regions, 
                               data.frame(species = spp[spp_index],
                                          region = "nam",
                                          nat_occs = nr_nam))
    } # end of if condition
    
    rm(over_nam, nr_nam)
    
    
    ## South America --
    
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


save(spp_nat_regions, file = "data/native_regions/spp_nat_regions_5_perc.RData")


## number of regions species are native to ---------------------------------

 

regions_per_species <- data.frame(species = spp_suitable_AC, nr_regions = NA)

for (spec in spp_suitable_AC) {
  nr_reg <- nrow(spp_nat_regions[spp_nat_regions$species == spec,])
  if(nr_reg > 0) {
    regions_per_species[regions_per_species$species == spec, "nr_regions"] <- nr_reg
  }

}
  
table(regions_per_species$nr_regions)
  




## create ID tags -------------------------------------------------------------

# load("data/native_region_ID/spp_nat_regions_5_perc.RData")

native_tags <- as.data.frame(matrix(nrow = 0, ncol = 2))
names(native_tags) <- c("species", "tag")

for (spp in unique(spp_nat_regions$species)) {
  
  # combine all regions of current species into one str tag
  native_regions <- spp_nat_regions %>% 
    filter(species == spp) %>% 
    pull(region) %>% 
    str_flatten(collapse = "_") # collapse vector to create str tag
  
  # add spp and tag in df
  native_tags <- rbind(native_tags,
                       data.frame(species = spp,
                                  tag = native_regions))
  
} # end of for loop over species

save(native_tags, file = "data/native_regions/native_region_IDs.RData")

length(unique(native_tags$tag)) # there are 82 unique combinations of native regions

sort(table(native_tags$tag)) # most common tag: nam_sam, followed by sam

sort(table(spp_nat_regions$region)) # most common native region: sam



# Koeppen climate regions -------------------------------------------------


# written for HPC


# we identified the main climate regions in which the species have native occurrences 
# because there where too many unique tags to get a meaningful idea of the species native range patterns

library(dplyr)
library(kgc)
library(tidyr)


## identify all climate regions (HPC cluster) ------------------------------

path_data <- ""

load(paste0(path_data, "/species_selection/spp_suitable_AC.RData"))

no_cores <- 20
cl <- makeCluster(no_cores)
registerDoParallel(cl)


df_occ_zones <- foreach(spp_index = 1:length(spp_suitable_AC), .packages = c("dplyr", "kgc"), .combine = "rbind", .verbose = TRUE) %dopar% # can be easily modded to be done on the cluster
  try({
    print(spp_suitable_AC[spp_index])
    
    # load native occurrences
    load(paste0(path_data,"/occurrence_data/regionl_occs/criterion_1/native/nat_occs",spp_suitable_AC[spp_index],".RData")) # object name: nat
    
    spp_occs <- nat %>%
      select(occ_id, lon, lat) %>%
      mutate(rndCoord.lon = RoundCoordinates(lon)) %>%
      mutate(rndCoord.lat = RoundCoordinates(lat))
    
    spp_occ_zones <- data.frame(spp_occs, ClimateZ = LookupCZ(spp_occs)) %>%
      select(occ_id, lon, lat, ClimateZ) %>%
      # dplyr::rename(climate_zone = ClimateZ) %>%
      mutate(species = spp_suitable_AC[spp_index], .before = occ_id)
    
    names(spp_occ_zones)[names(spp_occ_zones) == 'ClimateZ'] <- 'climate_zone'
    
    # df_occ_zones <- rbind(df_occ_zones, spp_occ_zones)
    
    spp_occ_zones
    
  }) # end of foreach loop over species

save(df_occ_zones, file = paste0(path_data, "/native_regions/df_occ_climate_zones.RData"))
stopCluster(cl)

# cluster specific clean up:
# rm(list = ls())
# gc()



## identify main climate regions (local machine) ---------------------------

load("data/native_regions/df_occ_climate_zones.RData")
load("data/species_selection/spp_suitable_AC.RData")

df_zones <- df_occ_zones %>% 
  select(!.before) %>% 
  select(species, occ_id, lon, lat, climate_zone)

df_zones[df_zones$climate_zone == "Climate Zone info missing","climate_zone"] <- NA

a_index <- which(grepl(pattern = "^A", df_zones$climate_zone))
b_index <- which(grepl(pattern = "^B", df_zones$climate_zone))
c_index <- which(grepl(pattern = "^C", df_zones$climate_zone))
d_index <- which(grepl(pattern = "^D", df_zones$climate_zone))
e_index <- which(grepl(pattern = "^E", df_zones$climate_zone))

df_zones[a_index, "main_climate"] <- "A"
df_zones[b_index, "main_climate"] <- "B"
df_zones[c_index, "main_climate"] <- "C"
df_zones[d_index, "main_climate"] <- "D"
df_zones[e_index, "main_climate"] <- "E"

#define custom function to add columns to data frame if they do not exist
add_cols <- function(df, cols) {
  add <- cols[!cols %in% names(df)]
  if (length(add) !=0) df[add] <- NA
  return(df)
}

zones <- c("A", "B", "C", "D", "E")

spp_zones_all <- as.data.frame(matrix(ncol = 7, nrow = 0))
names(spp_zones_all) <- c("species", "A", "B", "C", "D", "E", "main_climate")

for (spp in spp_suitable_AC) {
  
  print(spp)
  
  spp_zones <- as.data.frame(table(subset(df_zones, species == spp)$main_climate)) %>% 
    pivot_wider(names_from = Var1, values_from = Freq) %>%
    mutate(species = spp) %>% 
    add_cols(c("A", "B", "C", "D", "E")) %>% 
    select(species, A, B, C, D, E) %>% 
    mutate(total_occs = sum(A,B,C,D,E, na.rm = TRUE)) %>% 
    mutate(A = round(100/total_occs*A)) %>% 
    mutate(B = round(100/total_occs*B)) %>% 
    mutate(C = round(100/total_occs*C)) %>% 
    mutate(D = round(100/total_occs*D)) %>% 
    mutate(E = round(100/total_occs*E)) %>% 
    replace(is.na(.), 0) 
  
  spp_zones_all <- rbind(spp_zones_all, spp_zones)
}

spp_zones_all$freq_climate <- NA

for (spp in spp_suitable_AC) { 
  
  print(spp)
  
  spp_label <- NULL
  
  for (zone in zones) {
    print(zone)
    if (spp_zones_all[spp_zones_all$species == spp, zone] >= 30) {
      spp_label <- c(spp_label,zone)
    } # end of if condition
  } # end of for loop over zones
  
  spp_label <- paste(spp_label, collapse = "")
  
  spp_zones_all[spp_zones_all$species == spp,]$freq_climate <- spp_label
} # end of for loop over species


table(spp_zones_all$freq_climate)

save(spp_zones_all, file = "data/native_regions/spp_zones_all.RData")



