#' ---------------------------
#
# Purpose of script: splitting occurrence data into native and regional non-native
# subsets based on selection criterion 2 (no preferred status source)
# Author: Anna Rönnfeldt
# Date Created: ~ 2023-08
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------

library(sf)
library(dplyr)
library(terra)
library(foreach)

# required paths ----------------------------------------------------------

# provide path to folder with chels data here
path_chelsa <- ""


# prep data ---------------------------------------------------------------


load("data/occurrence_data/status_occs/occ_status_resolved.RData")
load("data/occurrence_data/occ_cleaned_slim.RData")

# select occ_id, and lon/lat columns
occ_cleaned_slim <- occ_cleaned_slim[,c(1,3:4)]


# table(occ_status_resolved$criterion_1)
# table(occ_status_resolved$criterion_2)

# slightly modify occ_status_resolved
occ_status_resolved <- occ_status_resolved %>%
  select(-final_status) %>%
  mutate(criterion_1 = replace(criterion_1, criterion_1 == "naturalized", "introduced")) %>%
  mutate(criterion_2 = replace(criterion_2, criterion_2 == "naturalized", "introduced")) %>%
  mutate(criterion_1 = replace(criterion_1, criterion_1 == "non-native", "introduced")) %>%
  mutate(criterion_2 = replace(criterion_2, criterion_2 == "non-native", "introduced")) %>%
  left_join(occ_cleaned_slim, by = "occ_id", keep = TRUE) # add lon/lat info 

occ_status_resolved <- occ_status_resolved %>%
  select(-occ_id.y) %>%
  rename("occ_id" = "occ_id.x") %>%
  arrange(occ_id)


rm(occ_cleaned_slim)

occ_crit_2 <- subset(occ_status_resolved, criterion_2 == "native" | criterion_2 == "introduced")
spp_2 <- unique(occ_crit_2$species.x) # 3645 unique species left (initially 3668)

save(occ_crit_2, file = "data/occurrence_data/regional_occs/occ_subset_crit_2.RData")
save(spp_2, file = "data/occurrence_data/regional_occs/spp_crit_2.RData")

# prep spatial data -------------------------------------------------------

pac_islands <- vect("data/spatial_data/pacific_islands.shp") # island shape files

tdwg <- st_read("data/spatial_data/tdwg/geojson/level1.geojson")[-9,] # without antarctic

# prepare individual shapefiles for the 8 different mainland regions
# unique(tdwg$LEVEL1_NAM)

#split the multipolygon up 
tdwg_poly <- st_cast(tdwg, "POLYGON")

# extract polygons per region and combine them in a SpatVector from the terra package
# make sure that overlaps with pac_islands are removed 
# (overkill, as I am doing it will all regions)

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

# prep species list -------------------------------------------------------

# load species list and occ subset
load("data/occurrence_data/regional_occs/occ_subset_crit_2.RData")
load("data/occurrence_data/regional_occs/spp_crit_2.RData")

occ_crit_2 <- occ_crit_2 %>%
  rename("species" = "species.x")

# create empty df to add info to
occ_count_crit_2 <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(occ_count_crit_2) <- c("species", "native_occs", "pacific_occs", "africa_occs", "europe_occs", "asia_temperate_occs", "asia_tropical_occs", "australasia_occs", "south_america_occs", "north_america_occs")


# # when running the code in chunks:
# load("data/regional_occs/criterion_2/occ_count_crit_2.RData")
# spp_initial <- spp_2
# spp_done <- unique(occ_count_crit_2$species)
# spp_left <- setdiff(spp_initial, spp_done)
# spp_2 <- spp_left[1:648]

# split into regional occs ------------------------------------------------

foreach(spp_index = 1:length(spp_2), .packages = c("dplyr", "terra")) %do% # can be easily modded to be done on the cluster
  try({
    
    print(spp_index)
    # reset nr variables for intr
    nr_pac <- NA
    nr_afr <- NA
    nr_eur <- NA
    nr_ate <- NA
    nr_atr <- NA
    nr_aus <- NA
    nr_sam <- NA
    nr_nam <- NA
    
    # subset for nat
    nat <- subset(occ_crit_2, criterion_2 == "native" & species == spp_2[spp_index])
    
    # get number of occurrences with status "native"
    nr_nat <- nrow(nat)
    
    # if number native occs >= 20: proceed with splitting and counting occurrences for each region
    if (length(nat$status) >= 20) {
      
      # save df with native occurrences for further processing
      
      save(nat, file = paste0("data/occurrence_data/regional_occs/criterion_2/native/nat_occs",spp_2[spp_index],".RData"))
      
      # subset for introduced
      intr <- subset(occ_crit_2, criterion_2 == "introduced" & species == spp_2[spp_index])
      
      # create SpatVector based on coordinates from the subset
      intr_coords <- terra::vect(data.frame(lon = intr$lon, lat = intr$lat))
      
      # add crs
      terra::crs(intr_coords) <- crs_chelsa
      
      # check how many introduced occurrences intersect with the different regional polygons
      
      #'# pacific islands ----
      
      # occ overlap
      over_pac <- terra::intersect(intr_coords, pac_islands)
      nr_pac <- length(over_pac)
      
      # if number of occs >= 20
      if (length(over_pac) >= 20) {
        
        # get coords and use them to select occs from the intr subset
        crds_pac <- as.data.frame(crds(over_pac))
        colnames(crds_pac) <- c("lon", "lat")
        intr_df_pac <- semi_join(intr, crds_pac, by = c("lon", "lat"))
        
        # save for later processing
        save(intr_df_pac, file = paste0("data/occurrence_data/regional_occs/criterion_2/introduced/intr_occs_pac_",spp_2[spp_index],".RData"))
        
      } # end of if over_pac
      
      #'# africa -----------
      
      # occ overlap
      over_africa <- terra::intersect(intr_coords, africa)
      nr_afr <- length(over_africa)
      
      # if number of occs >= 20
      if (length(over_africa) >= 20) {
        
        # get coords and use them to select occs from the intr subset
        crds_afr <- as.data.frame(crds(over_africa))
        colnames(crds_afr) <- c("lon", "lat")
        intr_df_afr <- semi_join(intr, crds_afr, by = c("lon", "lat"))
        
        # save for later processing
        save(intr_df_afr, file = paste0("data/occurrence_data/regional_occs/criterion_2/introduced/intr_occs_afr_",spp_2[spp_index],".RData"))
        
      } # end of if over_africa
      
      #'# europe ----------
      
      # occ overlap
      over_europe <- terra::intersect(intr_coords, europe)
      nr_eur <- length(over_europe)
      
      # if number of occs >= 20
      if (length(over_europe) >= 20) {
        
        # get coords and use them to select occs from the intr subset
        crds_eur <- as.data.frame(crds(over_europe))
        colnames(crds_eur) <- c("lon", "lat")
        intr_df_eur <- semi_join(intr, crds_eur, by = c("lon", "lat"))
        
        # save for later processing
        save(intr_df_eur, file = paste0("data/occurrence_data/regional_occs/criterion_2/introduced/intr_occs_eur_",spp_2[spp_index],".RData"))
        
      } # end of if over_europe
      
      #'# asia  temperate ------------
      
      # occ overlap
      over_ate <- terra::intersect(intr_coords, asia_temperate)
      nr_ate <- length(over_ate)
      
      # if number of occs >= 20
      if (length(over_ate) >= 20) {
        
        # get coords and use them to select occs from the intr subset
        crds_ate <- as.data.frame(crds(over_ate))
        colnames(crds_ate) <- c("lon", "lat")
        intr_df_ate <- semi_join(intr, crds_ate, by = c("lon", "lat"))
        
        # save for later processing
        save(intr_df_ate, file = paste0("data/occurrence_data/regional_occs/criterion_2/introduced/intr_occs_ate_",spp_2[spp_index],".RData"))
        
      } # end of if over_ate
      
      #'# asia  tropical ------------
      
      # occ overlap
      over_atr <- terra::intersect(intr_coords, asia_tropical)
      nr_atr <- length(over_atr)
      
      # if number of occs >= 20
      if (length(over_atr) >= 20) {
        
        # get coords and use them to select occs from the intr subset
        crds_atr <- as.data.frame(crds(over_atr))
        colnames(crds_atr) <- c("lon", "lat")
        intr_df_atr <- semi_join(intr, crds_atr, by = c("lon", "lat"))
        
        # save for later processing
        save(intr_df_atr, file = paste0("data/occurrence_data/regional_occs/criterion_2/introduced/intr_occs_atr_",spp_2[spp_index],".RData"))
        
      } # end of if over_atr
      
      #'# australasia ---------
      
      # occ overlap
      over_aus <- terra::intersect(intr_coords, australasia)
      nr_aus <- length(over_aus)
      
      # if number of occs >= 20
      if (length(over_aus) >= 20) {
        
        # get coords and use them to select occs from the intr subset
        crds_aus <- as.data.frame(crds(over_aus))
        colnames(crds_aus) <- c("lon", "lat")
        intr_df_aus <- semi_join(intr, crds_aus, by = c("lon", "lat"))
        
        # save for later processing
        save(intr_df_aus, file = paste0("data/occurrence_data/regional_occs/criterion_2/introduced/intr_occs_aus_",spp_2[spp_index],".RData"))
        
      } # end of if over_aus
      
      #'# north america ---------------------
      
      # occ overlap
      over_north_america <- terra::intersect(intr_coords, northern_america)
      nr_nam <- length(over_north_america)
      
      # if number of occs >= 20
      if (length(over_north_america) >= 20) {
        
        # get coords and use them to select occs from the intr subset
        crds_nam <- as.data.frame(crds(over_north_america))
        colnames(crds_nam) <- c("lon", "lat")
        intr_df_nam <- semi_join(intr, crds_nam, by = c("lon", "lat"))
        
        # save for later processing
        save(intr_df_nam, file = paste0("data/occurrence_data/regional_occs/criterion_2/introduced/intr_occs_nam_",spp_2[spp_index],".RData"))
        
      } # end of if over_nam
      
      #'# south america -------
      
      # occ overlap
      over_south_america <- terra::intersect(intr_coords, southern_america)
      nr_sam <- length(over_south_america)
      
      # if number of occs >= 20
      if (length(over_south_america) >= 20) {
        
        # get coords and use them to select occs from the intr subset
        crds_sam <- as.data.frame(crds(over_south_america))
        colnames(crds_sam) <- c("lon", "lat")
        intr_df_sam <- semi_join(intr, crds_sam, by = c("lon", "lat"))
        
        # save for later processing
        save(intr_df_sam , file = paste0("data/occurrence_data/regional_occs/criterion_2/introduced/intr_occs_sam_",spp_2[spp_index],".RData"))
        
      } # end of if over_sam
      
    } else {
      print("Not enough native occurrences")
    }
    
    # add to df
    (occ_count_crit_2 <- rbind(occ_count_crit_2,
                               data.frame(species = spp_2[spp_index],
                                          native_occs  = nr_nat,
                                          pacific_occs = nr_pac,
                                          africa_occs  = nr_afr,
                                          europe_occs  = nr_eur,
                                          asia_temperate_occs = nr_ate,
                                          asia_tropical_occs  = nr_atr,
                                          australasia_occs    = nr_aus,
                                          south_america_occs  = nr_sam,
                                          north_america_occs  = nr_nam)))
    
  }) # end of try criterion 1


save(occ_count_crit_2, file = "data/occurrence_data/regional_occs/criterion_2/occ_count_crit_2.RData")
