library(doParallel)
library(dplyr)
library(geojsonsf) # to work with the tdwg regions
library(ggplot2)
library(sf) # for st_read
library(terra)


# preamble
rm(list = ls())

# ## Packages ---------------------------------------------------------------
# install.load.package <- function(x) {
#   if (!require(x, character.only = TRUE))
#     install.packages(x, repos='http://cran.us.r-project.org')
#   require(x, character.only = TRUE)
# }
# package_vec <- c(
#   "dplyr", "sf", "doParallel", "foreach", "geojsonsf", "terra" # names of the packages required placed here as character objects
# )
# 
# sapply(package_vec, install.load.package)


# source("scripts/functions.R")

# required paths

# when working from home
# path_home <- "Z:/roennfeldt/C1/data" 
# path_ds <- "Y:/AG26/Arbeit/datashare/data/biodat/distribution/Pacific_invaders"

# work laptop
path_home <- "M:/C1/data"
path_ds <- "Z:/Arbeit/datashare/data/biodat/distribution/Pacific_invaders"

# cluster paths
path_imp <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1/")
path_mnt <- file.path("/mnt/ibb_share/zurell/biodat/distribution/Pacific_invaders")


# required data ----------------------------------------------------------------
load("results/status_info/all_specs_occ_final.RData") # eventually
load("results/intermediate/species_names.RData")
# load("results/intermediate/df_testing.RData")


# cluster:
# load(file.path(path_imp,"input_data/species_names.RData"))
# load(file.path(path_imp,"output/all_specs_occ_final.RData"))

# tdwg level 1 gives 9 continental boundaries
# tdwg1 <- st_read(file.path(path_imp,"input_data/level1.geojson"))
tdwg1 <- st_read(file.path(path_home, "TDWG/wgsrpd-master/geojson/level1.geojson"))
tdwg_info <- st_drop_geometry(tdwg1)

# load shapefile for pacific islands (spatialpolygonsdataframe)
# load(file.path(path_mnt, "geoentitites_210902/geoentities_plus_newname.RData"))
load(file.path(path_ds,"geoentitites_210902/geoentities_plus_newname.RData"))
pacific_islands <- vect(st_as_sf(geoentities))
crs(pacific_islands) <- crs(tdwg1)

# subset master object ---------------------------------------------------------

# create subset of main object to increase speed during testing phase

# df <- all_specs_occ_final[all_specs_occ_final$species %in% sample(specs_all, 4),]
# save(df, file = "results/intermediate/df_testing.RData")

# free memory
# rm(all_specs_occ_final)





# dummy workflow ----------------------------------------------------------------

# # create empty df to add info to 
occ_count <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(occ_count) <- c("species", "native_occs", "pac_occs", "eur_occs", "afr_occs", "ate_occs", "atr_occs", "aus_occs", "nam_occs", "sam_occs")


#spp <- unique(df$species) # for testing

# 
# registerDoParallel(cores = 10) # doParallel package
# getDoParWorkers()


# cluster foreach ----
# foreach(spp_index = 1:length(spp)) %do%
#   try({
#     
#     # reset all counters to NA
#     nr_nat <- NA
#     nr_pac <- NA
#     nr_eur <- NA
#     nr_afr <- NA
#     nr_ate <- NA
#     nr_atr <- NA
#     nr_aus <- NA
#     nr_nam <- NA
#     nr_sam <- NA
#     
#     # subset occ for current species
#     spec_df <- subset(all_specs_occ_final, species == spp[spp_index])
#     
#     # get native occurrences for which the continental code is not NA
#     nat <- subset(spec_df, final_status == "native1" & tdwg_l1_code != is.na(NA))
#     nr_nat <- nrow(nat)
#     save(nat, file = paste0(path_imp,"output/regional_occs/nat1_",spp[spp_index],".RData"))
#     
#     # get all introduced occurrences for which the continental code is not NA
#     intr <- subset(spec_df, final_status == "introduced1" & species == spp[spp_index] & tdwg_l1_code != is.na(NA))
#     intr_coords <- terra::vect(data.frame(lon = intr$lon, lat = intr$lat))
#     # add crs
#     crs(intr_coords) <- crs(tdwg1) 
#   
#     
#     # count occurrences over pacific islands and save results
#     over_island <- terra::intersect(intr_coords, pacific_islands)
#     nr_pac <- length(over_island)
#     
#     
#     regs <- unique(intr$tdwg_l1_code)
#     
#     for(reg in regs){
#       
#       region_name <- tdwg_info[tdwg_info$LEVEL1_COD == reg,2]
#       
#       # check which region it is and save nr of occs nd the occurrences itself 
#       if(region_name == "EUROPE"){
#         eur_df <- subset(intr, tdwg_l1_code == reg)
#         nr_eur <- nrow(eur_df)
#         save(eur_df, file = paste0(path_imp,"output/regional_occs/intr1_eur_",spp[spp_index],".RData"))} # end Europe
#       
#       if(region_name == "AFRICA"){
#         afr_df <- subset(intr, tdwg_l1_code == reg)
#         nr_afr <- nrow(afr_df)
#         save(afr_df, file = paste0(path_imp,"output/regional_occs/intr1_afr_",spp[spp_index],".RData"))} # end Africa
#       
#       if(region_name == "ASIA-TEMPERATE"){
#         ate_df <- subset(intr, tdwg_l1_code == reg)
#         nr_ate <- nrow(ate_df)
#         save(ate_df, file = paste0(path_imp,"output/regional_occs/intr1_ate_",spp[spp_index],".RData"))} # end Asia (temperate)
#       
#       if(region_name == "ASIA-TROPICAL"){
#         atr_df <- subset(intr, tdwg_l1_code == reg)
#         nr_atr <- nrow(atr_df)
#         save(atr_df, file = paste0(path_imp,"output/regional_occs/intr1_atr_",spp[spp_index],".RData"))} # end Asia (temperate)
#       
#       if(region_name == "AUSTRALASIA"){
#         aus_df <- subset(intr, tdwg_l1_code == reg)
#         nr_aus <- nrow(aus_df)
#         save(aus_df, file = paste0(path_imp,"output/regional_occs/intr1_aus_",spp[spp_index],".RData"))} # end Australasia
#       
#       if(region_name == "NORTHERN AMERICA"){
#         nam_df <- subset(intr, tdwg_l1_code == reg)
#         nr_nam <- nrow(nam_df)
#         save(nam_df, file = paste0(path_imp,"output/regional_occs/intr1_nam_",spp[spp_index],".RData"))} # end Northern America
#       
#       if(region_name == "SOUTHERN AMERICA"){
#         sam_df <- subset(intr, tdwg_l1_code == reg)
#         nr_sam <- nrow(sam_df)
#         save(sam_df, file = paste0(path_imp,"output/regional_occs/intr1_sam_",spp[spp_index],".RData"))} # end Asia (temperate)
#     
#       } # end of for loop over regs
#     
#     # combine info in df
#     occ_count <- rbind(occ_count,
#                        data.frame(species = spp[spp_index],
#                                   native_occs  = nr_nat,
#                                   pac_occs = nr_pac,
#                                   eur_occs = nr_eur,
#                                   afr_occs = nr_afr,
#                                   ate_occs = nr_ate,
#                                   atr_occs = nr_atr,
#                                   aus_occs = nr_aus,
#                                   nam_occs = nr_nam,
#                                   sam_occs = nr_sam))
#     
#   }) # end of foreach-try


save(occ_count, file = file.path(path_imp, "output/occ_count1.RData"))



# check which species have enough occurrences over the islands (suitable for general analysis)
spp_enough <- subset(occ_count, native_occs >= 20 & pac_occs >= 20) # 31

# check how many species can be for the analysis for each mainland region
eur_enough <- subset(spp_enough, eur_occs >= 20)        # 7
afr_enough <- subset(spp_enough, afr_occs >= 20)        # 286
ate_enough <- subset(spp_enough, ate_occs >= 20)          # 223
atr_enough <- subset(spp_enough, atr_occs >= 20)       # 126
aus_enough <- subset(spp_enough, aus_occs >= 20)     # 313
nam_enough <- subset(spp_enough, nam_occs >= 20) #268
sam_enough <- subset(spp_enough, sam_occs >= 20) # 290

# create list of species with enough occurrences depending on the region
occ_enough <- list(nr_occs = spp_enough, 
                   spp_names = list(general = spp_enough$species, 
                                    europe = eur_enough$species,
                                    africa = afr_enough$species,
                                    a_temperate = ate_enough$species,
                                    a_tropical = atr_enough$species,
                                    australasia = aus_enough$species,
                                    north_america = nam_enough$species,
                                    south_america = sam_enough$species))

# save results
save(occ_enough, file = file.path(path_imp, "output/enough_occs1.RData"))



# laptop version ----
spp <- specs_all[2001:2788]

foreach(spp_index = 1:length(spp)) %do%
  try({

    # reset all counters to NA
    nr_nat <- NA
    nr_pac <- NA
    nr_eur <- NA
    nr_afr <- NA
    nr_ate <- NA
    nr_atr <- NA
    nr_aus <- NA
    nr_nam <- NA
    nr_sam <- NA

    # subset occ for current species
    spec_df <- subset(all_specs_occ_final, species == spp[spp_index])

    # get native occurrences for which the continental code is not NA
    nat <- subset(spec_df, final_status == "native1" & tdwg_l1_code != is.na(NA))
    nr_nat <- nrow(nat)
    save(nat, file = paste0("data/regional_occs/nat1_",spp[spp_index],".RData"))

    # get all introduced occurrences for which the continental code is not NA
    intr <- subset(spec_df, final_status == "introduced1" & species == spp[spp_index] & tdwg_l1_code != is.na(NA))
    intr_coords <- terra::vect(data.frame(lon = intr$lon, lat = intr$lat))
    # add crs
    crs(intr_coords) <- crs(tdwg1)
    
    # get coords and use them to select occs from the intr subset
    crds_pac <- as.data.frame(crds(over_island))
    colnames(crds_pac) <- c("lon", "lat")
    pac_df <- semi_join(intr, crds_pac, by = c("lon", "lat")) 


    # count occurrences over pacific islands and save results
    over_island <- terra::intersect(intr_coords, pacific_islands)
    nr_pac <- length(over_island)


    regs <- unique(intr$tdwg_l1_code)

    for(reg in regs){

      region_name <- tdwg_info[tdwg_info$LEVEL1_COD == reg,2]

      # check which region it is and save nr of occs nd the occurrences itself
      if(region_name == "EUROPE"){
        eur_df <- subset(intr, tdwg_l1_code == reg)
        nr_eur <- nrow(eur_df)
        save(eur_df, file = paste0("data/regional_occs/intr1_eur_",spp[spp_index],".RData"))} # end Europe
      

      
      if(region_name == "AFRICA"){
        afr_df <- subset(intr, tdwg_l1_code == reg)
        nr_afr <- nrow(afr_df)
        save(afr_df, file = paste0("data/regional_occs/intr1_afr_",spp[spp_index],".RData"))} # end Africa

      if(region_name == "ASIA-TEMPERATE"){
        ate_df <- subset(intr, tdwg_l1_code == reg)
        nr_ate <- nrow(ate_df)
        save(ate_df, file = paste0("data/regional_occs/intr1_ate_",spp[spp_index],".RData"))} # end Asia (temperate)

      if(region_name == "ASIA-TROPICAL"){
        atr_df <- subset(intr, tdwg_l1_code == reg)
        nr_atr <- nrow(atr_df)
        save(atr_df, file = paste0("data/regional_occs/intr1_atr_",spp[spp_index],".RData"))} # end Asia (temperate)

      if(region_name == "AUSTRALASIA"){
        aus_df <- subset(intr, tdwg_l1_code == reg)
        nr_aus <- nrow(aus_df)
        save(aus_df, file = paste0("data/regional_occs/intr1_aus_",spp[spp_index],".RData"))} # end Australasia

      if(region_name == "NORTHERN AMERICA"){
        nam_df <- subset(intr, tdwg_l1_code == reg)
        nr_nam <- nrow(nam_df)
        save(nam_df, file = paste0("data/regional_occs/intr1_nam_",spp[spp_index],".RData"))} # end Northern America

      if(region_name == "SOUTHERN AMERICA"){
        sam_df <- subset(intr, tdwg_l1_code == reg)
        nr_sam <- nrow(sam_df)
        save(sam_df, file = paste0("data/regional_occs/intr1_sam_",spp[spp_index],".RData"))} # end Asia (temperate)

      } # end of for loop over regs

    # combine info in df
    occ_count <- rbind(occ_count,
                       data.frame(species = spp[spp_index],
                                  native_occs  = nr_nat,
                                  pac_occs = nr_pac,
                                  eur_occs = nr_eur,
                                  afr_occs = nr_afr,
                                  ate_occs = nr_ate,
                                  atr_occs = nr_atr,
                                  aus_occs = nr_aus,
                                  nam_occs = nr_nam,
                                  sam_occs = nr_sam))

  }) # end of foreach-try

save(occ_count, file = "results/intermediate/occ_count_intermediate.RData")




# criterion 2 ------------------------------------------------------------------


occ_count2 <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(occ_count2) <- c("species", "native_occs", "pac_occs", "eur_occs", "afr_occs", "ate_occs", "atr_occs", "aus_occs", "nam_occs", "sam_occs")

spp <- specs_all[1501:2788]

foreach(spp_index = 1:length(spp)) %do%
  try({
    
    print(spp_index)
    
    # reset all counters to NA
    nr_nat <- NA
    nr_pac <- NA
    nr_eur <- NA
    nr_afr <- NA
    nr_ate <- NA
    nr_atr <- NA
    nr_aus <- NA
    nr_nam <- NA
    nr_sam <- NA
    
    # subset occ for current species
    spec_df <- subset(all_specs_occ_final, species == spp[spp_index])
    
    # get native occurrences for which the continental code is not NA
    nat <- subset(spec_df, final_status == "native1" | final_status == "native2" & tdwg_l1_code != is.na(NA))
    nr_nat <- nrow(nat)
    save(nat, file = paste0("data/regional_occs/nat2_",spp[spp_index],".RData"))
    
    # get all introduced occurrences for which the continental code is not NA
    intr <- subset(spec_df, final_status == "introduced1" | final_status == "introduced2"  & species == spp[spp_index] & tdwg_l1_code != is.na(NA))
    intr_coords <- terra::vect(data.frame(lon = intr$lon, lat = intr$lat))
    # add crs
    crs(intr_coords) <- crs(tdwg1)
    
    
    # count occurrences over pacific islands and save results
    over_island <- terra::intersect(intr_coords, pacific_islands)
    nr_pac <- length(over_island)
    
    # get coords and use them to select occs from the intr subset
    crds_pac <- as.data.frame(crds(over_island))
    colnames(crds_pac) <- c("lon", "lat")
    pac_df <- semi_join(intr, crds_pac, by = c("lon", "lat")) 
    
    save(pac_df, file=paste0("data/regional_occs/intr2_pac_",spp[spp_index],".RData"))
    
    regs <- unique(intr$tdwg_l1_code)
    regs <- regs[!is.na(regs)]
    
    for(reg in regs){
      
      region_name <- tdwg_info[tdwg_info$LEVEL1_COD == reg,2]
      
      # check which region it is and save nr of occs nd the occurrences itself
      if(region_name == "EUROPE"){
        eur_df <- subset(intr, tdwg_l1_code == reg)
        nr_eur <- nrow(eur_df)
        save(eur_df, file = paste0("data/regional_occs/intr2_eur_",spp[spp_index],".RData"))} # end Europe
      
      if(region_name == "AFRICA"){
        afr_df <- subset(intr, tdwg_l1_code == reg)
        nr_afr <- nrow(afr_df)
        save(afr_df, file = paste0("data/regional_occs/intr2_afr_",spp[spp_index],".RData"))} # end Africa
      
      if(region_name == "ASIA-TEMPERATE"){
        ate_df <- subset(intr, tdwg_l1_code == reg)
        nr_ate <- nrow(ate_df)
        save(ate_df, file = paste0("data/regional_occs/intr2_ate_",spp[spp_index],".RData"))} # end Asia (temperate)
      
      if(region_name == "ASIA-TROPICAL"){
        atr_df <- subset(intr, tdwg_l1_code == reg)
        nr_atr <- nrow(atr_df)
        save(atr_df, file = paste0("data/regional_occs/intr2_atr_",spp[spp_index],".RData"))} # end Asia (temperate)
      
      if(region_name == "AUSTRALASIA"){
        aus_df <- subset(intr, tdwg_l1_code == reg)
        nr_aus <- nrow(aus_df)
        save(aus_df, file = paste0("data/regional_occs/intr2_aus_",spp[spp_index],".RData"))} # end Australasia
      
      if(region_name == "NORTHERN AMERICA"){
        nam_df <- subset(intr, tdwg_l1_code == reg)
        nr_nam <- nrow(nam_df)
        save(nam_df, file = paste0("data/regional_occs/intr2_nam_",spp[spp_index],".RData"))} # end Northern America
      
      if(region_name == "SOUTHERN AMERICA"){
        sam_df <- subset(intr, tdwg_l1_code == reg)
        nr_sam <- nrow(sam_df)
        save(sam_df, file = paste0("data/regional_occs/intr2_sam_",spp[spp_index],".RData"))} # end Asia (temperate)
      
    } # end of for loop over regs
    
    # combine info in df
    occ_count2 <- rbind(occ_count2,
                       data.frame(species = spp[spp_index],
                                  native_occs  = nr_nat,
                                  pac_occs = nr_pac,
                                  eur_occs = nr_eur,
                                  afr_occs = nr_afr,
                                  ate_occs = nr_ate,
                                  atr_occs = nr_atr,
                                  aus_occs = nr_aus,
                                  nam_occs = nr_nam,
                                  sam_occs = nr_sam))
    
  }) # end of foreach-try

save(occ_count2, file = "results/intermediate/occ_count2_intermediate.RData")




# check which species have enough occurrences over the islands (suitable for general analysis)
spp_enough <- subset(occ_count, native_occs >= 20 & pac_occs >= 20) # 506
spp_enough2 <- subset(occ_count2, native_occs >= 20 & pac_occs >= 20) # 558

# check how many species can be for the analysis for each mainland region
eur_enough <- subset(spp_enough, eur_occs >= 20)  # 0   
afr_enough <- subset(spp_enough, afr_occs >= 20)  # 286   
ate_enough <- subset(spp_enough, ate_occs >= 20)  # 223       
atr_enough <- subset(spp_enough, atr_occs >= 20)  # 126
aus_enough <- subset(spp_enough, aus_occs >= 20)  # 313 
nam_enough <- subset(spp_enough, nam_occs >= 20)  # 268
sam_enough <- subset(spp_enough, sam_occs >= 20)  # 290

eur_enough2 <- subset(spp_enough2, eur_occs >= 20)  # 145   
afr_enough2 <- subset(spp_enough2, afr_occs >= 20)  # 345   
ate_enough2 <- subset(spp_enough2, ate_occs >= 20)  # 262       
atr_enough2 <- subset(spp_enough2, atr_occs >= 20)  # 160
aus_enough2 <- subset(spp_enough2, aus_occs >= 20)  # 349 
nam_enough2 <- subset(spp_enough2, nam_occs >= 20)  # 324
sam_enough2 <- subset(spp_enough2, sam_occs >= 20)  # 358

# create list of species with enough occurrences depending on the region
occ_enough <- list(nr_occs = spp_enough, 
                   spp_names = list(general = spp_enough$species, 
                                    europe = eur_enough$species,
                                    africa = afr_enough$species,
                                    a_temperate = ate_enough$species,
                                    a_tropical = atr_enough$species,
                                    australasia = aus_enough$species,
                                    north_america = nam_enough$species,
                                    south_america = sam_enough$species))

occ_enough2 <- list(nr_occs = spp_enough2, 
                    spp_names = list(general = spp_enough2$species, 
                                     europe = eur_enough2$species,
                                     africa = afr_enough2$species,
                                     a_temperate = ate_enough2$species,
                                     a_tropical = atr_enough2$species,
                                     australasia = aus_enough2$species,
                                     north_america = nam_enough2$species,
                                     south_america = sam_enough2$species))

save(occ_enough, file = "results/enough_occs.RData")
save(occ_enough2, file = "results/enough_occs2.RData")

