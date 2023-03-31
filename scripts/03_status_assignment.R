library(doParallel)
library(dplyr)
library(sf)
library(tidyverse)
library(lwgeom)

# remove everything in the working environment:
rm(list = ls())

# make functions defined in utils.R available;
# these are:
# - getPowoNames()
# - getPowoStatus()
# - checkPowoStatus()
# - adaptPowoStatus()
# - getGiftNames()
# - getGiftStatusInf()

source("scripts/utils.R")

# required paths ------

path_import <- file.path("/import","ecoc9z", "data-zurell", "roennfeldt", "C1")
path_mnt <- file.path("/mnt", "ibb_share", "zurell", "biodat", "distribution", "Pacific_invaders")


# 
# # path_user <- "//ibb-fs01.ibb.uni-potsdam.de/users$/roennfeldt/C1/data" # TODO: maybe delete this path if it is not used on a regular bases 
# path_ds <- "Z:Arbeit/datashare/data/biodat/distribution/Pacific_invaders" # when working from the work laptop
# # path_ds <- "Z:AG26/Arbeit/datashare/data/biodat/distribution/Pacific_invaders" # when working from home

# required data ------

# TDWG level 3 regions (TDWG = Taxonomic Databases Working Group, Regions: World Geographical Scheme for Recording Plant Distributions: https://github.com/tdwg/wgsrpd)
tdwg <- st_read(file.path(path_mnt,"tdwg_lvl3.geojson"))

# occurrences (output of 2_data_prep.R)
load("data/all_species/occ_cleaned_slim.RData")

#------------------------------------#
####            POWO              ####
#------------------------------------#

# 1. get POWO names -----

# define species for which status information is required
specs_all <- sort(unique(occ_cleaned_slim$species))

# remove species that always cause problems 
flagged_names <- c("Acoelorraphe wrightii")
specs_all <- specs_all[!specs_all %in% flagged_names]


# prepare empty df to store info 
powo_page_inf <- data.frame(searched_name = character(),
                            lcvp_name = character(),
                            powo_name = character(),
                            powo_url = character(),
                            ipni_id = character(),
                            stringsAsFactors = FALSE)

# load half done object to continue work
# load("results/intermediate/powo_page_inf_half.RData")

# define species for which powo page information has not yet been checked
specs_done <- unique(powo_page_inf$searched_name)

specs_left <- setdiff(specs_all, specs_done)

# set counter to keep track on how many species have been through the loop
counter <- 0

# run loop over species
for(spec in specs_left){
  
  counter <- counter + 1
  print(counter)
  
  powo_page_inf <- bind_rows(powo_page_inf,
                             getPowoNames(spec, incl_lcvp_synonyms = TRUE, perfect_matches_only = TRUE))
  
  # stop after xx species (e.g. 200)
  if (counter >= 67){
    break
  } # end of if 
  
} # end of for loop over specs_left

# save(powo_page_inf, file = "results/intermediate/powo_page_inf_half.RData")

# 1.2) manually select POWO pages for species names for which no perfect match was found in step 1)
specs_no_powo_inf <- powo_page_inf %>% # for 12 species no matching POWO entry is found
  filter(is.na(powo_name)) %>%
  pull(searched_name)

# execute getPowoNames again for these species, but without matching author information (perfect_matches_only = FALSE):
# (getPowoNames defined in utils_1-5_dataprep.R)
powo_page_opts <- bind_rows(lapply(specs_no_powo_inf, getPowoNames, incl_lcvp_synonyms = TRUE, perfect_matches_only = FALSE))

# save(powo_page_opts, file = "results/intermediate/powo_page_opts.RData")

# manually select POWO pages that probably match the species names in the blacklist:
# powo_page_manually_matched <- powo_page_opts[c(1,3,4,5,6, 8, 10, 11, 12, 14),] # actual index values would be different

# all POWO pages from which status information will be obtained:

# original version (for when manual selection has been made)
# powo_page_inf_final <-powo_page_inf %>%
#   filter(!is.na(powo_name)) %>%
#   rbind(powo_page_manually_matched)


# code version for now:
powo_page_inf_final <- powo_page_inf %>%
  filter(!is.na(powo_name))

# save(powo_page_inf_final, file = "results/intermediate/powo_page_inf_final.RData")


# free up memory
rm(occ_cleaned_slim)
rm(powo_page_opts)
rm(powo_page_inf)

# 2.  get POWO status information ----------------------------------------------

# (getPowoStatus defined in utils_1-5_dataprep.R)

# use IPNI IDs to get distribution information from POWO:
# IPNI = International Plant Name Index: https://www.ipni.org/
powo_status_inf <- bind_rows(apply(powo_page_inf_final, 1,
                                   function(x){getPowoStatus(x["ipni_id"], x["lcvp_name"])}))


# load("results/intermediate/powo_page_inf_final.RData") # only for the testing phase; remove later

# join blacklist species names:
powo_status_inf <- powo_status_inf %>%
  left_join(powo_page_inf_final) %>%
  select(-powo_url) #7843 entries

# save(powo_status_inf, file = "results/intermediate/powo_status_inf.RData")

# 3. check for conflicting/matching status -----
#  relevant if more than one POWO page is found for a blacklist species name:
# (checkPowoStatus defined in utils_1-5_dataprep.R)

powo_status_check <- lapply(specs_all, checkPowoStatus, powo_stat_dt = powo_status_inf)
# contradictions:
powo_status_contr <- map_dfr(powo_status_check, ~.x$distr_contr) # 1996 entries for 191 unique species 
# agreements:
powo_status_match <- map_dfr(powo_status_check, ~.x$distr_match) # 78685

# # 3.2) manually re-assign some (or all) conflicting status information:
# # (adaptPowoStatus defined in utils_1-5_dataprep.R)
# ## (all that are not reassigned, get status "native" in the next step)
# ## (in this example, in case of conflicting information, only the status information of those species names is used that the LCVP search would yield if only accepted species names, and not also synonyms, were considered)
# powo_status_inf_mod <- adaptPowoStatus(spec = "Allamanda schottii", new_status = "Allamanda schottii Pohl", 
#                                        powo_stat_dt = powo_status_inf) #!
# powo_status_inf_mod <- adaptPowoStatus(spec = "Alysicarpus vaginalis", new_status = "Alysicarpus vaginalis (L.) DC.", 
#                                        powo_stat_dt = powo_status_inf_mod)  #!
# powo_status_inf_mod <- adaptPowoStatus(spec = "Asclepias curassavica", new_status = "Asclepias curassavica L.", 
#                                        powo_stat_dt = powo_status_inf_mod)  #!
# powo_status_inf_mod <- adaptPowoStatus(spec = "Chloris radiata", new_status = "Chloris radiata (L.) Sw.", 
#                                        powo_stat_dt = powo_status_inf_mod)  #!
# powo_status_inf_mod <- adaptPowoStatus(spec = "Crotalaria pallida", new_status = "Crotalaria pallida Aiton", 
#                                        powo_stat_dt = powo_status_inf_mod)  #!
# powo_status_inf_mod <- adaptPowoStatus(spec = "Rumex spinosus", new_status = "Rumex spinosus L.", 
#                                        powo_stat_dt = powo_status_inf_mod)  #!
# powo_status_inf_mod <- adaptPowoStatus(spec = "Tribulus terrestris", new_status = "Tribulus terrestris L.", 
#                                        powo_stat_dt = powo_status_inf_mod)  #!
# powo_status_inf_mod <- adaptPowoStatus(spec = "Vitis vinifera", new_status = "Vitis vinifera L.", 
#                                        powo_stat_dt = powo_status_inf_mod)  #!


# create the object called "powo_status_inf_mod" which should be identical to powo_status_inf (because I'm not manually reassigning the info)

powo_status_inf_mod <- powo_status_inf

# 4) final data frame with status information for all available species - region combinations:
# includes: name in blacklist - TDWG region name and code - status:
powo_dt <- powo_status_inf_mod %>% #!
  relocate(searched_name, tdwgName, tdwgCode, status, lcvp_name, powo_name, ipni_id) %>%
  distinct(searched_name, tdwgCode, status, .keep_all = TRUE) %>% # remove duplicates, resulting from matching status information of multiple POWO pages found for one blacklist species name
  group_by(searched_name, tdwgCode) %>% # assign all conflicting status information (that have not been manually reassigned in the previous step) the status "native"
  mutate(n = n()) %>%
  mutate(status = if_else(n == 2, "native", status)) %>%
  select(-n) %>%
  ungroup %>%
  distinct()  # remove duplicates, resulting from assigning all conflicting status information the status "native"

# save(powo_dt, file = "results/status_info/powo_status_information.RData")

# load("data/powo_status_information.RData") # status information on 143625 combinations of species and region (originally 7344 for the 120 species)  

# free memory:
rm(powo_status_check)
rm(specs_no_powo_inf)
rm(powo_status_inf)
rm(powo_status_inf_mod)


# ---------------------------------------------------- #
#   Assign POWO status information to occurrences   ####
# ---------------------------------------------------- #

# 1. harmonize region names ----------------------------
# (align POWO region names to TDWG region names)

POWO_regions <- sort(unique(powo_dt$tdwgName))
TDWG_regions <- sort(unique(tdwg$LEVEL3_NAM))
setdiff(POWO_regions, TDWG_regions)
setdiff(TDWG_regions, POWO_regions)

powo_dt_harmonized <- powo_dt %>%
  mutate(tdwgName = recode(tdwgName,
                           `Amsterdam-St.Paul Is` = "Amsterdam-St.Paul Is.",
                           `Central African Repu` = "Central African Republic",
                           `Central American Pac` = "C. American Pacific Is.",
                           `Central European Rus` = "Central European Russia",
                           `Cocos (Keeling) Is.` = "Cocos (Keeling) I.",
                           `Gambia` = "Gambia, The",
                           `Kirgizstan` = "Kirgizistan",
                           `Leeward Is.`  = "Leeward Is. AB Ant",
                           `Marion-Prince Edward` = "Marion-Prince Edward Is.",
                           `Mozambique Channel I` = "Mozambique Channel Is.",
                           `North European Russi` = "North European Russia",
                           `Northwest European R` = "Northwest European Russia",
                           `Northwest Territorie` = "Northwest Territories",
                           `Panamá` = "Panama",
                           `South European Russi` = "South European Russia",
                           `Suriname` = "Surinam"))

POWO_regions2 <- sort(unique(powo_dt_harmonized$tdwgName))
setdiff(POWO_regions2, TDWG_regions) # should be empty (character(0))!
setdiff(TDWG_regions, POWO_regions2) # doesn't need to be empty; regions without occurrences of blacklist species

# save(powo_dt_harmonized, file = "results/intermediate/powo_dt_harmonized.RData")

# free memory:
# rm(powo_dt)
# rm(POWO_regions)
# rm(TDWG_regions)
# rm(POWO_regions2)

# 2. spatial joining ------------------------------------

# join occurrences and TDWG level 3 regions spatially

# convert occurrences to spatial data
occ_sf <-  occ_cleaned_slim %>%
  filter(species %in% specs_all) %>%
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(tdwg)) #  26,186,010 occurrences for all species (old: 1'122'764 occurrences of species in blacklist)

# !! Skipped this step, because the file was too big
# write to shapefile to examine in QGIS:
# st_write(occ_sf, "data/occ_all.shp")


# join TDWG region in which an occurrence is located:
sf_use_s2(FALSE) # switch off spherical geometry package S2, otherwise join doesn't work due to invalid geometries
occ_tdwg_sf <- st_join(occ_sf, tdwg, st_intersects, left = TRUE) # all occurrences are kept, also those that don't intersect with a region
# 26,243,289 occurrences (old: 1,125,053), ca. 57,279 occurrences joined to two regions (when regions overlap at the occurrence's location)

# save(occ_tdwg_sf, file = "results/intermediate/occurrences_tdwg_region_sh.RData")

# 2.1 additionally join occurrences which don't overlap with TDWG region to nearest TDWG region if it is not more than 10 km away:

# the outlines of the TDWG regions are defined rather roughly, there are gaps between
# adjacent regions and they don't follow the coastlines exactly, thus there are occurrences
# which don't spatially overlap with any TDWG region, these later get status 'unknown',
# to reduce number of occurrences with unknown status, we match occurrence to the nearest TDWG regions,
# if they are less than 10 km apart (10 km = maximum coordinate uncertainty, defined in 3_occurrence_cleaning.R)

# occurrences that don't intersect with a TDWG level 3 region:
occ_no_tdwg_sf <- occ_tdwg_sf %>%
  filter(is.na(LEVEL3_COD)) %>%
  select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD) # 2,144,549 (82'082 occurrences)

# nearest TDWG region for each of these occurrences:
nearest_tdwg <- st_nearest_feature(x = occ_no_tdwg_sf, y = tdwg, longlat = TRUE) # returns index of nearest feature
nearest_tdwg_sf <- tdwg[nearest_tdwg,]

# distance of each occurrence to nearest TDWG region (calculation may take a while!):
dist_nearest_tdwg <- st_distance(occ_no_tdwg_sf,
                                 nearest_tdwg_sf,
                                 by_element = TRUE,
                                 tolerance = units::set_units(10000, m)) # first distance smaller than tolerance will be returned, true distance may be smaller


# did next step via the cluster

# # use nearest TDWG region, if distance is not larger than 10 km:
# dist_okay <- which(dist_nearest_tdwg <= units::set_units(10000, m))
# nearest_tdwg <- nearest_tdwg_sf %>% st_drop_geometry
# occ_no_tdwg <- occ_no_tdwg_sf %>% st_drop_geometry
# occ_no_tdwg[dist_okay, c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")] <- nearest_tdwg[dist_okay, c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")]
# 
# # insert into complete data set:
# occ_tdwg_sf[is.na(occ_tdwg_sf$LEVEL3_COD), c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")] <- occ_no_tdwg[,c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")]
# # 9332 occurrences without TDWG region
# 
# # free memory:
# rm(occ_no_tdwg)
# rm(occ_no_tdwg_sf)
# rm(nearest_tdwg)
# rm(nearest_tdwg_sf)
# rm(dist_nearest_tdwg)
# rm(dist_okay)

# load("results/intermediate/species_names.RData")# TODO: remove later on
# load("results/intermediate/powo_dt_harmonized.RData") # TODO: remove later on
# load("results/intermediate/occ_tdwg_sf.RData")


# 3) join occurrences and status information based on TDWG region:

# on the cluster:
# 
# all_species_occ_status <- occ_tdwg_sf %>%
#   mutate(lon = st_coordinates(.)[,1],lat = st_coordinates(.)[,2]) %>% # add coordinates to data frame for plotting
#   st_drop_geometry() %>%
#   select(-c(country, year, datasource, dataset, native)) %>%
#   left_join(powo_dt_harmonized, by = c("species" = "searched_name", "LEVEL3_NAM" = "tdwgName")) %>% # join occurrences and status, all occurrences included
#   group_by(occ_id) %>%         # some occurrences have been matched to 2 TDWG regions
#   arrange(desc(status)) %>%    # so use only one status (priority: native > introduced > NA)
#   slice(1) %>%                 # this operation is somewhat expensive, so be careful with >> 1M records
#   ungroup() %>%
#   mutate(status = replace_na(status, "unknown")) %>% # all occurrences which could not be matched to a status get status "unknown"
#   select(occ_id, lon, lat, species, tdwg_l3_name = LEVEL3_NAM, tdwg_l3_code = LEVEL3_COD,
#          tdwg_l2_code = LEVEL2_COD, tdwg_l1_code = LEVEL1_COD, status_POWO = status,
#          lcvp_name, powo_name, ipni_id)
# 
# save(all_specs_occ_status, file = paste0(path_imp, "/output/all_specs_occ_status.RData"))


# load("results/intermediate/all_specs_occ_status.RData")
