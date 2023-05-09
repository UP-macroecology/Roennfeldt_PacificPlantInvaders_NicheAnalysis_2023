library(dplyr)
library(sf)

source("scripts/functions.R")
# paths -------------------------------------------------------------------

# cluster
# path_import <- file.path("/import","ecoc9z", "data-zurell", "roennfeldt", "C1")
# path_mnt <- file.path("/mnt", "ibb_share", "zurell", "biodat", "distribution", "Pacific_invaders")

# work laptop
path_home <- "M:/C1/data"
# path_ds <- "Z:/Arbeit/datashare/data/biodat/distribution/Pacific_invaders"

# home office
# path_home <- "Z:/roennfeldt/C1/data"
# path_ds <- "Y:AG26/Arbeit/datashare/data/biodat/distribution/Pacific_invaders" 


# data --------------------------------------------------------------------
load("data/status_info/occ_GIFT_status_max_10km_dist.RData")
load("data/specs_all.RData")

# TDWG level 3 regions (TDWG = Taxonomic Databases Working Group, Regions: World Geographical Scheme for Recording Plant Distributions: https://github.com/tdwg/wgsrpd)
# tdwg <- st_read(file.path(path_mnt,"tdwg_lvl3.geojson"))
tdwg <- st_read(file.path(path_home,"TDWG/wgsrpd-master/geojson/level3.geojson"))



# 1. get POWO names -------------------------------------------------------


# remove secies names that always cause errors
flagged_names <- c("Acoelorraphe wrightii")
specs_all <- specs_all[!specs_all %in% flagged_names]

# prepare empty df to store info 
powo_page_inf <- data.frame(searched_name = character(),
                            lcvp_name = character(),
                            powo_name = character(),
                            powo_url = character(),
                            ipni_id = character(),
                            stringsAsFactors = FALSE)


# define species for which powo page information has not yet been checked
specs_done <- unique(powo_page_inf$searched_name)

specs_left <- setdiff(specs_all, specs_done)

# set counter to keep track on how many species have been through the loop
counter <- 0

#run loop over species
for(spec in specs_left){
  
  counter <- counter + 1
  print(counter)
  
  powo_page_inf <- bind_rows(powo_page_inf,
                             getPowoNames(spec, incl_lcvp_synonyms = TRUE, perfect_matches_only = TRUE))
  
  # stop after xx species (e.g. 200)
  if (counter >= 320){
    break
  } # end of if 
  
} # end of for loop over specs_left

save(powo_page_inf, file = "data/status_info/powo_page_inf.RData")


# 1.2. manually select POWO pages for species names for which no perfect match was found in step 1 ####
specs_no_powo_inf <- powo_page_inf %>% # for 279 species no matching POWO entry is found
  filter(is.na(powo_name)) %>%
  pull(searched_name)


# execute getPowoNames again for these species, but without matching author information (perfect_matches_only = FALSE):
# (getPowoNames defined in utils_1-5_dataprep.R)
powo_page_opts <- bind_rows(lapply(specs_no_powo_inf, getPowoNames, incl_lcvp_synonyms = TRUE, perfect_matches_only = FALSE))

save(powo_page_opts, file = "data/intermediate/powo_page_opts.RData")

# manually select POWO pages that probably match the species names in the blacklist:
powo_page_manually_matched <- powo_page_opts[c(1, 3, 6, 7, 9, 11, 12, 14, 15, 17, 18, 20, 23, 24, 25,
                                               28, 31, 32, 34, 35, 37, 38, 40, 46, 47, 48, 51, 54, 56,
                                               57, 58, 61, 62, 64, 65, 66, 68, 69, 71, 74, 75, 81, 84,
                                               86, 87, 88, 90, 91, 92, 93, 95, 96, 97, 98, 99, 101, 104,
                                               105, 107, 109, 113, 115, 117, 119, 120, 121, 122, 124,
                                               127, 128, 130, 131, 133, 134, 135, 140, 144, 146, 151,
                                               152, 152, 157, 159, 160, 161, 162, 163, 172, 175, 177,
                                               179, 181, 182, 183, 184, 187, 188, 189, 190, 193, 194, 
                                               195, 196, 197, 201, 203, 205, 206, 209, 211, 215, 217,
                                               220, 222, 224, 225, 227, 228, 230, 231, 233, 234, 237,
                                               238, 241, 243, 244, 245, 247, 248, 250, 257, 258, 259,
                                               260, 261, 262, 263, 265, 266, 267, 268, 270, 272, 273,
                                               274, 279, 281, 285, 287, 288, 289, 292, 295, 296, 297, 
                                               300, 304, 305, 306, 307, 310, 311, 312, 313, 315, 316, 
                                               317, 319, 320, 322, 324, 327, 328, 329, 331, 333, 336, 
                                               337, 339, 340),] # actual index values would be different

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

