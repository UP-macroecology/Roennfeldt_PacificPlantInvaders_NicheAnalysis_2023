library(dplyr)
library(sf)
library(stringr)

# required packages -------------------------------------------------------

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "sf", "doParallel", "foreach" # names of the packages required placed here as character objects
)
sapply(package_vec, install.load.package)


rm(list = ls())

# source("functions.R")
#source("scripts/functions.R")

# paths -------------------------------------------------------------------

# cluster
path_imp <- file.path("/import","ecoc9z", "data-zurell", "roennfeldt", "C1")
# path_mnt <- file.path("/mnt", "ibb_share", "zurell", "biodat", "distribution", "Pacific_invaders")

# work laptop
path_home <- "M:/C1/data"
# path_ds <- "Z:/Arbeit/datashare/data/biodat/distribution/Pacific_invaders"

# home office
# path_home <- "Z:/roennfeldt/C1/data"
# path_ds <- "Y:AG26/Arbeit/datashare/data/biodat/distribution/Pacific_invaders" 


# required data ----------------------------------------------------------------
# load(paste0(path_imp,"/input_data/specs_all.RData"))
# load(paste0(path_imp,"/input_data/occ_cleaned_slim.RData"))

# load("data/specs_all.RData")

# remove ACOELORRAPHE WRIGHTII because it causes errors
# flagged_names <- c("Acoelorraphe wrightii")
# specs_all <- specs_all[!specs_all %in% flagged_names]
# 
# 
# 

# get GIFT status information ---------------------------------------------

# 1) find corresponding species names in GIFT

# prepare empty df to store info
# GIFT_names <- data.frame(searched_name = character(),
#                          GIFT_genus = character(),
#                          GIFT_species_ep = character(),
#                          stringsAsFactors = FALSE)

# load("data/status_info/GIFT_names.RData")
#
# # define species for which powo page information has not yet been checked
# specs_done <- unique(GIFT_names$searched_name)
# specs_left <- setdiff(specs_all, specs_done)
#
# counter <- 0


# # run loop over species
# for(spec in specs_left){
#
#   counter <- counter + 1
#   print(counter)
#
#   GIFT_names <- bind_rows(GIFT_names,
#                           getGiftNames(spec, incl_lcvp_synonyms = TRUE))
#
#   # stop after xx species (e.g. 200)
#   if (counter >= 221){
#     break
#   } # end of if
#
# } # end of for loop over specs_left
# save(GIFT_names, file = "data/status_info/GIFT_names.RData")

#
# # sometimes, faulty internet connections can lead cause NA assignments
#
# # identify species with NA and re-run the above code for these
# specs_NA <- specs_all[which(is.na(GIFT_names$GIFT_genus))]
#
# # prepare empty df to store info
#
# GIFT_names_NA <- data.frame(searched_name = character(),
#                          GIFT_genus = character(),
#                          GIFT_species_ep = character(),
#                          stringsAsFactors = FALSE)
#
# counter <- 0
#
# # run loop over species
# for(spec in specs_NA){
#
#   counter <- counter + 1
#   print(counter)
#
#   GIFT_names_NA <- bind_rows(GIFT_names_NA,
#                           getGiftNames(spec, incl_lcvp_synonyms = TRUE))
#
#   # stop after xx species (e.g. 200)
#   if (counter >= 52){
#     break
#   } # end of if
#
# } # end of for loop over specs_NA
#
# # bind info together
# GIFT_names <- rbind(GIFT_names, GIFT_names_NA) %>%
#   na.omit() # removes duplicates and remaining NAs
#
# save(GIFT_names, file = "data/status_info/GIFT_names_no_NA.RData")
#
# # 2) extract status information from GIFT:
# # (getGiftStatusInf defined in utils_1-5_dataprep.R)
# GIFT_status <- vector("list", length = nrow(GIFT_names))
# for (s in 1:nrow(GIFT_names)) {
#   print(s)
#   GIFT_status[[s]] <- getGiftStatusInf(searched_name = GIFT_names$searched_name[s],
#                                        GIFT_spec_genus = GIFT_names$GIFT_genus[s],
#                                        GIFT_spec_epithet = GIFT_names$GIFT_species_ep[s])
# }
#
#
# list_index <- NULL
# for(s in 1:length(GIFT_status)){
#
#   # collect indices of list element without GIFT information (empty df)
#   if(nrow(GIFT_status[[s]]) == 0){list_index <- c(list_index, s)}
#
# } # end of loop over list indices
#
# # remove empty elements from list
# GIFT_status <- GIFT_status[-list_index]
#
# GIFT_status_all_details <- bind_rows(GIFT_status)
# save(GIFT_status, file = paste0("data/status_info/GIFT_status_all_details.RData"))
#
#
# rm(list_index)
#
# #rename the statuses based on the distinct combinations
# # based on: https://biogeomacro.github.io/GIFT/articles/GIFT_tutorial.html#species-distribution
#
# GIFT_status <- GIFT_status_all_details %>%
#   mutate(status_GIFT = case_when(
#     native == "native" & naturalized == "non-naturalized" ~ "native",
#     native == "native" & is.na(naturalized) ~ "native",
#     native == "non-native" & is.na(naturalized) ~ "non-native",
#     native == "non-native" & naturalized == "naturalized" ~ "naturalized",
#     native == "non-native" & naturalized == "non-naturalized" ~ "non-native",
#     is.na(native) & is.na(naturalized) ~ "unknown"
#   )) %>%
#   select(species, GIFT_species, entity_ID, status_GIFT)
#
# save(GIFT_status, file = "data/status_info/GIFT_status.RData")
# rm(GIFT_status)
#
# # 3) load spatial data of the GIFT regions with status information for the considered species:
# GIFT_polygons <- GIFT_shape(unique(GIFT_status_all_details$entity_ID), GIFT_version = "beta")
# save(GIFT_polygons, file = "data/status_info/Gift_polygons.RData")
#

# assign GIFT status to occurrences ---------------------------------------

# register cores for parallel computation to speed it up:


# load cluster data -------------------------------------------------------
load(file.path(path_imp, "input_data", "specs_all.RData"))
load(file.path(path_imp, "input_data", "Gift_polygons.RData"))
load(file.path(path_imp, "input_data", "GIFT_status.RData"))
load(file.path(path_imp, "input_data", "occ_cleaned_slim.RData"))
tdwg <- st_read(file.path(path_imp, "input_data", "tdwg_lvl3.geojson"))

no_cores <- 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)

specs_all <- specs_all

occ_GIFT_status <- foreach(s = 1:length(specs_all), .packages = c("dplyr", "sf"),
                           .combine = "rbind", .verbose = TRUE) %dopar% {

                             # GIFT status data for species s:
                             GIFT_status_spec <- GIFT_status %>%
                               filter(species == specs_all[s])

                             # GIFT polygon IDs:
                             polygon_IDs_spec <- GIFT_status_spec$entity_ID

                             # GIFT polygons as sf:
                             GIFT_polygons_spec <- GIFT_polygons %>%
                               filter(entity_ID %in% polygon_IDs_spec)

                             # occurrences of species s:
                             # subset occurrences for s
                             occ_sf_spec <-  occ_cleaned_slim %>%
                               filter(species == specs_all[s]) %>%
                               st_as_sf(coords = c("lon", "lat"), crs = st_crs(tdwg))


                             sf_use_s2(FALSE) # switch off spherical geometry package S2

                             # spatially join occurrences and polygons:
                             occ_GIFT_poly_spec <- occ_sf_spec %>%
                               select(occ_id, species) %>%
                               st_join(GIFT_polygons_spec, st_intersects, left = TRUE) %>% # occurrence must be inside the polygon
                               st_drop_geometry()

                             if(nrow(occ_GIFT_poly_spec) == 0) return(occ_GIFT_poly_spec) # no occurrences for the species

                             # additionally match occurrences not inside any GIFT region to the closest GIFT region
                             # with status information, if it is <= 10 km away:

                             # occurrence ID not joined:
                             occ_ID_no_Gift_poly <- occ_GIFT_poly_spec %>%
                               filter(is.na(entity_ID)) %>%
                               pull(occ_id)

                             if(length(occ_ID_no_Gift_poly) != 0) {

                               # occurrences not joined as sf :
                               occ_sf_spec_no_Gift_poly <- occ_sf_spec %>%
                                 filter(occ_id %in% occ_ID_no_Gift_poly)

                               # nearest GIFT region for each of these occurrences:
                               nearest_GIFT_region <- st_nearest_feature(x = occ_sf_spec_no_Gift_poly, y = GIFT_polygons_spec, longlat = TRUE)
                               nearest_GIFT_region_sf <- GIFT_polygons_spec[nearest_GIFT_region,]  %>%
                                 cbind("occ_id" = occ_sf_spec_no_Gift_poly$occ_id)

                               # distance of each occurrence to nearest GIFT region:
                               dist_nearest_GIFT_region <- st_distance(occ_sf_spec_no_Gift_poly,
                                                                       nearest_GIFT_region_sf,
                                                                       by_element = TRUE,
                                                                       tolerance = units::set_units(10000, m))

                               # keep matched regions if distance is <= 10km:
                               occ_GIFT_max10kmdist <- nearest_GIFT_region_sf %>%
                                 mutate(distance_m = dist_nearest_GIFT_region) %>%
                                 filter(distance_m <= units::set_units(10000, m)) %>%
                                 st_drop_geometry() %>%
                                 select(-distance_m)

                               # join to all occurrences:
                               occ_GIFT_poly_spec_max10kmdist <- occ_GIFT_poly_spec %>%
                                 left_join(occ_GIFT_max10kmdist, by = "occ_id") %>%
                                 mutate(entity_ID = ifelse(!is.na(entity_ID.x), entity_ID.x, entity_ID.y)) %>%
                                 mutate(geo_entity = ifelse(!is.na(geo_entity.x), geo_entity.x, geo_entity.y)) %>%
                                 mutate(area = ifelse(!is.na(area.x), area.x, area.y)) %>%
                                 mutate(polygon_source = ifelse(!is.na(polygon_source.x), polygon_source.x, polygon_source.y)) %>%
                                 select(occ_id, species, entity_ID, geo_entity, area, polygon_source)

                             } else {occ_GIFT_poly_spec_max10kmdist <- occ_GIFT_poly_spec %>%
                               select(occ_id, species, entity_ID, geo_entity, area, polygon_source)
                             }

                             # join status information to GIFT regions:
                             occ_GIFT_status_spec <- occ_GIFT_poly_spec_max10kmdist %>%
                               left_join(GIFT_status_spec)

                             # since polygons are nested, more than 1 polygon may be joined to a single occurrence (e.g. Paraguay and Southern South America)
                             # in the following I use for each occurrence the status information belonging to the smaller polygon:
                             occ_GIFT_status_spec_final <- occ_GIFT_status_spec %>%
                               group_by(occ_id) %>%
                               arrange(area, .by_group = TRUE) %>%
                               slice(1) %>%
                               ungroup
                           }

save(occ_GIFT_status, file = paste0(path_imp, "/output/occ_GIFT_status_max_10km_dist.RData"))

# stop the cluster
stopCluster(cl)
