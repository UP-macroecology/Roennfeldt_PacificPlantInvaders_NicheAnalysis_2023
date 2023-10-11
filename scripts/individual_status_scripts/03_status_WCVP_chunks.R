library(dplyr)
library(sf)
library(foreach)
library(doSNOW)
library(progress)

filter <- dplyr::filter

load("data/status_assignment/wcvp_distribution.RData")
load("data/occ_cleaned_slim.RData")
tdwg <- st_read("data/tdwg/geojson/level3.geojson")


wcvp_status_long <- wcvp_status
specs_initial <- unique(occ_cleaned_slim$species)



specs_wcvp <- specs_initial[1:27]
wcvp_status <- wcvp_status_long[wcvp_status_long$species %in% specs_wcvp,]




# load("data/occ_cleaned_slim.RData")
# load("data/status_assignment/wcvp_distribution.RData") # object is called wcvp_status
# tdwg <- st_read("data/tdwg/geojson/level3.geojson")


# progress bar 
pb <- progress_bar$new(
  format = "species = :letter [:bar] :elapsed | eta: :eta",
  total = length(specs_wcvp),    # 100 
  width = 60)

# allowing progress bar to be used in foreach -----------------------------
progress <- function(n){
  pb$tick(tokens = list(letter = specs_wcvp[n]))
} 

opts <- list(progress = progress)

specs_done <- unique(occ_wcvp_status_1$species)

specs_left <- setdiff(specs_initial, specs_done)

specs_wcvp <- specs_left[1:260]


wcvp_status <- wcvp_status_long[wcvp_status_long$species %in% specs_wcvp,]

# progress bar 
pb <- progress_bar$new(
  format = "species = :letter [:bar] :elapsed | eta: :eta",
  total = length(specs_wcvp),    # 100 
  width = 60)

# allowing progress bar to be used in foreach -----------------------------
progress <- function(n){
  pb$tick(tokens = list(letter = specs_wcvp[n]))
} 

opts <- list(progress = progress)



no_cores <- 2
cl <- makeCluster(no_cores)
registerDoSNOW(cl)

occ_wcvp_status_3 <- foreach(s = 1:length(specs_wcvp), .packages = c("dplyr", "sf", "units"),
                             .combine = "rbind", .verbose = TRUE, .options.snow = opts) %dopar% { 
                               
                               
                               # WCVP status data for species s:
                               wcvp_status_spec <- wcvp_status %>%
                                 filter(species == specs_wcvp[s])
                               
                               # occurrences of species s:
                               occ_sf_spec <-  occ_cleaned_slim %>%
                                 filter(species == specs_wcvp[s]) %>%
                                 st_as_sf(coords = c("lon", "lat"), crs = st_crs(tdwg))
                               
                               sf_use_s2(FALSE) # switch off spherical geometry package S2
                               
                               # spatially join occurrences and polygons:
                               occ_wcvp_poly_spec <- occ_sf_spec %>%
                                 select(occ_id, species) %>%
                                 st_join(wcvp_status_spec, st_intersects, left = TRUE) %>% # occurrence must be inside the polygon
                                 st_drop_geometry() %>%
                                 rename("species" = "species.x") %>%
                                 select(-species.y)
                               
                               
                               if (nrow(occ_wcvp_poly_spec) == 0) return(occ_wcvp_poly_spec) # no occurrences for the species
                               
                               # additionally match occurrences not inside any WCVP region to the closest WCVP region
                               # with status information, if it is <= 10 km away:
                               
                               # occurrence ID not joined:
                               occ_ID_no_wcvp_poly <- occ_wcvp_poly_spec %>%
                                 filter(is.na(LEVEL3_NAM)) %>%
                                 pull(occ_id)
                               
                               
                               if (length(occ_ID_no_wcvp_poly) != 0) {
                                 
                                 # occurrences not joined as sf :
                                 occ_sf_spec_no_wcvp_poly <- occ_sf_spec %>%
                                   filter(occ_id %in% occ_ID_no_wcvp_poly)
                                 
                                 # nearest WCVP region for each of these occurrences:
                                 nearest_wcvp_region <- st_nearest_feature(x = occ_sf_spec_no_wcvp_poly, y = wcvp_status_spec, longlat = TRUE)
                                 nearest_wcvp_region_sf <- wcvp_status_spec[nearest_wcvp_region,]  %>%
                                   cbind("occ_id" = occ_sf_spec_no_wcvp_poly$occ_id)
                                 
                                 # distance of each occurrence to nearest GIFT region:
                                 dist_nearest_wcvp_region <- st_distance(occ_sf_spec_no_wcvp_poly,
                                                                         nearest_wcvp_region_sf,
                                                                         by_element = TRUE,
                                                                         tolerance = units::set_units(10000, m))
                                 
                                 # keep matched regions if distance is <= 10km:
                                 occ_wcvp_max10kmdist <- nearest_wcvp_region_sf %>%
                                   mutate(distance_m = dist_nearest_wcvp_region) %>%
                                   filter(distance_m <= units::set_units(10000, m)) %>%
                                   st_drop_geometry() %>%
                                   select(-distance_m)
                                 
                                 if(length(occ_wcvp_max10kmdist) != 0) {
                                   # join to all occurrences:
                                   occ_wcvp_poly_spec_max10kmdist <- occ_wcvp_poly_spec %>%
                                     left_join(occ_wcvp_max10kmdist, by = c("occ_id", "species")) %>%
                                     mutate(LEVEL3_NAM = ifelse(!is.na(LEVEL3_NAM.x), LEVEL3_NAM.x, LEVEL3_NAM.y)) %>%
                                     mutate(LEVEL3_COD = ifelse(!is.na(LEVEL3_COD.x), LEVEL3_COD.x, LEVEL3_COD.y)) %>%
                                     mutate(LEVEL2_COD = ifelse(!is.na(LEVEL2_COD.x), LEVEL2_COD.x, LEVEL2_COD.y)) %>%
                                     mutate(LEVEL1_COD = ifelse(!is.na(LEVEL1_COD.x), LEVEL1_COD.x, LEVEL1_COD.y)) %>%
                                     mutate(occurrence_type = ifelse(!is.na(occurrence_type.x), occurrence_type.x, occurrence_type.y)) %>%
                                     mutate(area = ifelse(!is.na(area.x), area.x, area.y)) %>%
                                     select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD, area)
                                 } else {
                                   occ_wcvp_poly_spec_max10kmdist <- occ_wcvp_poly_spec %>%
                                     select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD, area)
                                 }
                                 
                                 
                               } else {occ_wcvp_poly_spec_max10kmdist <- occ_wcvp_poly_spec %>%
                                 select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD, area)
                               }
                                 
                                 # join status information to WCVP regions:
                                 occ_wcvp_status_spec <- occ_wcvp_poly_spec_max10kmdist %>%
                                   left_join(wcvp_status_spec)# end of try
                               
                               # since polygons are nested, more than 1 polygon may be joined to a single occurrence (e.g. Paraguay and Southern South America)
                               # in the following, for each occurrence the status information belonging to the smaller polygon is used:
                               occ_wcvp_status_spec_final <- occ_wcvp_status_spec %>%
                                 select(-geometry) %>%
                                 group_by(occ_id) %>%
                                 arrange(area, .by_group = TRUE) %>%
                                 slice(1) %>%
                                 ungroup 
                               
                               
                             } # end of foreach

load("data/status_assignment/occ_wcvp_status/occ_WCVP_status_1.RData")

specs_covered_1 <- unique(occ_wcvp_status_1$species)
rm(occ_wcvp_status_1)

load("data/status_assignment/occ_wcvp_status/occ_WCVP_status_2.RData")
specs_covered_2 <- unique(occ_wcvp_status_2$species)
rm(occ_wcvp_status_2)
 
load("data/status_assignment/occ_wcvp_status/occ_WCVP_status_3.RData") 

specs_done <- c(specs_covered_1, specs_covered_2, unique(occ_wcvp_status_3$species) )
specs_left <- setdiff(specs_initial, specs_done)

specs_wcvp <- specs_left[1:100]


wcvp_status <- wcvp_status_long[wcvp_status_long$species %in% specs_wcvp,]
# rm(wcvp_status_long)

# progress bar 
pb <- progress_bar$new(
  format = "species = :letter [:bar] :elapsed | eta: :eta",
  total = length(specs_wcvp),    # 100 
  width = 60)

# allowing progress bar to be used in foreach -----------------------------
progress <- function(n){
  pb$tick(tokens = list(letter = specs_wcvp[n]))
} 

opts <- list(progress = progress)



no_cores <- 2
cl <- makeCluster(no_cores)
registerDoSNOW(cl)

occ_wcvp_status_new <- foreach(s = 1:length(specs_wcvp), .packages = c("dplyr", "sf", "units"),
                               .combine = "rbind", .verbose = TRUE, .options.snow = opts) %dopar% { 
                                 
                                 
                                 # WCVP status data for species s:
                                 wcvp_status_spec <- wcvp_status %>%
                                   filter(species == specs_wcvp[s])
                                 
                                 # occurrences of species s:
                                 occ_sf_spec <-  occ_cleaned_slim %>%
                                   filter(species == specs_wcvp[s]) %>%
                                   st_as_sf(coords = c("lon", "lat"), crs = st_crs(tdwg))
                                 
                                 sf_use_s2(FALSE) # switch off spherical geometry package S2
                                 
                                 # spatially join occurrences and polygons:
                                 occ_wcvp_poly_spec <- occ_sf_spec %>%
                                   select(occ_id, species) %>%
                                   st_join(wcvp_status_spec, st_intersects, left = TRUE) %>% # occurrence must be inside the polygon
                                   st_drop_geometry() %>%
                                   rename("species" = "species.x") %>%
                                   select(-species.y)
                                 
                                 
                                 if (nrow(occ_wcvp_poly_spec) == 0) return(occ_wcvp_poly_spec) # no occurrences for the species
                                 
                                 # additionally match occurrences not inside any WCVP region to the closest WCVP region
                                 # with status information, if it is <= 10 km away:
                                 
                                 # occurrence ID not joined:
                                 occ_ID_no_wcvp_poly <- occ_wcvp_poly_spec %>%
                                   filter(is.na(LEVEL3_NAM)) %>%
                                   pull(occ_id)
                                 
                                 
                                 if (length(occ_ID_no_wcvp_poly) != 0) {
                                   
                                   # occurrences not joined as sf :
                                   occ_sf_spec_no_wcvp_poly <- occ_sf_spec %>%
                                     filter(occ_id %in% occ_ID_no_wcvp_poly)
                                   
                                   # nearest WCVP region for each of these occurrences:
                                   nearest_wcvp_region <- st_nearest_feature(x = occ_sf_spec_no_wcvp_poly, y = wcvp_status_spec, longlat = TRUE)
                                   nearest_wcvp_region_sf <- wcvp_status_spec[nearest_wcvp_region,]  %>%
                                     cbind("occ_id" = occ_sf_spec_no_wcvp_poly$occ_id)
                                   
                                   # distance of each occurrence to nearest GIFT region:
                                   dist_nearest_wcvp_region <- st_distance(occ_sf_spec_no_wcvp_poly,
                                                                           nearest_wcvp_region_sf,
                                                                           by_element = TRUE,
                                                                           tolerance = units::set_units(10000, m))
                                   
                                   # keep matched regions if distance is <= 10km:
                                   occ_wcvp_max10kmdist <- nearest_wcvp_region_sf %>%
                                     mutate(distance_m = dist_nearest_wcvp_region) %>%
                                     filter(distance_m <= units::set_units(10000, m)) %>%
                                     st_drop_geometry() %>%
                                     select(-distance_m)
                                   
                                   if(length(occ_wcvp_max10kmdist) != 0) {
                                     # join to all occurrences:
                                     occ_wcvp_poly_spec_max10kmdist <- occ_wcvp_poly_spec %>%
                                       left_join(occ_wcvp_max10kmdist, by = c("occ_id", "species")) %>%
                                       mutate(LEVEL3_NAM = ifelse(!is.na(LEVEL3_NAM.x), LEVEL3_NAM.x, LEVEL3_NAM.y)) %>%
                                       mutate(LEVEL3_COD = ifelse(!is.na(LEVEL3_COD.x), LEVEL3_COD.x, LEVEL3_COD.y)) %>%
                                       mutate(LEVEL2_COD = ifelse(!is.na(LEVEL2_COD.x), LEVEL2_COD.x, LEVEL2_COD.y)) %>%
                                       mutate(LEVEL1_COD = ifelse(!is.na(LEVEL1_COD.x), LEVEL1_COD.x, LEVEL1_COD.y)) %>%
                                       mutate(occurrence_type = ifelse(!is.na(occurrence_type.x), occurrence_type.x, occurrence_type.y)) %>%
                                       mutate(area = ifelse(!is.na(area.x), area.x, area.y)) %>%
                                       select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD, area)
                                   } else {
                                     occ_wcvp_poly_spec_max10kmdist <- occ_wcvp_poly_spec %>%
                                       select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD, area)
                                   }
                                   
                                   
                                 } else {occ_wcvp_poly_spec_max10kmdist <- occ_wcvp_poly_spec %>%
                                   select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD, area)
                                 }
                                 
                                 # join status information to WCVP regions:
                                 occ_wcvp_status_spec <- occ_wcvp_poly_spec_max10kmdist %>%
                                   left_join(wcvp_status_spec)# end of try
                                 
                                 # since polygons are nested, more than 1 polygon may be joined to a single occurrence (e.g. Paraguay and Southern South America)
                                 # in the following, for each occurrence the status information belonging to the smaller polygon is used:
                                 occ_wcvp_status_spec_final <- occ_wcvp_status_spec %>%
                                   select(-geometry) %>%
                                   group_by(occ_id) %>%
                                   arrange(area, .by_group = TRUE) %>%
                                   slice(1) %>%
                                   ungroup 
                                 
                                 
                               } # end of foreach

stopCluster(cl)

occ_wcvp_status_3 <- rbind(occ_wcvp_status_3, occ_wcvp_status_new)
rm(occ_wcvp_status_new)

save(occ_wcvp_status_3, file = "data/status_assignment/occ_wcvp_status/occ_WCVP_status_3.RData")


# clear entire environment
rm(list = ls())

# merge status chunks -----------------------------------------------------

# load the three WCVP status chunks
load("data/status_assignment/occ_wcvp_status/occ_WCVP_status_1.RData")
load("data/status_assignment/occ_wcvp_status/occ_WCVP_status_2.RData")
load("data/status_assignment/occ_wcvp_status/occ_WCVP_status_3.RData")



# merge chunks into one big df
# only keep necessary columns


occ_wcvp_status <- rbind(occ_wcvp_status_1, occ_wcvp_status_2, occ_wcvp_status_3) %>%
  select(-c(LEVEL2_COD, LEVEL1_COD))


rm(occ_wcvp_status_1, occ_wcvp_status_2, occ_wcvp_status_3) 
save(occ_wcvp_status, file = "data/status_assignment/occ_wcvp_status.RData")



