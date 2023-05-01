library(dplyr)
library(GIFT)

rm(list = ls())

source("scripts/functions.R")


# required data ----------------------------------------------------------------
load("data/specs_all.RData")

specs_all <- specs_all[1:50]


# get GIFT status information ---------------------------------------------

# 1) find corresponding species names in GIFT

GIFT_names <- bind_rows(lapply(specs_all, getGiftNames, incl_lcvp_synonyms = TRUE))
#save(GIFT_names, file = file.path("data", "GIFT_names.RData"))


# 2) extract status information from GIFT:
# (getGiftStatusInf defined in utils_1-5_dataprep.R)
GIFT_status <- vector("list", length = nrow(GIFT_names))
for (s in 1:nrow(GIFT_names)) {
  print(s)
  GIFT_status[[s]] <- getGiftStatusInf(searched_name = GIFT_names$searched_name[s],
                                       GIFT_spec_genus = GIFT_names$GIFT_genus[s],
                                       GIFT_spec_epithet = GIFT_names$GIFT_species_ep[s])
}
GIFT_status_all <- bind_rows(GIFT_status)
#save(GIFT_status_all, file = file.path("data","status_information", "GIFT_status_all.RData"))
rm(GIFT_status)


#rename the statuses based on the distinct combinations
# based on: https://biogeomacro.github.io/GIFT/articles/GIFT_tutorial.html#species-distribution

GIFT_status <- GIFT_status_all %>%
  mutate(status_GIFT = case_when(
    native == "native" & naturalized == "non-naturalized" ~ "native",
    native == "native" & is.na(naturalized) ~ "native",
    native == "non-native" & is.na(naturalized) ~ "non-native",
    native == "non-native" & naturalized == "naturalized" ~ "naturalized",
    native == "non-native" & naturalized == "non-naturalized" ~ "non-native",
    is.na(native) & is.na(naturalized) ~ "unknown"
  )) %>%
  select(species, GIFT_species, entity_ID, status_GIFT)

save(GIFT_status, file = "data/status_information/GIFT_status.RData")



# 3) load spatial data of the GIFT regions with status information for the considered species:
GIFT_polygons <- GIFT_shape(unique(GIFT_status_df$entity_ID), GIFT_version = "beta") 
#save(GIFT_polygons, file = file.path("data", "Gift_polygons.RData"))


# assign GIFT status to occurrences ---------------------------------------

# register cores for parallel computation to speed it up:
registerDoParallel(cores = 1) # doParallel package
getDoParWorkers() # check registered number of cores

occ_GIFT_status <- foreach(s = 1:length(specs_all), .packages = c("dplyr", "sf"),
                           .combine = "rbind", .verbose = TRUE) %dopar% {
                             
                             # GIFT status data for species s:
                             GIFT_status_spec <- GIFT_status %>% 
                               filter(searched_name == specs_all[s])
                             
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
                               left_join(GIFT_status_spec) %>% 
                               select(-c(species, GIFT_species))
                             
                             # since polygons are nested, more than 1 polygon may be joined to a single occurrence (e.g. Paraguay and Southern South America)
                             # in the following I use for each occurrence the status information belonging to the smaller polygon:
                             occ_GIFT_status_spec_final <- occ_GIFT_status_spec %>% 
                               group_by(occ_id) %>% 
                               arrange(area, .by_group = TRUE) %>%
                               slice(1) %>%
                               ungroup
                           }
