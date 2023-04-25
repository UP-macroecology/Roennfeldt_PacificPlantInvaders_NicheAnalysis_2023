# required packages 

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "tidyverse", "sf", "doParallel" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)

# required path
path_imp <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1")

# # load required data 
load(file.path(path_imp, "input_data", "specs_all.RData"))
load(file.path(path_imp, "input_data", "Gift_polygons_nested_regions.RData"))
load(file.path(path_imp, "input_data", "GIFT_status_df_nested_regions.RData"))
load(file.path(path_imp, "input_data", "occ_cleaned_slim.RData"))
load(file.path(path_imp, "output", "all_specs_occ_status.RData"))

# find the species for which GIFT information was found
specs_GIFT <- intersect(specs_all,unique(GIFT_status_df$blacklist_name))


# for each species, merge occurrences, GIFT regions and corresponding GIFT status:
# (for each species separately because not only the location of the occurrence determines the matching
# GIFT region, but also the species since GIFT regions are nested and the level on which status information is
# available differs between species (e.g. occurrence is on Honshu, for one species there is
# status information for Honshu available, thus occurrence should be matched to Honshu, for another species
# information is only available for Japan in general, then the occurrence should be matched to Japan))

# register cores for parallel computation to speed it up:
registerDoParallel(cores = 6) # doParallel package
getDoParWorkers() # check registered number of cores


occ_GIFT_status <- foreach(s = 1:length(specs_GIFT), .packages = c("dplyr", "sf"),
                           .combine = "rbind", .verbose = TRUE) %dopar% {

                             # subset occurrences for s
                             occ_sf <-  occ_cleaned_slim %>%
                               filter(species == specs_GIFT[s]) %>%
                               st_as_sf(coords = c("lon", "lat"), crs = st_crs(tdwg))


                             # GIFT status data for species s:
                             GIFT_status_spec <- GIFT_status_df %>%
                               filter(blacklist_name == specs_GIFT[s])

                             # GIFT polygon IDs:
                             polygon_IDs_spec <- GIFT_status_spec$entity_ID

                             # GIFT polygons as sf:
                             GIFT_polygons_spec <- GIFT_polygons %>%
                               filter(entity_ID %in% polygon_IDs_spec)

                             # occurrences of species s:
                             occ_sf_spec <- occ_sf %>%
                               filter(species == specs_GIFT[s])

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
                               select(-c(blacklist_name, GIFT_species))

                             # since polygons are nested, more than 1 polygon may be joined to a single occurrence (e.g. Paraguay and Southern South America)
                             # in the following I use for each occurrence the status information belonging to the smaller polygon:
                             occ_GIFT_status_spec_final <- occ_GIFT_status_spec %>%
                               group_by(occ_id) %>%
                               arrange(area, .by_group = TRUE) %>%
                               slice(1) %>%
                               ungroup
                           }




# recode GIFT status information:
occ_GIFT_status <- occ_GIFT_status %>%
  mutate(GIFT_native = ifelse(native == "native", 1, 0)) %>%
  mutate(GIFT_naturalized = ifelse(naturalized == "naturalized", 1, 0)) %>%
  mutate(GIFT_endemic = ifelse(endemic_list == "endemic_list", 1, 0)) %>%
  select(-c(native, naturalized, endemic_list))

save(occ_GIFT_status, file = paste0(path_imp, "/output/occ_GIFT_status_max_10km_dist.RData"))



# combine GIFT status information with Powo information 
# load(file.path(path_imp, "output", "occ_GIFT_status_max_10km_dist.RData"))


all_specs_occ_status_POWO_GIFT <- left_join(all_specs_occ_status,
                                            occ_GIFT_status,
                                            by = "occ_id") %>%
  rename(c(species = species.x, GIFT_polygon_ID = entity_ID, GIFT_polygon = geo_entity,
           GIFT_polygon_area = area, GIFT_polygon_source = polygon_source)) %>%
  select(-species.y)

save(all_specs_occ_status_POWO_GIFT, file = paste0(path_imp, "/output/all_specs_occ_status_POWO_GIFT.RData"))

