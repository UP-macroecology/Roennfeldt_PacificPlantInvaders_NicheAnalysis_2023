library(foreach)
library(lcvplants)
library(tidyverse)
library(rWCVP) # required data: remotes::install_github('matildabrown/rWCVPdata')
library(units)

rm(list = ls())


# required data -----------------------------------------------------------

load("data/occ_cleaned_slim.RData")
tdwg <- st_read("data/tdwg/geojson/level3.geojson")

# load in the list of available wcvp names
wcvp_names <- rWCVPdata::wcvp_names

load("data/specs_all.RData")

specs <- specs_all[c(999, 2746)] # example species: "Crotalaria verrucosa" "Phyllostachys nigra"

rm(specs_all)

# 
# # Paciflora names for hybrids are in a format that causes errors when running the wcvp_distribution() function
# hybrids <- species_names %>%
#   filter(grepl('_x', PaciFlora)) %>%
#   pull() 
# 
# # hybrids_corrected <- data.frame(PaciFlora = str_replace(hybrids, "_x", ""))
# hybrids_corrected <- hybrids %>%
#   str_replace("_x", "") %>%
#   str_replace(" ", " x ")
# 
# 
# # remove hybrids with the old format and add the corrected format
# species_names_corrected <- species_names %>%
#   filter(!grepl('_x', PaciFlora)) %>% # remove old format
#   rbind(data.frame(PaciFlora = hybrids_corrected)) %>% # add corected format
#   arrange(PaciFlora) %>% # sort alphabetically
#   pull(PaciFlora)
# 
# save(species_names_corrected, file = "data/initial_species_list.RData")


# WCVP species distribution -----------------------------------------------


# set.seed(7)
# specs <- sample(species_names$PaciFlora, 5)

# specs <- species_names_corrected

wcvp_status <- foreach(s = 1:length(specs), .packages = c("dplyr"), .combine = "rbind", .verbose = TRUE) %do% {
  
  distribution <- wcvp_distribution(taxon = specs[s], taxon_rank = "species",
                                    location_doubtful = FALSE, extinct = FALSE) %>%
    mutate(species = specs[s]) %>%
    relocate(species)
  
  # calculate area and make sure it is in km^2 so that it is comparable to the gift polygons
  
  distribution <- distribution %>%
    mutate(area = drop_units(set_units(sf::st_area(distribution), "km^2"))) 
  
  
  distribution
  
} # end of foreach 

rm(distribution)


# merge with occurrences --------------------------------------------------

specs_wcvp <- unique(wcvp_status$species)

no_cores <- 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)

occ_wcvp_status <- foreach(s = 1:length(specs_wcvp), .packages = c("dplyr", "sf", "units"),
                           .combine = "rbind", .verbose = TRUE) %do% {
                             
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
                               st_drop_geometry() 
                             
                             # two species columns are kept(haven't figured out why)
                             # only keep and rename species.x if they are identical
                             
                             if (length(setdiff(occ_wcvp_poly_spec$species.x, occ_wcvp_poly_spec$species.y)) == 0) {
                               
                               occ_wcvp_poly_spec <- occ_wcvp_poly_spec %>%
                                 rename("species" = "species.x") %>%
                                 select(-species.y)
                             }
                             
                             
                             if(nrow(occ_wcvp_poly_spec) == 0) return(occ_wcvp_poly_spec) # no occurrences for the species
                             
                             # additionally match occurrences not inside any WCVP region to the closest WCVP region
                             # with status information, if it is <= 10 km away:
                             
                             # occurrence ID not joined:
                             occ_ID_no_wcvp_poly <- occ_wcvp_poly_spec %>%
                               filter(is.na(LEVEL3_NAM)) %>%
                               pull(occ_id)
                             
                             
                             if(length(occ_ID_no_wcvp_poly) != 0) {
                               
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
                               
                             } else {occ_GIFT_poly_spec_max10kmdist <- occ_GIFT_poly_spec %>%
                               select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD, area)
                             }
                             
                             # join status information to GIFT regions:
                             occ_wcvp_status_spec <- occ_wcvp_poly_spec_max10kmdist %>%
                               left_join(wcvp_status_spec)
                             
                             # since polygons are nested, more than 1 polygon may be joined to a single occurrence (e.g. Paraguay and Southern South America)
                             # in the following, for each occurrence the status information belonging to the smaller polygon is used:
                             occ_wcvp_status_spec_final <- occ_wcvp_status_spec %>%
                               group_by(occ_id) %>%
                               arrange(area, .by_group = TRUE) %>%
                               slice(1) %>%
                               ungroup 
                             
                           } # end of foreach

save(occ_wcvp_status, file = "data/testing/occ_WCVP_status.RData")

stopCluster(cl)
# -------------------------------------------------------------------------


n_occur[n_occur$Freq > 1,]

table(occ_wcvp_status_spec_final$occ_id)

occ_wcvp_status_spec_final[occ_wcvp_status_spec_final$occ_id > 1,]
# 
# # codes for level 3 regions in which species occurs
# reg_cod_specs <- unique(wcvp_status_spec$LEVEL3_COD)
# 
# # level 3 polygons in which species occurs:
# polygon_IDs_spec <- GIFT_status_spec$entity_ID

# # GIFT polygons as sf:
# wcvp_polygons_spec <- wcvp_status_spec %>%
#   filter(entity_ID %in% polygon_IDs_spec)