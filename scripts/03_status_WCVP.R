library(conflicted)
library(foreach)
library(doParallel)
library(lcvplants)
library(tidyverse)
library(rWCVP) # required data: remotes::install_github('matildabrown/rWCVPdata')
library(units)
library(sf)

rm(list = ls())

filter <- dplyr::filter

# required data -----------------------------------------------------------

# list of available wcvp names
wcvp_names <- rWCVPdata::wcvp_names
# initial species list based on PaciFLora
load("data/initial_species_list.RData")


# WCVP species distribution -----------------------------------------------

wcvp_matches <- wcvp_match_names(species_names$species_orig, name_col = "species_changed", progress_bar = TRUE) %>%
  filter(!(multiple_matches == TRUE & wcvp_status != "Accepted"))

wcvp_matches_refined <- wcvp_matches %>%
  filter(wcvp_status == "Accepted") %>%
  filter(match_edit_distance < 3) %>% # fuzzy search output should be less than three characters away from the initil input
  distinct(wcvp_name) # duplicates are a result from different authors name, keep only unique names

wcvp_matches_synonyms <- wcvp_matches %>%
  filter(wcvp_status == "Synonym")

# save synonyms for later
save(wcvp_matches_synonyms, file = "data/status_assignment/wcvp_synonyms.RData")

# check whether filtering arguments where enough (should return FALSE)
# it could be that fewer names are returned from the wcvp search, because not all species work for this funtion
nrow(wcvp_matches_refined) + nrow(wcvp_matches_synonyms) > nrow(species_names)

wcvp_specs <- wcvp_matches_refined$wcvp_name


save(wcvp_specs, file = "data/status_assignment/wcvp_specs.RData")


wcvp_status <- foreach(s = 1:length(wcvp_specs), .packages = c("dplyr"), .combine = "rbind", .verbose = TRUE) %do% {

  distribution <- wcvp_distribution(taxon = wcvp_specs[s], taxon_rank = "species",
                                    location_doubtful = FALSE, extinct = FALSE) %>%
    mutate(species = wcvp_specs[s]) %>%
    relocate(species)

  # calculate area and make sure it is in km^2 so that it is comparable to the gift polygons

  distribution <- distribution %>%
    mutate(area = drop_units(set_units(sf::st_area(distribution), "km^2")))


  distribution

} # end of foreach

save(wcvp_status, file = "data/status_assignment/wcvp_distribution.RData")

rm(distribution)
rm(wcvp_specs)
rm(specs)
rm(wcvp_matches_refined)
rm(wcvp_matches_synonyms)
rm(wcvp_matches)

# merge with occurrences --------------------------------------------------

# preamble
rm(list = ls())

filter <- dplyr::filter

# required packages ------------------------------------------------------------
# set up to run on HPC

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org', dep = TRUE)
  require(x, character.only = TRUE)
}
package_vec <- c(
  "sf", "units", "tidyverse", "foreach", "doParallel" # names of the packages required placed here as character objects
)

# install.packages(c("pillar", "phangorn")) # install packages that caused namespace errors before running the previously defined function
sapply(package_vec, install.load.package)



# required paths ------------------------------------------------------------------------
path_import <- file.path("/import","ecoc9z", "data-zurell", "roennfeldt", "C1")


# required data -----------------------------------------------------------
load(paste0(path_import, "/output/occ_cleaned_slim.RData"))
load(paste0(path_import, "/input/wcvp_distribution.RData"))
tdwg <- st_read(paste0(path_import,"/input/level3.geojson"))


# load("data/occ_cleaned_slim.RData")
# load("data/status_assignment/wcvp_distribution.RData") # object is called wcvp_status
# tdwg <- st_read("data/tdwg/geojson/level3.geojson")

specs_initial <- unique(wcvp_status$species)


specs_wcvp <- specs_initial


no_cores <- 14
cl <- makeCluster(no_cores)
registerDoParallel(cl)

occ_wcvp_status <- foreach(s = 1:length(specs_wcvp), .packages = c("dplyr", "sf", "units"),
                           .combine = "rbind", .verbose = TRUE) %dopar% { 
                             
                             
                             print(s)
                             
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
                               
                             } else {occ_wcvp_poly_spec_max10kmdist <- occ_wcvp_poly_spec %>%
                               select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD, area)
                             }
                             
                             try(
                               
                             # join status information to WCVP regions:
                             occ_wcvp_status_spec <- occ_wcvp_poly_spec_max10kmdist %>%
                               left_join(wcvp_status_spec)) # end of try
                             
                             # since polygons are nested, more than 1 polygon may be joined to a single occurrence (e.g. Paraguay and Southern South America)
                             # in the following, for each occurrence the status information belonging to the smaller polygon is used:
                             occ_wcvp_status_spec_final <- occ_wcvp_status_spec %>%
                               select(-geometry) %>%
                               group_by(occ_id) %>%
                               arrange(area, .by_group = TRUE) %>%
                               slice(1) %>%
                               ungroup 
                             

                             } # end of foreach


occ_wcvp_status <- occ_wcvp_status %>%
  select(-c(LEVEL2_COD, LEVEL1_COD))

save(occ_wcvp_status, file = paste0(path_import, "output/occ_WCVP_status.RData"))

stopCluster(cl)
