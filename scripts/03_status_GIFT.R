
# required packages -------------------------------------------------------

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "sf", "GIFT", "stringr", "doParallel", "foreach" # names of the packages required placed here as character objects
)
sapply(package_vec, install.load.package)

rm(list = ls())

path_imp <- file.path("/import","ecoc9z", "data-zurell", "roennfeldt", "C1")



# required functions ------------------------------------------------------

getGiftNames <- function(spec, incl_lcvp_synonyms = FALSE){
  
  # searches for species name in GIFT dataset, checks species name against LCVP (on which blacklist is based),
  # from GIFT extracts WCVP harmonized species name (same taxonomic backbone as in POWO) if species is also included in LCVP
  
  # input:  
  #   - spec: LCVP based species name
  #   - incl_lcvp_synonyms: TRUE = LCVP search results include names that are considered synonyms in LCVP;
  #                         FALSE = only accepted names in LCVP are considered
  # output: 
  #   - df: searched name - GIFT result genus - GIFT result species epithet
  
  print(spec)
  
  # split name in genus and species epithet:
  spec_gen_epi <- unlist(str_split(spec, pattern = " ", n = 2))
  
  # find searched species name in GIFT:
  GIFT_spec_res <- tryCatch({
    
    GIFT_species_lookup(genus = spec_gen_epi[1], 
                        epithet = spec_gen_epi[2], 
                        namesmatched = TRUE, # TRUE = look for the species not only in the standardized names but also in the original names
                        GIFT_version = "beta")
  },
  error = function(e){
    print(paste("Connection to Gift information failed, try again later."))
    return(data.frame("searched_name" = spec,
                      "GIFT_genus" = NA,
                      "GIFT_species_ep" = NA))})
  
  # GIFT results: species names incl. author before harmonization:
  GIFT_spec_org <- paste(GIFT_spec_res$genus, GIFT_spec_res$species_epithet, GIFT_spec_res$author)
  
  # check against LCVP:
  
  # search LCVP entries connected to the searched species name:
  if(incl_lcvp_synonyms){
    status_ok <- c("accepted", "synonym") # exclude unresolved and external results
  } else {status_ok <- "accepted"} # only accepted species names
  lcvp_spec_res <- unique(lcvp_fuzzy_search(spec, status = status_ok)$Output.Taxon) # lcvp_fuzzy_search from package lcvpplants
  
  # GIFT WCVP harmonized names (column work_species) matching LCVP results:
  GIFT_res_harm <- GIFT_spec_res$work_species[which(GIFT_spec_org %in% lcvp_spec_res)]
  
  # there is no exact match (e.g. due to slightly different names; 6 species of the 122 blacklist species)
  if(length(GIFT_res_harm) == 0){
    
    # find most similar name with fuzzy matching:
    print("No exact match between Gift and LCVP name found. Used fuzzy matching instead. Consider checking the results manually.")
    best_match <- DIFFLIB$get_close_matches(word = lcvp_spec_res, 
                                            possibilities = GIFT_spec_org,
                                            n = as.integer(1), cutoff = 0)
    print(paste("LCVP name:", lcvp_spec_res))
    print(paste("Matched Gift name:", best_match))
    GIFT_res_harm <- GIFT_spec_res$work_species[which(GIFT_spec_org == best_match)]
  }
  
  # split in genus and species epithet:
  GIFT_harm_gen_epi <- unlist(str_split(GIFT_res_harm, pattern = " ", n = 2))
  
  if(length(GIFT_harm_gen_epi) != 0){
    return(data.frame("searched_name" = spec,
                      "GIFT_genus" = GIFT_harm_gen_epi[1],
                      "GIFT_species_ep" = GIFT_harm_gen_epi[2]))
  }else{
    print("Matching GIFT and LCVP name didn't work. Check manually.")
    return(data.frame("searched_name" = spec,
                      "GIFT_genus" = NA,
                      "GIFT_species_ep" = NA))
  }
}


getGiftStatusInf <- function(searched_name, GIFT_spec_genus, GIFT_spec_epithet){
  
  # extracts status information from GIFT
  # input:  
  #   - searched_name: original species name from blacklist, used in output to later join occurrences
  #   - GIFT_spec_genus: genus of GIFT species name
  #   - GIFT_spec_epithet: species epithet of GIFT species name
  # output: 
  #   - df: searched_species, GIFT species - entity_ID (= ID of GIFT polygon) - native - naturalized - endemic_list
  
  # attention: GIFT provides status information for nested regions, e.g. status information for Honshu, but also for Japan in general,
  # whether nested regions should all be retained or how information should be dissolved can be defined within
  # GIFT_species_distribution()
  # for us it makes sense to keep all nested regions in the first place and remove nested regions after merging with occurrences
  # (occurrences may be located at different nesting levels, e.g. not on Honshu but in another location in Japan, if we had dropped Japan in the first place,
  # no status information would be assigned)
  
  print(paste(GIFT_spec_genus, GIFT_spec_epithet))
  
  # find GIFT distribution for harmonized species name: 
  GIFT_spec_distr <- tryCatch({
    
    GIFT_species_distribution(genus = GIFT_spec_genus,
                              epithet = GIFT_spec_epithet, 
                              aggregation = TRUE, # TRUE = only one status per polygon
                              namesmatched = FALSE, # TRUE not necessary since harmonized species name is used
                              #remove_overlap = TRUE, # return only one of overlapping polygons, depending on the rules defined below:
                              #overlap_th = 0.1, # default, if polygons overlap by min. 10 % they are treated as overlapping, if they overlap less, both polygons are kept
                              #area_th_mainland = 0, # overlapping mainlands: use smallest mainland (e.g. use Tanzania rather than East Tropical Africa)
                              #area_th_island = 0, # use smallest island (rather than Island Group, e.g. use Honshu rather than Japan)
                              GIFT_version = "beta") # doesn't allow including author in search
  }, error = function(e){
    print(paste("Connection to Gift information failed, try again later."))
    spec_status_inf <- data.frame(species = searched_name,
                                  GIFT_species = paste(GIFT_spec_genus, GIFT_spec_epithet),
                                  entity_ID = NA,
                                  native = NA,
                                  naturalized = NA,
                                  endemic_list = NA)
    return(spec_status_inf)
  })
  
  # extract and re-format information:
  spec_status_inf <- GIFT_spec_distr %>%
    mutate(species = searched_name) %>%
    mutate(GIFT_species = paste(GIFT_spec_genus, GIFT_spec_epithet)) %>%
    mutate(native = ifelse(native == 1, "native", "non-native"),
           naturalized = ifelse(naturalized == 1, "naturalized",
                                "non-naturalized"),
           endemic_list = ifelse(endemic_list == 1, "endemic_list",
                                 "non-endemic_list")) %>%
    select(species, GIFT_species, entity_ID, native, naturalized, endemic_list) %>%
    filter_at(vars(native, naturalized, endemic_list), any_vars(!is.na(.)))  # remove entries without any status information
  
  return(spec_status_inf)
  
}



# required data  -----------------------------------------------------------

load(paste0(path_imp, "input/initial_species_list.RData"))
specs <- species_names$species_changed

rm(species_names)

# GIFT names --------------------------------------------------------------
# compare the species from the Paciflora species list (based on LCVP) with the names used for GIFT

# prepare empty df to store info
GIFT_names <- data.frame(searched_name = character(),
                         GIFT_genus = character(),
                         GIFT_species_ep = character(),
                         stringsAsFactors = FALSE)

# load("data/status_assignment/GIFT_names.RData")

# run loop over species
for(spec in specs_left){
  
  GIFT_names <- bind_rows(GIFT_names,
                          getGiftNames(spec, incl_lcvp_synonyms = TRUE))
  
} # end of for loop over specs_left

# sometimes, faulty internet connections can cause NA assignments

# identify species with NA and re-run the above code for these
specs_NA <- specs[which(is.na(GIFT_names$GIFT_genus))]

# prepare empty df to store info

GIFT_names_NA <- data.frame(searched_name = character(),
                            GIFT_genus = character(),
                            GIFT_species_ep = character(),
                            stringsAsFactors = FALSE)

# run loop over species
for(spec in specs_NA){

  GIFT_names_NA <- bind_rows(GIFT_names_NA,
                             getGiftNames(spec, incl_lcvp_synonyms = TRUE))

} # end of for loop over specs_NA

# bind info together
GIFT_names <- rbind(GIFT_names, GIFT_names_NA) %>%
  na.omit() # removes duplicates and remaining NAs

save(GIFT_names, file = paste0(path_imp, "output/GIFT_names.RData"))

# GIFT status -------------------------------------------------------------


GIFT_status <- vector("list", length = nrow(GIFT_names))

for (s in 1:nrow(GIFT_names)) {
  print(s)
  GIFT_status[[s]] <- getGiftStatusInf(searched_name = GIFT_names$searched_name[s],
                                       GIFT_spec_genus = GIFT_names$GIFT_genus[s],
                                       GIFT_spec_epithet = GIFT_names$GIFT_species_ep[s])
}

# sometimes, no status information is returned, these empty list elements cause errors and have to be removed
list_index <- NULL
for(s in 1:length(GIFT_status)){
  
  # collect indices of list element without GIFT information (empty df)
  if(nrow(GIFT_status[[s]]) == 0){list_index <- c(list_index, s)}
  
} # end of loop over list indices

# remove empty elements from list
GIFT_status <- GIFT_status[-list_index]

GIFT_status_all_details <- bind_rows(GIFT_status)

#rename the statuses based on the distinct combinations
# based on: https://biogeomacro.github.io/GIFT/articles/GIFT_tutorial.html#species-distribution

GIFT_status <- GIFT_status_all_details %>%
  mutate(status = case_when(
    native == "native" & naturalized == "non-naturalized" ~ "native",
    native == "native" & is.na(naturalized) ~ "native",
    native == "non-native" & is.na(naturalized) ~ "non-native",
    native == "non-native" & naturalized == "naturalized" ~ "naturalized",
    native == "non-native" & naturalized == "non-naturalized" ~ "non-native",
    is.na(native) & is.na(naturalized) ~ "unknown"
  )) %>%
  select(species, GIFT_species, entity_ID, status)

save(GIFT_status, file = paste0(path_imp, "output/GIFT_status.RData"))


# 3) load spatial data of the GIFT regions with status information for the considered species:
GIFT_polygons <- GIFT_shape(unique(GIFT_status$entity_ID), GIFT_version = "beta")
save(GIFT_polygons, file = paste0(path_imp,"output/Gift_polygons.RData"))

# status assignment -------------------------------------------------------

# additionally required data
load(paste0(path_imp, "output/occ_cleaned_slim.RData"))
tdwg <- st_read(paste0(path_import, "input/level3.geojson"))



# get number of species for which GIFT status was available
specs_gift <- unique(GIFT_status$species)

no_cores <- 3
cl <- makeCluster(no_cores)
registerDoParallel(cl)


occ_GIFT_status <- foreach(s = 1:length(specs_gift), .packages = c("dplyr", "sf"),
                           .combine = "rbind", .verbose = TRUE) %dopar% {
                             
                             
                             # GIFT status data for species s:
                             GIFT_status_spec <- GIFT_status %>%
                               filter(species == specs_gift[s])
                             
                             # GIFT polygon IDs:
                             polygon_IDs_spec <- GIFT_status_spec$entity_ID
                             
                             # GIFT polygons as sf:
                             GIFT_polygons_spec <- GIFT_polygons %>%
                               filter(entity_ID %in% polygon_IDs_spec)
                             
                             # occurrences of species s:
                             # subset occurrences for s
                             occ_sf_spec <-  occ_cleaned_slim %>%
                               filter(species == specs_gift[s]) %>%
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
                             # in the following, for each occurrence the status information belonging to the smaller polygon is used:
                             occ_GIFT_status_spec_final <- occ_GIFT_status_spec %>%
                               group_by(occ_id) %>%
                               arrange(area, .by_group = TRUE) %>%
                               slice(1) %>%
                               ungroup
                           }


save(occ_GIFT_status, file = paste0(path_imp, "output/occ_GIFT_status.RData"))

stopCluster(cl)
