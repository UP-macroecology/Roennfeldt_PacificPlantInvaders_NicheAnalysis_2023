
# paths
path_imp <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1")

## Packages ---------------------------------------------------------------
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "sf", "doParallel", "foreach" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)

# load required data
load(file.path(path_imp, "input_data", "species_names.RData"))
load(file.path(path_imp, "input_data", "powo_page_inf_final.RData"))
load(file.path(path_imp, "input_data", "glonaf_blacklist_dt.RData"))
load(file.path(path_imp, "output", "all_specs_occ_status_POWO_GIFT.RData"))
tdwg <- st_read(file.path(path_imp, "input_data", "tdwg_lvl3.geojson"))

# ---------------------------------------------------- #
#   Assign GloNAF status information to occurrences ####
# ---------------------------------------------------- #

registerDoParallel(cores = 4) # doParallel package
getDoParWorkers()

all_specs_occ_status_POWO_GIFT_GloNAF <- foreach(s = 1:length(specs_all), .packages = c("dplyr"),
                                                 .combine = "rbind", .verbose = TRUE) %dopar% {

                                                   # occurrence data of specified species:
                                                   occs_subset <- all_specs_occ_status_POWO_GIFT %>%
                                                     filter(species == specs_all[s])

                                                   if(nrow(occs_subset) == 0){
                                                     return(NULL)
                                                   }

                                                   # TDWG regions where species occurs:
                                                   tdwg_status <- occs_subset %>%
                                                     filter(!is.na(tdwg_l3_code)) %>%
                                                     distinct(tdwg_l3_code) %>%
                                                     pull

                                                   # check whether GloNAF contains status information (alien or naturalized) for these regions:

                                                   # LCVP and POWO names by which GloNAF data should be extracted:
                                                   lcvp_powo_name <- powo_page_inf_final %>%
                                                     filter(searched_name == specs_all[s]) %>%
                                                     distinct(lcvp_name, powo_name)
                                                   status_powo_name <- as.character(unique(lcvp_powo_name$powo_name[!is.na(lcvp_powo_name$powo_name)]))
                                                   status_lcvp_name <- as.character(unique(lcvp_powo_name$lcvp_name[!is.na(lcvp_powo_name$lcvp_name)]))

                                                   # extract relevant GloNAF data:
                                                   glonaf_dt_rel <- glonaf_blacklist_dt %>%
                                                     filter(lcvp_name %in% status_lcvp_name | powo_name %in% status_powo_name) %>%
                                                     filter(tdwg3 %in% tdwg_status) %>%
                                                     select(tdwg3, status) %>%
                                                     distinct

                                                   if(nrow(glonaf_dt_rel) == 0){

                                                     occs_subset_updt <- occs_subset %>%
                                                       mutate(status_GloNAF = NA)

                                                   }else{

                                                     # add status information from GloNAF:
                                                     occs_subset_updt <- occs_subset %>%
                                                       left_join(glonaf_dt_rel, by = c(tdwg_l3_code = "tdwg3")) %>%
                                                       rename(status_GloNAF = status) %>%
                                                       group_by(occ_id) %>%
                                                       arrange(desc(status_GloNAF), .by_group = TRUE) %>% # use naturalized rather than alien, if there are both information for the species in the region (happens if 2 species names match the species name in the blacklist)
                                                       slice(1) %>%
                                                       ungroup
                                                   }
                                                   occs_subset_updt
                                                 }

rm(all_specs_occ_status_POWO_GIFT)

save(all_specs_occ_status_POWO_GIFT_GloNAF, file = paste0(path_imp, "/output/all_specs_occ_status_POWO_GIFT_GloNAF.RData"))

# ----------------------------------------------------------#
#   Assign overall status based on POWO, GIFT and GloNAF ####
# ----------------------------------------------------------#

# treated GIFT’s “naturalized”, GloNAF’s “naturalized” and left “aliens” and POWO’s “introduced” as “introduced”

# (native1 = native, no disagreement between data sets,
# native2 = native in 2 of the 3 data sets, disagreement with third data set
# introduced1 = introduced, no disagreement between data sets,
# introduced2 = introduced in 2 of the 3 data sets, disagreement with third data set
# unknown = no status information in any of the data sets
# confl = conflicting information)

all_specs_occ_final <- all_specs_occ_status_POWO_GIFT_GloNAF %>%
  mutate(final_status = case_when(

    # native, no disagreements:
    status_POWO == "native" & (GIFT_native == 1 | is.na(GIFT_native)) & (GIFT_naturalized == 0 | is.na(GIFT_naturalized)) & is.na(status_GloNAF) ~ "native1",
    status_POWO == "unknown" & GIFT_native == 1 & (GIFT_naturalized == 0 | is.na(GIFT_naturalized)) & is.na(status_GloNAF) ~ "native1",

    # native, majority vote:
    status_POWO == "native" & GIFT_native == 1 & (GIFT_naturalized == 0 | is.na(GIFT_naturalized)) & !is.na(status_GloNAF) ~ "native2",
    status_POWO == "native" & GIFT_native == 1 & GIFT_naturalized == 1 & is.na(status_GloNAF) ~ "native2",

    # introduced, no disagreements:
    status_POWO == "introduced" & (GIFT_native == 0 | is.na(GIFT_native)) & (GIFT_naturalized == 1 | is.na(GIFT_naturalized)) ~ "introduced1",
    status_POWO == "unknown" & (GIFT_native == 0 | is.na(GIFT_native)) & (GIFT_naturalized == 1 | is.na(GIFT_naturalized)) & !is.na(status_GloNAF) ~ "introduced1",
    status_POWO == "unknown" & (GIFT_native == 0 | is.na(GIFT_native)) & GIFT_naturalized == 1 ~ "introduced1",
    status_POWO == "unknown" & GIFT_native == 0 & GIFT_naturalized == 0 ~ "introduced1",
    status_POWO == "unknown" & GIFT_native == 0 & is.na(GIFT_naturalized) ~ "introduced1",

    # introduced, majority vote:
    status_POWO == "introduced" & (GIFT_native == 0 | is.na(GIFT_native)) & GIFT_naturalized == 0 & !is.na(status_GloNAF) ~ "introduced2",
    status_POWO == "introduced" & (GIFT_native == 0 | is.na(GIFT_native)) & GIFT_naturalized == 0 & is.na(status_GloNAF) ~ "introduced2",
    status_POWO == "native" & (GIFT_native == 0 | is.na(GIFT_native)) & (is.na(GIFT_naturalized) | GIFT_naturalized == 1) & !is.na(status_GloNAF) ~ "introduced2",
    status_POWO == "introduced" & GIFT_native == 1 & GIFT_naturalized == 1 ~ "introduced2",
    status_POWO == "introduced" & GIFT_native == 1 & (GIFT_naturalized == 0 | is.na(GIFT_naturalized)) & !is.na(status_GloNAF) ~ "introduced2",
    status_POWO == "unknown" & GIFT_native == 1 & GIFT_naturalized == 1 & !is.na(status_GloNAF) ~ "introduced2",

    # conflicting information:
    status_POWO == "introduced" & GIFT_native == 1 & (is.na(GIFT_naturalized) | GIFT_naturalized == 0) & is.na(status_GloNAF) ~ "confl",
    status_POWO == "introduced" & GIFT_native == 1 & GIFT_naturalized == 0 & !is.na(status_GloNAF) ~ "confl",
    status_POWO == "native" & GIFT_native == 0 & is.na(status_GloNAF) ~ "confl",
    status_POWO == "native" & is.na(GIFT_native) & is.na(GIFT_naturalized) & !is.na(status_GloNAF) ~ "confl",
    status_POWO == "native" & GIFT_native == 1 & GIFT_naturalized == 1 & !is.na(status_GloNAF) ~ "confl",
    status_POWO == "native" & GIFT_native == 0 & GIFT_naturalized == 0 & !is.na(status_GloNAF) ~ "confl",
    status_POWO == "unknown" & GIFT_native == 1 & (GIFT_naturalized == 0 | is.na(GIFT_naturalized)) & !is.na(status_GloNAF) ~ "confl",
    status_POWO == "unknown" & GIFT_native == 1 & (GIFT_naturalized == 1 | is.na(GIFT_naturalized)) & is.na(status_GloNAF) ~ "confl",

    # no status information:
    status_POWO == "unknown" & is.na(GIFT_native) & is.na(GIFT_naturalized) & is.na(status_GloNAF) ~ "unknown"
  ))

save(all_specs_occ_final, file = paste0(path_imp, "/output/all_specs_occ_final.RData"))


