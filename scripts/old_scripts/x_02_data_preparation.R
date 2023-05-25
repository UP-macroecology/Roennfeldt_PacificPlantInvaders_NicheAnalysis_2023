library(CoordinateCleaner)
library(countrycode) # to use the function "countrycode"
library(tidyverse)



# preamble ----
rm(list = ls())

# define required paths
# path_user <- "//ibb-fs01.ibb.uni-potsdam.de/users$/roennfeldt/C1/data/"
# path_ds <- "Z:/AG26/Arbeit/datashare/data/biodat/distribution/Pacific_invaders/" # from home
path_ds <- "Z:/Arbeit/datashare/data/biodat/distribution/Pacific_invaders/" # work laptop


# load and prep data -----------------------------------------------------------

# BIEN data
files <- list.files(paste0(path_ds, "download_bien_2023/"), ignore.case = FALSE, full.names = TRUE)
occ_bien <- map_dfr(files, function(file){load(file); return(occ_df)}) # 5,318,461 records (instead of 5'824'132 records)
length(unique(occ_bien$scrubbed_species_binomial)) #  2541 species (instead of 2540 species)
spp_freq_bien <- occ_bien %>% 
  group_by(scrubbed_species_binomial) %>% 
  tally() %>%
  arrange(desc(n))# number of records per species

# GBIF data
files <- list.files(paste0(path_ds, "download_gbif_2023/"), ignore.case = FALSE, full.names = TRUE)
occ_gbif <- map_dfr(files, function(file){load(file); return(occ_df)}) # 30,032,210 occurrences (instead of 25,886,049 occurrences)
length(unique(occ_gbif$species))# 2624 unique species (instead of 2635) 
spp_freq_gbif <- occ_gbif %>%
  group_by(species) %>%
  tally() %>%
  arrange(desc(n))


# free up memory
rm(files)
rm(spp_freq_bien) # to free up memory
rm(spp_freq_gbif) # to free up memory

# harmonise columns
occ_bien_std <- occ_bien %>% 
  select(species = "scrubbed_species_binomial",
         lat = "latitude",
         lon = "longitude",
         country = "country",
         year = "date_collected",
         datasource = "datasource",
         dataset = "dataset",
         native = "native_status",
         cult_obs = "is_cultivated_observation",
         cult_reg = "is_cultivated_in_region",
         cult_loc = "is_location_cultivated") %>%
  # "Kosovo" and "Micronesia" can not be matched to an ISO3c country code by countrycode()
  # (ISO3c country codes were also used by TDWG to define political countries; used in 4_status_assignment.R)
  # "Micronesia" is in codelist as "Micronesia (Federated States of)" -> adapt this
  # Kosovo has no ISO3c code -> gets country code NA -> think about fixing this
  mutate(country = replace(country, country == "Micronesia", "Micronesia (Federated States of)")) %>% # not in previous ChrK-version!
  mutate(year = lubridate::year(year), 
         country = countrycode(country, origin = "country.name", destination = "iso3c"))

occ_gbif_std = occ_gbif %>% 
  select(species = "species",
         lat = "decimalLatitude",
         lon = "decimalLongitude",
         country = "country",
         year = "year",
         datasource = "institutionCode",
         dataset = "datasetName",
         native = "establishmentMeans",
         coordinate_uncertainty = "coordinateUncertaintyInMeters") %>% 
# Kosovo has no ISO3c code -> gets country code NA -> think about fixing this
# Problem while computing `country = countrycode(country, origin = "country.name", destination = "iso3c")`.
# Some values were not matched unambiguously: Kosovo, Türkiye, unknown or invalid 
  mutate(country = replace(country, country == "Türkiye", "Turkey")) %>%
  mutate(country = countrycode(country, origin = "country.name", destination = "iso3c"))

# free up memory
rm(occ_bien) # to free up memory
rm(occ_gbif) # to free up memory

# save(occ_bien_std, file = file.path("data", "occ_bien_std.RData"))
# save(occ_gbif_std, file = file.path("data", "occ_gbif_std.RData"))


# load("data/occ_bien_std.RData")
# load("data/occ_gbif_std.RData")

# clean data -------------------------------------------------------------------
occ_cleaned <- bind_rows(occ_bien_std, occ_gbif_std) %>% 
  mutate_at(vars(lon, lat), round, 4) %>%             # round to four digits (corresponds to a maximum of 11.13 m at equator)
  dplyr::filter(!(is.na(lat) | is.na(lon)),           # only records with coords
                !(lat == lon | lat == 0 | lon == 0),  # coords should not be equal
                !(year < 1900 | year > 2023),         # no unrealistic years
                (is.na(coordinate_uncertainty) | coordinate_uncertainty < 10000)) %>%  # coordinate precision < 10km 
  arrange(native, coordinate_uncertainty) %>%                                # sort before distinct() to keep the most informative records 
  distinct(species, lon, lat, year, country, datasource, .keep_all = TRUE) %>%  # remove duplicate or redundant records
  clean_coordinates(lon = "lon", lat = "lat", species = "species", countries = "country", 
                    tests = c("centroids", "capitals", "gbif", "institutions"))

# save(occ_cleaned, file = file.path("data", "occ_cleaned.RData"))


# free up memory 
rm(occ_bien_std)
rm(occ_gbif_std)


# Final subset and filtering ----

# 26,704,899 occurrences before this step 

occ_cleaned_slim <- occ_cleaned %>% 
  dplyr::filter(.summary == TRUE) %>% # remove occurrences that were flagged by coordinateCleaner
  rowid_to_column(var = "occ_id") %>% # create unique identifier for each occurrence
  dplyr::select(occ_id, species, lon, lat, country, year, datasource, dataset, native, cult_obs, cult_reg, cult_loc) # select only relevant columns

# 26,186,010 occurrences after this step

save(occ_cleaned_slim, file =  file.path("data", "occ_cleaned_slim.RData"))

# free up memory
rm(occ_cleaned)


# get all species names and save them for later
specs_all <- sort(unique(occ_cleaned_slim$species))
save(specs_all, file = "data/specs_all.RData")