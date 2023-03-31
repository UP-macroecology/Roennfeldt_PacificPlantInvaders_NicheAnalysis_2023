
# paths
path_imp <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1")

## Packages ---------------------------------------------------------------
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "tidyverse", "sf", "lwgeom" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)


# # load required data 
# load(file.path(path_imp, "input_data", "species_names.RData"))
# load(file.path(path_imp, "input_data", "occ_cleaned_slim.RData"))

tdwg <- st_read(file.path(path_imp, "input_data", "tdwg_lvl3.geojson"))
sf_use_s2(FALSE) # switch off spherical geometry package S2, otherwise join doesn't work due to invalid geometries

load(file.path(path_imp, "output", "occ_tdwg_sf.RData"))
load(file.path(path_imp, "input_data", "powo_dt_harmonized.RData"))


# 2.1 additionally join occurrences which don't overlap with TDWG region to nearest TDWG region if it is not more than 10 km away:

# the outlines of the TDWG regions are defined rather roughly, there are gaps between
# adjacent regions and they don't follow the coastlines exactly, thus there are occurrences
# which don't spatially overlap with any TDWG region, these later get status 'unknown',
# to reduce number of occurrences with unknown status, we match occurrence to the nearest TDWG regions,
# if they are less than 10 km apart (10 km = maximum coordinate uncertainty, defined in 3_occurrence_cleaning.R)

# occurrences that don't intersect with a TDWG level 3 region:
# occ_no_tdwg_sf <- occ_tdwg_sf %>%
#   filter(is.na(LEVEL3_COD)) %>%
#   select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD) # 2,144,549 (82'082 occurrences)
# 
# # nearest TDWG region for each of these occurrences:
# nearest_tdwg <- st_nearest_feature(x = occ_no_tdwg_sf, y = tdwg, longlat = TRUE) # returns index of nearest feature
# nearest_tdwg_sf <- tdwg[nearest_tdwg,]
# 
# # distance of each occurrence to nearest TDWG region (calculation may take a while!):
# dist_nearest_tdwg <- st_distance(occ_no_tdwg_sf,
#                                  nearest_tdwg_sf,
#                                  by_element = TRUE,
#                                  tolerance = units::set_units(10000, m)) # first distance smaller than tolerance will be returned, true distance may be smaller
# 
# # use nearest TDWG region, if distance is not larger than 10 km:
# dist_okay <- which(dist_nearest_tdwg <= units::set_units(10000, m))
# nearest_tdwg <- nearest_tdwg_sf %>% st_drop_geometry
# occ_no_tdwg <- occ_no_tdwg_sf %>% st_drop_geometry
# occ_no_tdwg[dist_okay, c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")] <- nearest_tdwg[dist_okay, c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")]
# 
# # insert into complete data set:
# occ_tdwg_sf[is.na(occ_tdwg_sf$LEVEL3_COD), c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")] <- occ_no_tdwg[,c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")]
# # 9332 occurrences without TDWG region
# 
# save(occ_tdwg_sf, file = paste0(path_imp, "/output/occ_tdwg_sf.RData"))
# # free memory:
# rm(occ_no_tdwg)
# rm(occ_no_tdwg_sf)
# rm(nearest_tdwg)
# rm(nearest_tdwg_sf)
# rm(dist_nearest_tdwg)
# rm(dist_okay)

# 3) join occurrences and status information based on TDWG region:




all_specs_occ_status <- occ_tdwg_sf %>%
  mutate(lon = st_coordinates(.)[,1],lat = st_coordinates(.)[,2]) %>% # add coordinates to data frame for plotting
  st_drop_geometry() %>%
  select(-c(country, year, datasource, dataset, native)) %>%
  left_join(powo_dt_harmonized, by = c("species" = "searched_name", "LEVEL3_NAM" = "tdwgName")) %>% # join occurrences and status, all occurrences included
  group_by(occ_id) %>%         # some occurrences have been matched to 2 TDWG regions
  arrange(desc(status)) %>%    # so use only one status (priority: native > introduced > NA)
  slice(1) %>%                 # this operation is somewhat expensive, so be careful with >> 1M records
  ungroup() %>%
  mutate(status = replace_na(status, "unknown")) %>% # all occurrences which could not be matched to a status get status "unknown"
  select(occ_id, lon, lat, species, tdwg_l3_name = LEVEL3_NAM, tdwg_l3_code = LEVEL3_COD,
         tdwg_l2_code = LEVEL2_COD, tdwg_l1_code = LEVEL1_COD, status_POWO = status,
         lcvp_name, powo_name, ipni_id)

save(all_specs_occ_status, file = paste0(path_imp, "/output/all_specs_occ_status.RData"))
#load(file.path("data", "blacklist_occ_status.RData"))

# free memory:
# rm(occ_cleaned_slim)







# # convert occurrences to spatial data ----
# 
# occ_sf <-  occ_cleaned_slim %>%
#   filter(species %in% specs_all) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = st_crs(tdwg)) # (1'122'764 occurrences of species in blacklist)
# # write to shapefile to examine in QGIS:
# st_write(occ_sf, paste0(path_imp,"/output/occ_all.shp"))
# 
# # join TDWG region in which an occurrence is located:
# sf_use_s2(FALSE) # switch off spherical geometry package S2, otherwise join doesn't work due to invalid geometries
# occ_tdwg_sf <- st_join(occ_sf, tdwg, st_intersects, left = TRUE) # all occurrences are kept, also those that don't intersect with a region
# 
# save(occ_tdwg_sf, file = paste0(path_imp, "/output/occ_tdwg_region_sf.RData"))
# 


