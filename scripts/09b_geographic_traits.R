library(dplyr)
library(maps)
library(sf)
library(terra)

rm(list = ls())


# required paths -----------------------------------------------------------

bioclim_folder <- "Z:/Arbeit/datashare/data/envidat/biophysical/CHELSA_V1"

bioclim_folder <- "Z:/roennfeldt/C1/data/CHELSA_V2/"

# range size (calculated from polygons or convex hull?) 
# latitudinal centroid (from polygons or convex hull?)
# niche breadth (relative to the globally available climatic niche) 


# load("data/status_assignment/occ_status_resolved.RData")
# load("data/occ_cleaned_slim.RData")
# 
# occ_cleaned_slim <- occ_cleaned_slim[,c(1,3:4)]


# table(occ_status_resolved$criterion_1)
# table(occ_status_resolved$criterion_2)

# slightly modify occ_status_resolved
# occ_status_resolved <- occ_status_resolved %>%
#   select(-final_status) %>%
#   mutate(criterion_1 = replace(criterion_1, criterion_1 == "naturalized", "introduced")) %>%
#   mutate(criterion_2 = replace(criterion_2, criterion_2 == "naturalized", "introduced")) %>%
#   mutate(criterion_1 = replace(criterion_1, criterion_1 == "non-native", "introduced")) %>%
#   mutate(criterion_2 = replace(criterion_2, criterion_2 == "non-native", "introduced")) %>%
#   left_join(occ_cleaned_slim, by = "occ_id", keep = TRUE)
# 
# occ_status_resolved <- occ_status_resolved %>%
#   select(-occ_id.y) %>%
#   rename("occ_id" = "occ_id.x") %>%
#   arrange(occ_id)
# 
# save(occ_status_resolved, file = "data/occ_status_resolved_lonlat.RData")

load("data/occ_status_resolved_lonlat.RData")

# native range size -------------------------------------------------------

# approach one: calculate range size from the tdwg lvl 3 polygons (and gift areas where lvl 3 == NA)

# get unique species names
specs <- unique(occ_status_resolved$species)

# prepare df
native_range_df <- data.frame(species = specs,
                              range_wcvp = NA,
                              range_both = NA)

counter <- 0

# loop over species
for (spp in specs) {
  
  counter <- counter + 1
  print(counter)
  
  # subset for the native occurrences of the species
  df <- subset(occ_status_resolved, species == spp) %>%
    select(occ_id, species, lon, lat, wcvp_LEVEL3_NAM, wcvp_area, gift_entity_ID, gift_area, criterion_1) %>% 
    filter(criterion_1 == "native")
  
  # sum the area size of unique wcvp polygons 
  native_range_wcvp <- df %>%
    distinct(wcvp_LEVEL3_NAM, .keep_all = TRUE) %>%
    select(wcvp_area) %>%
    na.omit() %>%
    summarise_all(sum) %>%
    pull() %>%
    round(2)
  
  # sum the area size of unique gift polygons when wcvp area = NA
  # combine wcvp and gift area sizes
  native_range_both <- native_range_wcvp + df %>%
    filter(is.na(wcvp_LEVEL3_NAM)) %>%
    distinct(gift_entity_ID, .keep_all = TRUE) %>%
    select(gift_area) %>%
    na.omit() %>%
    summarise_all(sum) %>%
    pull() %>%
    round(2)
  
  native_range_df[native_range_df$species == spp, "range_wcvp"] <- native_range_wcvp
  native_range_df[native_range_df$species == spp, "range_both"] <- native_range_both
}


save(native_range_df, file = "data/trait_analysis/native_range_size.RData")

rm(list = setdiff(ls(), c("occ_status_resolved", "specs")))


# latitudinal centroid ----------------------------------------------------

# load in tdwg lvl 3
tdwg <- st_read("data/tdwg/geojson/level3.geojson")

# prepare df
native_centroid_df <- data.frame(species = specs,
                                 lon_centroid = NA,
                                 lat_centroid = NA)


counter <- 0
# loop over species
for (spp in specs) {
  
  counter <- counter + 1
  print(counter)
  
  # subset occurrences for current species
  df <- subset(occ_status_resolved, species == spp)
  
  # get the names of the regions (lvl 3) to which the species is native
  region_names <- df %>%
    distinct(wcvp_LEVEL3_NAM) %>%
    na.omit() %>%
    pull()
  
  # get the polygons for these regions and combine them into one multipolygon
  spp_range <- st_union(tdwg[tdwg$LEVEL3_NAM %in% region_names,5])
  
  # get the centroid and its lon lat 
  spp_centroid <- st_centroid(spp_range)[[1]]
  spp_lon <- as.data.frame(st_coordinates(spp_centroid))[1,1]
  spp_lat <- as.data.frame(st_coordinates(spp_centroid))[1,2]
  
  # add info to the existing df
  native_centroid_df[native_centroid_df$species == spp, "lon_centroid"] <- spp_lon
  native_centroid_df[native_centroid_df$species == spp, "lat_centroid"] <- spp_lat
  }

save(native_centroid_df, file = "data/trait_analysis/native_centroid.RData")


# niche breadth -----------------------------------------------------------

# based on: (line 169)
# https://github.com/UP-macroecology/EBBA_Niche_vs_Range_shifts/blob/main/scripts/3_prep_trait_data.R


# niche breadth is quantified using the Shannon index of the occurrence density grid corrected for environmental prevalence
# to be comparable across species, the environmental background must be the same:


# bioclim variables:
biovars_rast <- rast(file.path(bioclim_folder, 
                               paste0("CHELSA_bio10_", paste0(c(paste0("0", 1:9),10:19)),".tif")))

# global raster points (= environmental background, absences and presences):
BL_global_points <- biovars_rast[[1]] %>% 
  as.points

# add bioclim variables to raster points:
BL_vars_df <- terra::extract(biovars_rast, BL_global_points)

# assess climate niche by using the first 2 PCA axes:
# calibrating the PCA in the whole study area:
pca.env <- dudi.pca(BL_vars_df[, paste0("bio", 1:19)], scannf = FALSE,
                    nf = 2) # number of axes

# predict the scores on the PCA axes:
scores.globclim <- pca.env$li # PCA scores for global raster points
