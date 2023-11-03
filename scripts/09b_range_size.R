
# preamble ----------------------------------------------------------------

library(dplyr)
library(GIFT)
library(sf)
library(terra)


rm(list = ls())

path_data <- "Z:/roennfeldt/C1/data/"


# required data -----------------------------------------------------------

# load final species list
load("data/testing/final_species_list_preliminary.RData")

# load in tdwg lvl 3
tdwg <- st_read("data/tdwg/geojson/level3.geojson")



# native range size and centroid ------------------------------------------


# prepare dfs for the native range size and latitudinal centroid
native_range_df <- data.frame(species = spp_final,
                              range_wcvp = NA,
                              range_both = NA)

native_centroid_df <- data.frame(species = spp_final,
                                 lon_centroid = NA,
                                 lat_centroid = NA)

# sf_use_s2(FALSE)

# spp <- spp_final[1]


# set counter to 0
counter <- 0


# spp_final <- spp_final[1:10]

# loop over species
for (spp in spp_final) {
  
  try({
  
  # print(spp)
  counter <- counter + 1
  print(counter)
  
  # load thinned native occurrences 
  load(paste0(path_data, "coords_final_nat/coords_final_nat_200_",spp, ".RData")) # object is called "coords_final_nat_200"
  # load native occurrences (pre-thinning)
  load(paste0(path_data, "regional_occs/criterion_1/native/nat_occs",spp, ".RData")) # object is called "nat"
  
  # only keep presences from the native range
  coords_final_nat_200 <- subset(coords_final_nat_200, present == 1)
  
  # join data based on lon/lat 
  joined_df  <- inner_join(coords_final_nat_200, nat,
                           by = c("lon" = "lon", "lat" = "lat", "species" = "species")) 
  
  
  # unique coords are often matched to multiple occurrence IDs 
  # We have to make sure that these lie within the same WCVP/GIFT regions to be able to calculate the native area 
  
  # coords <- unique(joined_df[,c("lon", "lat")])
  # 
  # for (r in 1:nrow(coords)) {
  #   
  #   regs_wcvp <- subset(joined_df, lon == coords[r,1] & lat == coords[r,2])$wcvp_LEVEL3_COD
  #   regs_gift <- subset(joined_df, lon == coords[r,1] & lat == coords[r,2])$gift_entity_ID
  #   
  #   
  #   
  #   
  #   if (length(unique(regs_wcvp)) != 1) {
  #     print(r)
  #     print("More than one WCVP region")}
  #   if (length(unique(regs_gift)) != 1) {
  #     print(r)
  #     print("More than one GIFT region")}
  #  
  # } # end of for loop over rows
  
  # No problems occurred. Proceed~
  
  joined_df <- joined_df %>%
    distinct(lon, lat, wcvp_LEVEL3_COD, .keep_all = TRUE)
  
  # sf_use_s2(TRUE)
  
  # # sum the area size of unique wcvp polygons 
  # native_range_wcvp <- joined_df %>%
  #   distinct(wcvp_LEVEL3_NAM, .keep_all = TRUE) %>%
  #   select(wcvp_area) %>%
  #   na.omit() %>%
  #   summarise_all(sum) %>%
  #   pull() %>%
  #   round(2)
  # 
  # 
  # # sum the area size of unique gift polygons when wcvp area = NA
  # # combine wcvp and gift area sizes
  # native_range_both <- native_range_wcvp + joined_df %>%
  #   filter(is.na(wcvp_LEVEL3_NAM)) %>%
  #   distinct(gift_entity_ID, .keep_all = TRUE) %>%
  #   select(gift_area) %>%
  #   na.omit() %>%
  #   summarise_all(sum) %>%
  #   pull() %>%
  #   round(2)
  # 
  # native_range_df[native_range_df$species == spp, "range_wcvp"] <- native_range_wcvp
  # native_range_df[native_range_df$species == spp, "range_both"] <- native_range_both
  
  
  # determine latitudinal centroid 
  
  # split the joined_df into two: 
  # 1. occurrences matching wcvp regions
  # 2. occurrences without matching wcvp region but matches to the gift data set
  
  wcvp_occs <- joined_df[!is.na(joined_df$wcvp_area),]
  gift_occs <- joined_df[is.na(joined_df$wcvp_area) & !is.na(joined_df$gift_area),]
  
  # get the names of the regions (lvl 3) to which the species is native
  wcvp_names <- wcvp_occs %>%
    distinct(wcvp_LEVEL3_NAM) %>%
    na.omit() %>%
    pull()
  
  # get the names of the regions (lvl 3) to which the species is native
  gift_names <- gift_occs %>%
    distinct(gift_entity_ID) %>%
    na.omit() %>%
    pull()

  # get the polygons for these regions and combine them into one multipolygon
  wcvp_range <- terra::aggregate(vect(tdwg[tdwg$LEVEL3_NAM %in% wcvp_names,5])) # aggregate -> single instead of multiple polygons
  gift_range <- terra::aggregate(vect(st_make_valid(GIFT_shapes(entity_ID = gift_names)[,5])))
  
  # combine the ranges
  total_range <- terra::aggregate(rbind(wcvp_range, gift_range))
  
  spp_centroid <- centroids(total_range)
  spp_lon <- geom(spp_centroid)[3]
  spp_lat <- geom(spp_centroid)[4]
  
  # add info to the existing df
  native_centroid_df[native_centroid_df$species == spp, "lon_centroid"] <- spp_lon
  native_centroid_df[native_centroid_df$species == spp, "lat_centroid"] <- spp_lat
  
  
  }) # end of try
  
} # end of loop over species

# save(native_range_df, file = "data/trait_analysis/native_range_size.RData")
# save(native_range_df, file = "results/geographic_traits/native_range_size.RData")

save(native_centroid_df, file = "data/trait_analysis/native_centroid.RData")
save(native_centroid_df, file = "results/geographic_traits/native_centroid.RData")

