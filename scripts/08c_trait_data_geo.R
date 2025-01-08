#' ---------------------------
#
# Purpose of script: compiling "trait" information about the biogeographic 
# characteristics of the different native/non-native regions 
# Author: Anna Rönnfeldt
# Date Created: ~ 2023-12 
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------

library(dplyr)
library(GIFT)
# library(landscapemetrics)
# library(maps)
library(sf)
library(terra)
library(purrr)
library(stringr)
library(ecospat)


# Native range size -------------------------------------------------------

# load data
load("data/occ_status_resolved_lonlat.RData")
load("data/spp_suitable_after_thinning.RData")

# calculate range size from the tdwg lvl 3 polygons (and gift areas where lvl 3 == NA)

# prepare df
native_range_df <- data.frame(species = spp_suitable,
                              range_wcvp = NA,
                              range_both = NA)

counter <- 0

# loop over species
for (spp in spp_suitable) {
  
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

rm(list = setdiff(ls(), c("occ_status_resolved", "spp_suitable")))



# Native range centroid ---------------------------------------------------

# load data
tdwg <- st_read("data/tdwg/geojson/level3.geojson") # tdwg level 3
load("data/status_assignment/Gift_polygons.RData") # GIFT polygons, prepared during status assignment

# prepare df
native_centroid_df <- data.frame(species = spp_suitable,
                                 lon_centroid = NA,
                                 lat_centroid = NA)


counter <- 0

# loop over species
for (spp in spp_suitable) {
  
  counter <- counter + 1
  print(counter)
  
  # subset occurrences for current species
  df <- subset(occ_status_resolved, species == spp & criterion_1 == "native")
  
  # get the names of the regions (lvl 3) to which the species is native
  region_names <- df %>%
    distinct(wcvp_LEVEL3_NAM) %>%
    na.omit() %>%
    pull()
  
  if (length(region_names != 0)) {
    
    sf_use_s2(FALSE)
    # get the polygons for these regions and combine them into one multipolygon
    spp_range <- st_union(tdwg[tdwg$LEVEL3_NAM %in% region_names,5])
    
    # get the centroid and its lon lat 
    spp_centroid <- st_centroid(spp_range)[[1]]
    
  } else {
    
    # GIFT centroid version if there is no WCVP info on the species
    # get the names of the  GIFT regions to which the species is native
    region_names <- df %>%
      distinct(gift_entity_ID) %>%
      na.omit() %>%
      pull()
    
    sf_use_s2(FALSE)
    
    # get the polygons for these regions and combine them into one multipolygon
    spp_range <- st_union(GIFT_polygons[GIFT_polygons$entity_ID %in% region_names, 13])
    
    # get the centroid and its lon lat 
    spp_centroid <- st_centroid(spp_range)[[1]]
  }
  
  
  spp_lon <- as.data.frame(st_coordinates(spp_centroid))[1,1]
  spp_lat <- as.data.frame(st_coordinates(spp_centroid))[1,2]
  
  # add info to the existing df
  native_centroid_df[native_centroid_df$species == spp, "lon_centroid"] <- spp_lon
  native_centroid_df[native_centroid_df$species == spp, "lat_centroid"] <- spp_lat
}

save(native_centroid_df, file = "data/trait_analysis/native_centroid.RData")

rm(list = setdiff(ls(), c("occ_status_resolved", "spp_suitable", "spp_traits", "tdwg", "GIFT_polygons")))


# Intr. range centroid ----------------------------------------------------

# prepare df
intro_centroid_df <- data.frame(species = NA,
                                region = NA,
                                lon_centroid = NA,
                                lat_centroid = NA)



counter <- 0

# loop over species
for (spp in spp_traits) {
  
  counter <- counter + 1
  print(counter)
  
  # subset df with all occurrences (before thinning) for current species
  occ_spec <- subset(occ_crit_1, species == spp) %>%
    filter(criterion_1 == "introduced") %>% 
    select(species, lon, lat, wcvp_LEVEL3_NAM, wcvp_area, gift_entity_ID, gift_area)
  
  # determine regions the species has been introduced to 
  reg_spec <- subset(suitable_AC, species == spp) %>% 
    select(!c(species, nat, mainland_regions))
  regions <- colnames(reg_spec[,which(reg_spec == 1)])
  
  for (region in regions) {
    
    # load in intr occurrences in that region
    load(paste0(path_data,"input_intr_",region,"_",spp,".RData")) # object name: data_prep_intr
    
    present_coords <- subset(data_prep_intr, present == 1) %>% 
      select(lon, lat)
    
    # match these coords with the ones in occ_spec to acces wcvp information
    df_spec <- present_coords %>% 
      left_join(occ_spec, by = c("lon", "lat")) %>%
      distinct()
    
    # get the names of the regions (lvl 3) to which the species is native
    region_names <- df_spec %>%
      distinct(wcvp_LEVEL3_NAM) %>%
      na.omit() %>%
      pull()
    
    
    if (length(region_names != 0)) {
      
      sf_use_s2(FALSE)
      # get the polygons for these regions and combine them into one multipolygon
      spp_range <- st_union(tdwg[tdwg$LEVEL3_NAM %in% region_names,5])
      
      # get the centroid and its lon lat 
      spp_centroid <- st_centroid(spp_range)[[1]]
      
    } else {
      
      # GIFT centroid version if there is no WCVP info on the species
      # get the names of the  GIFT regions to which the species is native
      region_names <- df_spec %>%
        distinct(gift_entity_ID) %>%
        na.omit() %>%
        pull()
      
      sf_use_s2(FALSE)
      
      # get the polygons for these regions and combine them into one multipolygon
      spp_range <- st_union(GIFT_polygons[GIFT_polygons$entity_ID %in% region_names, 13])
      
      # get the centroid and its lon lat 
      spp_centroid <- st_centroid(spp_range)[[1]]
    }
    
    
    spp_lon <- as.data.frame(st_coordinates(spp_centroid))[1,1]
    spp_lat <- as.data.frame(st_coordinates(spp_centroid))[1,2]
    
    # add results to df
    intro_centroid_df <- rbind(intro_centroid_df, 
                               data.frame(species = spp,
                                          region = region,
                                          lon_centroid = spp_lon,
                                          lat_centroid = spp_lat))
    
  } # end of for loop over regions
} # end of for loop over species

intro_centroid_df <- intro_centroid_df[-1,]

save(intro_centroid_df, file = "data/trait_analysis/intro_centroid.RData")

rm(list = ls())





# Latitudinal distance ----------------------------------------------------

# load data
load("data/trait_analysis/intro_centroid.RData")
load("data/trait_analysis/native_centroid.RData")

df_lat_distance <- data.frame(species = as.factor(NULL),
                              region = as.factor(NULL),
                              lat_nat = as.numeric(NULL),
                              lat_intr = as.numeric(NULL),
                              lat_dist = as.numeric(NULL))


specs <- unique(intro_centroid_df$species)

for (spec in specs) {
  
  
  lat_nat <- native_centroid_df[native_centroid_df$species == spec , "lat_centroid"]
  df_intr <- subset(intro_centroid_df, species == spec)
  
  regions <- unique(df_intr$region) 
  
  for (region in regions) {
    
    lat_intr <- df_intr[df_intr$region == region,"lat_centroid"]
    
    df_lat_distance <- rbind(df_lat_distance,
      data.frame(species = as.factor(spec),
                                  region = as.factor(region),
                 lat_nat = lat_nat,
                 lat_intr = lat_intr,
                                  lat_dist = abs( lat_intr - lat_nat)))
  } # end of for loop over regions
    
} # end of for loop over specs

save(df_lat_distance, file = "data/trait_analysis/lat_dist.RData")

rm(list = ls())

# Global niche ------------------------------------------------------------

# everything related to the niche breath and centroid runs on the HPC, because 
# the file size of the global niche objects crashes local machines

# required path
path_imp <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1/")
bioclim_folder <- "/import/ecoc9z/data-zurell/roennfeldt/C1/input/Chelsa_V2"
# based on: (line 169)
# https://github.com/UP-macroecology/EBBA_Niche_vs_Range_shifts/blob/main/scripts/3_prep_trait_data.R


# niche breadth is quantified using the Shannon index of the occurrence density grid corrected for environmental prevalence
# to be comparable across species, the environmental background must be the same:

# bio_numb <- c(paste0(c(paste0("0", 1:9),10:19)))
bio_numb <- c(1:19)

# bioclim variables:
biovars_rast <- rast(file.path(bioclim_folder,
                               paste0("CHELSA_bio10_", bio_numb,".tif")))

# global raster points (= environmental background, absences and presences):
BL_global_points <- biovars_rast[[1]] %>%
 as.points

# add bioclim variables to raster points:
BL_vars_df <- terra::extract(biovars_rast, BL_global_points)

save(BL_vars_df, file = paste0(path_imp, "output/PCA/BL_vars_df.RData"))

# load(paste0(path_imp, "output/PCA/BL_vars_df.RData"))

BL_vars_df <- na.omit(BL_vars_df)

# change column names to shorter version (so that labels don't overlap in the corplot)
names(BL_vars_df) <- str_replace(names(BL_vars_df), pattern = "CHELSA_bio", "B_")

# assess climate niche by using the first 2 PCA axes:
# calibrating the PCA in the whole study area:
pca_env_global <- dudi.pca(BL_vars_df[, paste0("CHELSA_bio10_", bio_numb)], scannf = FALSE,
                   nf = 2) # number of axes
save(pca_env_global, file = paste0(path_imp, "output/PCA/global_pca.RData"))


# load(paste0(path_imp, "output/PCA/global_pca.RData"))

# change column names to shorter version (so that labels don't overlap in the corplot)
rownames(pca_env_global$co) <- str_replace(rownames(pca_env_global$tco), pattern = "CHELSA_bio10_", "B_")


# cor plot
pdf(paste0(path_imp, "plots/pca_global_cor_plot.pdf"))

ecospat.plot.contrib(contrib = pca_env_global$co, eigen = pca_env_global$eig)

dev.off()


# predict the scores on the PCA axes:
global_scores <- pca_env_global$li # PCA scores for global raster points
save(global_scores, file = paste0(path_imp, "output/PCA/global_scores.RData"))


rm(list = ls())


# Native niche ------------------------------------------------------------

source("ecospat_mod.R")

# load the global pca environment and scores
load(paste0(path_imp, "output/PCA/global_pca.RData"))
load(paste0(path_imp, "output/PCA/global_scores.RData"))

# load final species selection
load(paste0(path_imp, "input/spp_trait_analysis.RData"))


spp_done <- list.files(paste0(path_imp, "output/PCA/"), pattern = "native_niche_breadth_centroid_twice_") %>%
  str_remove(".RData") %>%
  str_split(pattern = "_") %>%
  map(~ .x[[6]]) %>%
  simplify()


spp <- setdiff(spp_traits, spp_done)


no_cores <- 5
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# loop over species:
foreach(spp_index = 1:length(spp),
                            .packages = c("ecospat", "ade4", "dplyr","terra"),
                            .verbose = TRUE
                            ) %dopar% {


                                # load species occurrence data already matched with bioclim variables
                                load(paste0(path_imp, "output/final_input_nat_rev/input_nat_",spp[spp_index],".RData"))

                                # rename object to a shorter version and remove original
                                input_nat <- data_prep_nat
                                rm(data_prep_nat)

                                input_nat <- subset(input_nat, present == 1)

                                # data frame to store results:
                                nbc_spec_df <- data.frame(species = sub("_", " ", spp[spp_index]),
                                                         niche_breadth_zcor = NA,
                                                         niche_centroid1_global = NA,
                                                         niche_centroid2_global = NA,
                                                         niche_centroid1_native = NA,
                                                         niche_centroid2_native = NA)

                                # PCA scores for species occurrences:
                                species_scores_global <- suprow(pca_env_global, input_nat[, paste0("bio_", 1:19)])$li # PCA scores the native species distribution
                                species_scores_native <- dudi.pca(input_nat[, paste0("bio_", 1:19)], scannf = FALSE, nf = 2)$li


                                 # calculate the Occurrence Densities Grid for the species distribution:

                                # native range area:
                                grid_clim_native <- ecospat.grid.clim.dyn.mod(glob = global_scores, # background pixels of the global range
                                                                          glob1 = global_scores, # same for background pixels of the species
                                                                          sp = species_scores_global, # environmental values for the occurrences of the species
                                                                          R = 100,
                                                                          th.sp = 0)

                                nbc_spec_df$niche_breadth_zcor <- vegan::diversity(as.vector(grid_clim_native$z.cor), index = "shannon")

                                # determine niche centroid of the native range
                                nbc_spec_df$niche_centroid1_global <- median(species_scores_global[,1])
                                nbc_spec_df$niche_centroid2_global <- median(species_scores_global[,2])

                                nbc_spec_df$niche_centroid1_native <- median(species_scores_native[,1])
                                nbc_spec_df$niche_centroid2_native <- median(species_scores_native[,2])

                                save(nbc_spec_df, file = paste0(path_imp,"output/PCA/native_niche_breadth_centroid_twice_",spp[spp_index],".RData"))


                            } # end of foreach over spp


stopCluster(cl)



# combine data on native niche breadth and centroid

df_native_niche <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(df_native_niche) <- c("species", "niche_breadth_zcor", "niche_centroid1_global", "niche_centroid2_global", "niche_centroid1_native", "niche_centroid2_native")

spp_breadth <- list.files("data/trait_analysis/niche_breadth_centroid/", pattern = "niche_breadth_centroid_twice_") %>%
  str_remove(".RData") %>%
  str_split(pattern = "_") %>%
  map(~ .x[[6]]) %>%
  simplify()

for (spp in spp_breadth) {
  
  load(paste0("data/trait_analysis/niche_breadth_centroid/native_niche_breadth_centroid_twice_",spp,".RData")) # object name: nbc_spec_df
  df_native_niche <- rbind(df_native_niche, nbc_spec_df)
} # end of loop over spp_suitable_AC

# save results
save(df_native_niche, file = "data/trait_analysis/native_niche_breadth_centroid.RData")




