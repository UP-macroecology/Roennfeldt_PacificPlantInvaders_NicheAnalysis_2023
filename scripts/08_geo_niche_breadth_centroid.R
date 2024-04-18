library(ade4)
library(foreach)
library(doParallel)
library(dplyr)
library(terra)
library(stringr)
library(ecospat)
library(purrr)
library(sf)

rm(list = ls())

terraOptions(tempdir = "/import/ecoc9/data-zurell/roennfeldt/tmp")

# required paths ----------------------------------------------------------

path_imp <- file.path("/import/ecoc9/data-zurell/roennfeldt/C1/")
bioclim_folder <- "/import/ecoc9/data-zurell/roennfeldt/C1/input/Chelsa_V2"

source("ecospat_mod.R")

# global niche -----------------------------------------------------------

# 
# # prepare mask to crop V2 data (excluding oceanic areas)
# pac_islands <- vect(paste0(path_imp, "input/spatial_data/pacific_islands.shp"))
# tdwg <- st_read(paste0(path_imp, "input/spatial_data/level1.geojson"))[-c(6,9),]
# 
# #split the multipolygon up 
# tdwg <- vect(st_cast(tdwg, "POLYGON"))
# # create world mask
# world_mask_chelsa <- rbind(pac_islands, tdwg)

# based on: (line 169)
# https://github.com/UP-macroecology/EBBA_Niche_vs_Range_shifts/blob/main/scripts/3_prep_trait_data.R


# niche breadth is quantified using the Shannon index of the occurrence density grid corrected for environmental prevalence
# to be comparable across species, the environmental background must be the same:

# bio_numb <- c(paste0(c(paste0("0", 1:9),10:19)))

# bio_numb <- c(1:19)


# for (bio in bio_numb) {
#   # bioclim variables:
#   biovars_rast <- rast(file.path(bioclim_folder,
#                                  paste0("CHELSA_bio", bio_numb,"_1981-2010_V.2.1.tif")))
#   
#   # crop layer with world mask
#   biovars_rast <- terra::mask(biovars_rast, world_mask_chelsa)
#   
#   # global raster points (= environmental background, absences and presences):
#   BL_global_points <- biovars_rast[[1]] %>%
#     as.points
#   
#   # add bioclim variables to raster points:
#   BL_vars_df <- terra::extract(biovars_rast, BL_global_points)
#   
#   save(BL_vars_df, file = paste0(path_imp, "output/PCA/BL_vars_df_", bio ,".RData"))
#   
#   rm(biovars_rast)
#   rm(BL_vars_df)
#   gc()
# }
# # 
# # bioclim variables:
# biovars_rast <- rast(file.path(bioclim_folder,
#                                paste0("CHELSA_bio", bio_numb,"_1981-2010_V.2.1.tif")))
# 
# # crop layer with world mask
# biovars_rast <- terra::mask(biovars_rast, world_mask_chelsa)
# 
# # global raster points (= environmental background, absences and presences):
# BL_global_points <- biovars_rast[[1]] %>%
#  as.points
# 
# # add bioclim variables to raster points:
# BL_vars_df <- terra::extract(biovars_rast, BL_global_points)
# 
# 
# 
# BL_vars_df <- na.omit(BL_vars_df)
#save(BL_vars_df, file = paste0(path_imp, "output/PCA/BL_vars_df.RData"))
# load(paste0(path_imp, "output/PCA/BL_vars_df.RData"))

# 
# 
# # change column names to shorter version (so that labels don't overlap in the corplot)
# names(BL_vars_df) <- str_replace(names(BL_vars_df), pattern = "CHELSA_bio", "B_")
# names(BL_vars_df) <- str_replace(names(BL_vars_df), pattern = "_1981-2010_V.2.1", "")
# 
# 
# # assess climate niche by using the first 2 PCA axes:
# # calibrating the PCA in the whole study area:
# pca_env_global <- dudi.pca(BL_vars_df[, paste0("B_", bio_numb)], scannf = FALSE,
#                    nf = 2) # number of axes
# save(pca_env_global, file = paste0(path_imp, "output/PCA/global_pca.RData"))


#load(paste0(path_imp, "output/PCA/global_pca.RData"))

# change column names to shorter version (so that labels don't overlap in the corplot)
#rownames(pca_env_global$co) <- str_replace(rownames(pca_env_global$tco), pattern = "CHELSA_bio10_", "B_")


# cor plot ----------------------------------------------------------------
# pdf(paste0(path_imp, "plots/pca_global_cor_plot.pdf"))
# 
# ecospat.plot.contrib(contrib = pca_env_global$co, eigen = pca_env_global$eig)
# 
# dev.off()


# # predict the scores on the PCA axes:
# global_scores <- pca_env_global$li # PCA scores for global raster points
# save(global_scores, file = paste0(path_imp, "output/PCA/global_scores.RData"))
# 
# 

# native niche breadth ----------------------------------------------------


# load the global pca environmetn and scores
load(paste0(path_imp, "output/PCA/global_pca.RData"))
load(paste0(path_imp, "output/PCA/global_scores.RData"))


# # load final species selection
load(paste0(path_imp, "input/spp_trait_analysis.RData"))



spp_done <- list.files(paste0(path_imp, "output/PCA/"), pattern = "native_niche_breadth_centroid_twice_") %>%
  str_remove(".RData") %>%
  str_split(pattern = "_") %>%
  map(~ .x[[6]]) %>%
  simplify()


spp <- setdiff(spp_traits, spp_done)


no_cores <- 6
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# loop over species:
foreach(spp_index = 1:length(spp),
                            .packages = c("ecospat", "ade4", "dplyr","terra"),
                            .verbose = TRUE
                            ) %dopar% {


                                # load species occurrence data already matched with bioclim variables
                                load(paste0(path_imp, "output/final_input_nat/input_nat_",spp[spp_index],".RData"))


                                data_prep_nat <- subset(data_prep_nat, present == 1)
                                # rename object to a shorter version and remove original
                                input_nat <- data_prep_nat
                                rm(data_prep_nat)

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


# non-native niche breadth ------------------------------------------------
#
# # load final species selection
# load(paste0(path_imp, "input/spp_trait_analysis.RData"))
# load(paste0(path_imp, "input/suitable_AC_df.RData"))
#
# spp_done <- list.files(paste0(path_imp, "output/PCA/"), pattern = "intr_niche_breadth_centroid_twice_") %>%
#   str_remove(".RData") %>%
#   str_split(pattern = "_") %>%
#   map(~ .x[[6]]) %>%
#   simplify()
#
#
# spp <- setdiff(spp_suitable_AC, spp_done)
#
#
# no_cores <- 4
# cl <- makeCluster(no_cores)
# registerDoParallel(cl)
#
# # loop over species:
# foreach(spp_index = 1:length(spp),
#         .packages = c("ecospat", "ade4", "dplyr","terra"),
#         .verbose = TRUE) %dopar% {
#
#
#           # determine regions the species has been introduced to
#           reg_spec <- subset(suitable_AC, species == spp[spp_index]) %>%
#             select(!c(species, nat, mainland_regions))
#           regions <- colnames(reg_spec[,which(reg_spec == 1)])
#
#
#           # loop over regions
#           for (region in regions) {
#
#             # load species occurrence data already matched with bioclim variables
#             load(paste0(path_imp, "output/final_input_intr/input_intr_",region,"_",spp[spp_index],".RData"))
#
#             # rename object to a shorter version and remove original
#             input_intr <- data_prep_intr
#             rm(data_prep_intr)
#
#             data_prep_intr <- subset(data_prep_intr, present == 1)
#
#             nbc_spec_df <- data.frame(species = NA,
#                                       region = NA,
#                                       niche_breadth_zcor = NA,
#                                       niche_centroid1_global = NA,
#                                       niche_centroid2_global = NA,
#                                       niche_centroid1_native = NA,
#                                       niche_centroid2_native = NA)
#
#             # PCA scores for species occurrences:
#             species_scores_global <- suprow(pca_env_global, input_intr[, paste0("bio_", 1:19)])$li # PCA scores the native species distribution
#             species_scores_native <- dudi.pca(input_intr[, paste0("bio_", 1:19)], scannf = FALSE, nf = 2)$li
#
#
#             # calculate the Occurrence Densities Grid for the species distribution:
#
#             # native range area:
#             grid_clim_intr <- ecospat.grid.clim.dyn.mod(glob = global_scores, # background pixels of the global range
#                                                           glob1 = global_scores, # same for background pixels of the species
#                                                           sp = species_scores_global, # environmental values for the occurrences of the species
#                                                           R = 100,
#                                                           th.sp = 0)
#
#
#
#             nbc_spec_df <- rbind(nbc_spec_df,
#                                  data.frame(species = spp[spp_index],
#                                             region = region,
#                                             niche_breadth_zcor = vegan::diversity(as.vector(grid_clim_intr$z.cor), index = "shannon"),
#                                             niche_centroid1_global = median(species_scores_global[,1]),
#                                             niche_centroid2_global = median(species_scores_global[,2]),
#                                             niche_centroid1_native = median(species_scores_native[,1]),
#                                             niche_centroid2_native = median(species_scores_native[,2])))
#
#
#             nbc_spec_df <- nbc_spec_df[-1,]
#
#             save(nbc_spec_df, file = paste0(path_imp,"output/PCA/intr_niche_breadth_centroid_twice_",spp[spp_index],".RData"))
#
#           } # end of for loop over regions
# } # end of foreach over spp
#
#
# stopCluster(cl)
#
