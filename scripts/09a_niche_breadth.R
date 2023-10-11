library(ade4)
library(terra)
library(dplyr)

rm(list = ls())

# required paths ----------------------------------------------------------

path_imp <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1/")
bioclim_folder <- "/mnt/ibb_share/zurell/envidat/biophysical/CHELSA_V1"

# # niche breadth -----------------------------------------------------------
# 
# # based on: (line 169)
# # https://github.com/UP-macroecology/EBBA_Niche_vs_Range_shifts/blob/main/scripts/3_prep_trait_data.R
# 
# 
# # niche breadth is quantified using the Shannon index of the occurrence density grid corrected for environmental prevalence
# # to be comparable across species, the environmental background must be the same:
# 
# bio_numb <- c(paste0(c(paste0("0", 1:9),10:19)))
# 
# # bioclim variables:
# # biovars_rast <- rast(file.path(bioclim_folder,
# #                                paste0("CHELSA_bio10_", bio_numb,".tif")))
# # 
# # # global raster points (= environmental background, absences and presences):
# # BL_global_points <- biovars_rast[[1]] %>%
# #  as.points
# # 
# # # add bioclim variables to raster points:
# # BL_vars_df <- terra::extract(biovars_rast, BL_global_points)
# # save(BL_vars_df, file = paste0(path_imp, "output/PCA/BL_vars_df.RData"))
# load(paste0(path_imp, "output/PCA/BL_vars_df.RData"))
# 
# BL_vars_df <- na.omit(BL_vars_df)
# 
# # assess climate niche by using the first 2 PCA axes:
# # calibrating the PCA in the whole study area:
# pca_env_global <- dudi.pca(BL_vars_df[, paste0("CHELSA_bio10_", bio_numb)], scannf = FALSE,
#                    nf = 2) # number of axes
# save(pca_env_global, file = paste0(path_imp, "output/PCA/global_pca.RData"))
# 
# # predict the scores on the PCA axes:
# global_scores <- pca_env_global$li # PCA scores for global raster points
# save(global_scores, file = paste0(path_imp, "output/PCA/global_scores.RData"))
# 

load(paste0(path_imp, "output/PCA/global_scores.RData"))
