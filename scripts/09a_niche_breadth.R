library(ade4)
library(foreach)
library(doParallel)
library(dplyr)
library(terra)
library(stringr)

rm(list = ls())

# required paths ----------------------------------------------------------

path_imp <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1/")
bioclim_folder <- "/mnt/ibb_share/zurell/envidat/biophysical/CHELSA_V1"

# niche breadth -----------------------------------------------------------

# based on: (line 169)
# https://github.com/UP-macroecology/EBBA_Niche_vs_Range_shifts/blob/main/scripts/3_prep_trait_data.R


# niche breadth is quantified using the Shannon index of the occurrence density grid corrected for environmental prevalence
# to be comparable across species, the environmental background must be the same:

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


# load the global pca environmetn and scores
load(paste0(path_imp, "output/PCA/global_pca.RData"))
load(paste0(path_imp, "output/PCA/global_scores.RData"))

# load final species list
load(paste0(path_imp, "output/final_species_list_preliminary.RData")) #TODO


spp_done <- str_remove_all(list.files(paste0(path_imp,"output/PCA/"), pattern = "niche_breadth_"), 
                           pattern = paste(c("niche_breadth_", ".Rdata"), collapse = "|"))

# spp_done <- str_remove_all(list.files("data/PCA/", pattern = "niche_breadth_"), 
#                            pattern = paste(c("niche_breadth_", ".Rdata"), collapse = "|"))




spp <- setdiff(spp_final, spp_done)

spp_large_files <- NULL

no_cores <- 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# loop over species:
foreach(spp_index = 1:length(spp),
                            .packages = c("ecospat", "ade4", "dplyr","terra"), 
                            .verbose = TRUE,
                            .errorhandling = "remove") %dopar% {
                              
                              # only proceed with spp were the input file is below 1000000 bytes
                              f_size <- file.size(paste0(path_imp, "output/final_input_nat/input_nat_",spp[spp_index],".RData"))
                              
                              if (f_size < 1000000) {
                                # load species occurrence data already matched with bioclim variables
                                load(paste0(path_imp, "output/final_input_nat/input_nat_",spp[spp_index],".RData")) 
                                
                                # rename object to a shorter version and remove original
                                input_nat <- data_prep_nat
                                rm(data_prep_nat)
                                
                                # data frame to store results:
                                nb_spec_df <- data.frame("species" = sub("_", " ", spp[spp_index]),
                                                         "niche_breadth_zcor" = NA)
                                
                                # PCA scores for species occurrences:
                                regional_scores <- suprow(pca_env_global, input_nat[, paste0("bio", 1:19, "_1981-2010_V.2.1")])$li # PCA scores the native species distribution
                                
                                # calculate the Occurrence Densities Grid for Birdlife distribution:
                                
                                # native range area:
                                grid_clim_native <- ecospat.grid.clim.dyn(glob = global_scores, # background pixels of the global range
                                                                          glob1 = global_scores, # same for background pixels of the species
                                                                          sp = regional_scores, # environmental values for the occurrences of the species
                                                                          R = 100, 
                                                                          th.sp = 0)
                                
                                nb_spec_df$niche_breadth_zcor <- vegan::diversity(as.vector(grid_clim_native$z.cor), index = "shannon")
                                
                                
                                save(nb_spec_df, file = paste0(path_imp,"output/PCA/niche_breadth_",spp[spp_index],".Rdata"))
                                
                                rm(regional_scores)
                                
                              } else {
                                spp_large_files <- c(spp_large_files, spp[spp_index])
                                } # end of else
                              
                              spp_large_files
                            } # end of foreach over spp


save(spp_large_files, file = paste0(path_imp,"output/PCA/spp_large_files.Rdata"))


stopCluster(cl)