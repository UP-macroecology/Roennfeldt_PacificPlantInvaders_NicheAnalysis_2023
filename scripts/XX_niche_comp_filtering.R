
library(ade4)
library(ecospat)
library(stringr)
library(dplyr)
library(purrr) # for simplify()

rm(list = ls())

# source("scripts/ecospat_mod.R")
source("scripts/ecospat_mod.R")


# paths and data ----------------------------------------------------------

path_ds  <- file.path("Y:/roennfeldt/C1/data/") 

load("data/spp_suitable_after_thinning.RData")


# randomly select example species
spp <- sample(spp_suitable, 1)
spp_index <- 1

# loop over all species
foreach(spp_index = 1:length(spp), .packages = c("terra", "dplyr", "ade4", "ecospat", "stringr", "purrr")) %do% {
  try({
    
    # load nat occurrences
    load(paste0(path_ds, "/final_input_nat/input_nat_",spp[spp_index],".RData")) 
    
    
    # rename object to a shorter version and remove original
    input_nat <- data_prep_nat
    rm(data_prep_nat)
    
    
    # get the regions in which this species occurs as introduced species
    regions <- list.files(path = paste0(path_ds, "/final_input_intr/"), pattern = paste0(spp[spp_index],".RData")) %>% #TODO
      str_remove(".RData") %>% 
      str_split(pattern = "_") %>%
      map(~ .x[[3]]) %>%
      simplify() %>%
      unique()
    
    
    region <- sample(regions, 1)
    
    for (region in regions) {
      
      # load intr occs for current region
      load(paste0(path_ds, "/final_input_intr/input_intr_",region,"_",spp[spp_index],".RData")) #TODO
      
      # rename object to a shorter version and remove original
      input_intr <- data_prep_intr
      rm(data_prep_intr)
      
      # check whether input_intr has enough occurrences (>= 20)
      if (nrow(subset(input_intr, present == 1)) >= 20) {
        
        
        # PCA environment ---------------------------------------------------------
        
        # define the regional PCA environment for this species
        pca_env_regional <- dudi.pca(rbind(input_nat,input_intr)[,7:25], scannf = FALSE, nf = 2)
        
        # save the regional PCA for later
        # save(pca_env_regional, file = paste0(path_imp, "output/PCA/regional_pca_",region,"_",spp[spp_index],".RData")) #TODO
        
        # predict scores on the axis
        # PCA scores for the whole study area
        scores_globclim <- pca_env_regional$li
        # PCA scores for the species' native distribution
        scores_sp_nat <- suprow(pca_env_regional, input_nat[which(input_nat[,"present"] == 1),7:25])$li
        # PCA scores for the species' invasive distribution
        scores_sp_intr <- suprow(pca_env_regional, input_intr[which(input_intr[,"present"] == 1),7:25])$li
        # PCA scores for the whole native study area
        scores_clim_nat <- suprow(pca_env_regional,input_nat[,7:25])$li
        # PCA scores for the whole invaded study area
        scores_clim_intr <- suprow(pca_env_regional,input_intr[,7:25])$li
        
        
        # calculate occurrence densities grid -------------------------------------
        
        # gridding the native niche
        grid_clim_nat  <- ecospat.grid.clim.dyn(glob = scores_globclim,
                                                glob1 = scores_clim_nat,
                                                sp = scores_sp_nat,
                                                R = 100,
                                                th.sp = 0)
        
        # gridding the invasive niche
        grid_clim_intr <- ecospat.grid.clim.dyn(glob = scores_globclim,
                                                glob1 = scores_clim_intr,
                                                sp = scores_sp_intr, 
                                                R = 100,
                                                th.sp = 0)
        
        
        
        ecospat.plot.niche.dyn(grid_clim_nat, grid_clim_intr, interest = 2,
                               col.unf = "lightgoldenrod1", 
                               col.exp = "darkseagreen3",
                               col.stab = "lightblue2",
                               colZ1 = "black",
                               colZ2 = "darkred")
        
        
        # case 1 - intr env extent is completely in nat:
        
        # terra::plot(grid_clim_nat[["Z"]], legend = F, col = c("white", "grey"))
        # terra::points(grid_clim_nat[["sp"]], legend = F, add = T, col = "blue")
        # terra::points(grid_clim_intr[["sp"]], legend = F, add = T, col = "red")
        # 
        
      } # end of if condition over input_intr
    } # end of for loop over regions
  })} # end of foreach 