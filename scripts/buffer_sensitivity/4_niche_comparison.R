
# source("scripts/ecospat_mod.R")
source("ecospat_mod.R")
## Packages --------------------------------------------------------------------
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "doParallel", "foreach", "terra", "purrr", "stringr", "ade4", "ecospat" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)

# prepare paths and data ------------------

path_data <- "/import/ecoc9z/data-zurell/roennfeldt/C1/"

# load final species selection
load(paste0(path_data, "input/species_selection/spp_buf_comp.RData"))
spp <- spp_buf_comp[! spp_buf_comp %in% c("Asparagus setaceus")]


# ecospat niche comparison ---

# set up cluster
no_cores <- 16
cl <- makeCluster(no_cores)
registerDoParallel(cl)



# loop over all species
foreach(spp_index = 1:length(spp), .packages = c("terra", "dplyr", "ade4", "ecospat", "stringr", "purrr")) %dopar% {
    
    # load nat occurrences
    load(paste0(path_data, "output/buffer_sensitivity/final_input_nat/input_nat_",spp[spp_index],".RData")) 
    
    
    # rename object to a shorter version and remove original
    input_nat <- data_prep_nat
    rm(data_prep_nat)
    
    
    # get the regions in which this species occurs as introduced species
    regions <- list.files(path = paste0(path_data, "output/buffer_sensitivity/final_input_intr/"), pattern = paste0(spp[spp_index],".RData")) %>% 
      str_remove(".RData") %>% 
      str_split(pattern = "_") %>%
      map(~ .x[[3]]) %>%
      simplify() %>%
      unique()
    
    
    for (region in regions) {
      
      # load intr occs for current region
      load(paste0(path_data, "output/buffer_sensitivity/final_input_intr/input_intr_",region,"_",spp[spp_index],".RData")) 
      
      # rename object to a shorter version and remove original
      input_intr <- data_prep_intr
      rm(data_prep_intr)
      
      # check whether input_intr has enough occurrences (>= 20)
      if (nrow(subset(input_intr, present == 1)) >= 20) {
        
        
        # PCA environment ---------------------------------------------------------
        
        # define the regional PCA environment for this species
        pca_env_regional <- dudi.pca(rbind(input_nat,input_intr)[,7:25], scannf = FALSE, nf = 2)
        
        # save the regional PCA for later if interested
        # save(pca_env_regional, file = paste0(path_data, "output/PCA/regional_pca_",region,"_",spp[spp_index],".RData")) 
        
        # predict scores on the axis
        # PCA scores for the whole study area
        scores_globclim <- pca_env_regional$li
        # PCA scores for the species' native distribution
        scores_sp_nat <- suprow(pca_env_regional, input_nat[which(input_nat[,2] == 1),7:25])$li
        # PCA scores for the species' invasive distribution
        scores_sp_intr <- suprow(pca_env_regional, input_intr[which(input_intr[,2] == 1),7:25])$li
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
        
        
        # niche overlap -----------------------------------------------------------
        
        # D_overlap <- ecospat.niche.overlap(grid_clim_nat, grid_clim_intr, cor = TRUE)$D
        # 
        # # save results
        # save(D_overlap, file = paste0(path_results, "/ecospat/niche_overlap/overlap_",spp[spp_index],"_",region,".RData")) 
        # 
        # 
        
        # similarity test ---------------------------------------------------------
        
        # (at least 1200 reps recommended)
        # test settings depend on what one wants to test:
        # to test for niche conservatism: overlap (higher), expansion (lower), stability (higher), unfilling (lower)
        # to test for niche shifts: overlap (lower), expansion (higher), similarity (lower), unfilling (higher)
        
        # niche conservatism
        # intersect between native and introduced range (intersection = 0), meaning analogue conditions
        sim_test_conservatism <- ecospat.niche.similarity.test.mod(grid_clim_nat, grid_clim_intr,
                                                                   rep = 1200,
                                                                   intersection = 0, # 0 means the analysis is only done for the intersection of native and introduced niche
                                                                   overlap.alternative = "higher",
                                                                   expansion.alternative = "lower",
                                                                   stability.alternative = "higher",
                                                                   unfilling.alternative = "lower",
                                                                   rand.type = 2)
        
        # niche shifts
        # intersect between native and introduced range (intersection = 0), meaning analogue conditions
        sim_test_shift <- ecospat.niche.similarity.test.mod(grid_clim_nat, grid_clim_intr,
                                                            rep = 1200,
                                                            intersection = 0, 
                                                            overlap.alternative = "lower",
                                                            expansion.alternative = "higher",
                                                            stability.alternative = "lower",
                                                            unfilling.alternative = "higher",
                                                            rand.type = 2)
        
        #save results
        save(sim_test_conservatism, file = paste0(path_data, "output/buffer_sensitivity/niche_similarity/sim_conservatism_",spp[spp_index],"_",region,".RData")) 
        save(sim_test_shift, file = paste0(path_data, "output/buffer_sensitivity/niche_similarity/sim_shift_",spp[spp_index],"_",region,".RData")) 
        
        
        # niche dynamics ----------------------------------------------------------
        
        # # over whole environmental extent
        # niche_dyn_whole <- ecospat.niche.dyn.index(grid_clim_nat, grid_clim_intr, intersection = NA)
        # # intersection between native and introduced range (= analogue climate)
        # niche_dyn_inter <- ecospat.niche.dyn.index(grid_clim_nat, grid_clim_intr, intersection = 0)
        # 
        # # save results
        # save(niche_dyn_whole, file = paste0(path_data,"output/buffer_sensitivity/niche_dynamics/niche_dyn_whole_",spp[spp_index],"_",region,".RData")) 
        # save(niche_dyn_inter, file = paste0(path_data, "output/buffer_sensitivity/niche_dynamics/niche_dyn_inter_",spp[spp_index],"_",region,".RData")) 
        
        
      } # end of if condition over input_intr
    } # end of for loop over regions
  } # end of foreach 

stopCluster(cl)