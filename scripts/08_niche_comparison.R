# library(ade4)
# library(ecospat)
# library(stringr)
# library(dplyr)
# library(purrr) # for simplify()

rm(list = ls())

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

# paths ------------------
path_imp  <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1/") # TODO

# load final species list
# load(paste0(path_imp, "output/final_species_list_preliminary_2.RData")) #TODO
# spp <- spp_final

load(paste0(path_imp, "input/spp_missed.RData"))
spp <- spp_missed

# when adding newly added species:
# load(paste0(path_imp, "input/spp_newly_selected.RData")) #TODO
# 
# spp <- spp_new

# ecospat niche comparison ---

no_cores <- 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)



# loop over all species
foreach(spp_index = 1:length(spp), .packages = c("terra", "dplyr", "ade4", "ecospat", "stringr", "purrr")) %dopar% {
  try({
    
    # load nat occurrences
    load(paste0(path_imp, "output/final_input_nat/input_nat_",spp[spp_index],".RData")) 
    
    
    # rename object to a shorter version and remove original
    input_nat <- data_prep_nat
    rm(data_prep_nat)
    
    
    # get the regions in which this species occurs as introduced species
    regions <- list.files(path = paste0(path_imp, "output/final_input_intr/"), pattern = paste0(spp[spp_index],".RData")) %>% #TODO
      str_remove(".RData") %>% 
      str_split(pattern = "_") %>%
      map(~ .x[[3]]) %>%
      simplify() %>%
      unique()
    
    
    for (region in regions) {
      
      # load intr occs for current region
      load(paste0(path_imp, "output/final_input_intr/input_intr_",region,"_",spp[spp_index],".RData")) #TODO
      
      # rename object to a shorter version and remove original
      input_intr <- data_prep_intr
      rm(data_prep_intr)
      
      # check whether input_intr has enough occurrences (>= 20)
      if (nrow(subset(input_intr, present == 1)) >= 20) {
        
        
        # PCA environment ---------------------------------------------------------
        
        # define the regional PCA environment for this species
        pca_env_regional <- dudi.pca(rbind(input_nat,input_intr)[,7:25], scannf = FALSE, nf = 2)
        
        # save the regional PCA for later
        save(pca_env_regional, file = paste0(path_imp, "output/PCA/regional_pca_",region,"_",spp[spp_index],".RData")) #TODO
        
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
        
        D_overlap <- ecospat.niche.overlap(grid_clim_nat, grid_clim_intr, cor = TRUE)$D
        
        # save results
        save(D_overlap, file = paste0(path_imp, "output/niche_overlap/overlap_",spp[spp_index],"_",region,".RData")) #TODO
        
        
        
        # similarity test ---------------------------------------------------------
        
        # (at least 1200 reps recommended)
        # test settings depend on what one wants to test:
        # to test for niche conservatism: overlap (higher), expansion (lower), stability (higher), unfilling (lower)
        # to test for niche shifts: overlap (lower), expansion (higher), similarity (lower), unfilling (higher)
        
        # niche conservatism
        # intersect between native and introduced range (intersection = 0)
        sim_test_conservatism <- ecospat.niche.similarity.test.mod(grid_clim_nat, grid_clim_intr,
                                                               rep = 1200,
                                                               intersection = 0, 
                                                               overlap.alternative = "higher",
                                                               expansion.alternative = "lower",
                                                               stability.alternative = "higher",
                                                               unfilling.alternative = "lower",
                                                               rand.type = 2)
        
        # niche shifts
        # intersect between native and introduced range (intersection = 0)
        sim_test_shift <- ecospat.niche.similarity.test.mod(grid_clim_nat, grid_clim_intr,
                                                        rep = 1200,
                                                        intersection = 0,
                                                        overlap.alternative = "lower",
                                                        expansion.alternative = "higher",
                                                        stability.alternative = "lower",
                                                        unfilling.alternative = "higher",
                                                        rand.type = 2)
        
        #save results
        save(sim_test_conservatism, file = paste0(path_imp, "output/niche_similarity/sim_conservatism_",spp[spp_index],"_",region,".RData")) #TODO
        save(sim_test_shift, file = paste0(path_imp, "output/niche_similarity/sim_shift_",spp[spp_index],"_",region,".RData")) #TODO
        
        
        # SES ---------------------------------------------------------------------
        
        # standardised effect size - conservatism
        ses_conservatism <- list()
        
        sim.o <- sim_test_conservatism[["sim"]]
        obs.o <- sim_test_conservatism[["obs"]]
        
        ses_conservatism$p.D <- (sum(sim.o$D >= obs.o$D) + 1)/(length(sim.o$D) + 1)
        ses_conservatism$p.I <- (sum(sim.o$I >= obs.o$I) + 1)/(length(sim.o$I) + 1)
        ses_conservatism$ses.rank.D <- ((sum(obs.o$D > sim.o$D) + 1)/(length(sim.o$D) + 1)) 	# standardised effect size based on the rank of the observation among simulated values
        ses_conservatism$ses.rank.I <- ((sum(obs.o$I > sim.o$I) + 1)/(length(sim.o$I) + 1))
        ses_conservatism$ses.z.D <- (obs.o$D - mean(sim.o$D)) / sd(sim.o$D)				# standardised effect size calculated as standardised mean difference between observation and simulated values (the z-score)
        ses_conservatism$ses.z.I <- (obs.o$I - mean(sim.o$I)) / sd(sim.o$I)
        
        save(ses_conservatism, file = paste0(path_imp, "output/niche_similarity/ses_conservatism_",spp[spp_index],"_",region,".RData")) #TODO
        
        # standardised effect size - niche shift
        ses_shift <- list()
        
        sim.o <- sim_test_shift[["sim"]]
        obs.o <- sim_test_shift[["obs"]]
        
        ses_shift$p.D <- (sum(sim.o$D <= obs.o$D) + 1)/(length(sim.o$D) + 1)
        ses_shift$p.I <- (sum(sim.o$I <= obs.o$I) + 1)/(length(sim.o$I) + 1)
        ses_shift$ses.rank.D <- ((sum(obs.o$D > sim.o$D) + 1)/(length(sim.o$D) + 1)) 	# standardised effect size based on the rank of the observation among simulated values
        ses_shift$ses.rank.I <- ((sum(obs.o$I > sim.o$I) + 1)/(length(sim.o$I) + 1))
        ses_shift$ses.z.D <- (obs.o$D - mean(sim.o$D)) / sd(sim.o$D)				# standardised effect size calculated as standardised mean difference between observation and simulated values (the z-score)
        ses_shift$ses.z.I <- (obs.o$I - mean(sim.o$I)) / sd(sim.o$I)
        
        save(ses_shift, file = paste0(path_imp,"output/niche_similarity/ses_shift_ ",spp[spp_index],"_",region,".RData")) #TODO
        
        # niche dynamics ----------------------------------------------------------
        
        # over whole environmental extent
        niche_dyn_whole <- ecospat.niche.dyn.index(grid_clim_nat, grid_clim_intr, intersection = NA)
        # intersection between native and introduced range (= analogue climate)
        niche_dyn_inter <- ecospat.niche.dyn.index(grid_clim_nat, grid_clim_intr, intersection = 0)
        
        # save results
        save(niche_dyn_whole, file = paste0(path_imp,"output/niche_dynamics/niche_dyn_whole_",spp[spp_index],"_",region,".RData")) #TODO
        save(niche_dyn_inter, file = paste0(path_imp, "output/niche_dynamics/niche_dyn_inter_",spp[spp_index],"_",region,".RData")) #TODO
        
        
      } # end of if condition over input_intr
    } # end of for loop over regions
  })} # end of foreach 

stopCluster(cl)
