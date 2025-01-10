library(ade4)
library(dplyr)
library(ecospat)
library(purrr) # required for simplify
library(stringr) #required for str_split
library(terra)


load("data/species_selection/spp_suitable_after_thinning.RData")

# prep df to sotre results
AC_occs_df <- as.data.frame(matrix(ncol = 5, nrow = 0))
names(AC_occs_df) <- c("species", "intr_region", "intr_occs_AC", "nat_occs_AC", "suitability")

counter <- 0
for (spp in spp_suitable) {
  
  counter <- counter + 1
  print(counter)
  
  # get the regions in which this species occurs as introduced species
  regions <- list.files(path = "data/final_input_intr/", pattern = paste0("_",spp,".RData")) %>% 
    str_remove(".RData") %>% 
    str_split(pattern = "_") %>%
    map(~ .x[[3]]) %>%
    simplify() %>%
    unique()
  
  for (region in regions) {
    
    # load intr and nat occurrences of that species-region combination
    load(paste0("data/final_input_nat/input_nat_", spp,".RData")) 
    load(paste0("data/final_input_intr/input_intr_",region,"_",spp,".RData"))
    
    # rename object to a shorter version and remove original
    input_intr <- data_prep_intr
    input_nat <- data_prep_nat
    rm(data_prep_nat, data_prep_intr)
    
    if (nrow(subset(input_intr, present == 1)) >= 20 & nrow(subset(input_nat, present == 1)) >= 20) {
      
      # PCA environment ---------------------------------------------------------
      
      # define the regional PCA environment for this species
      pca_env_regional <- dudi.pca(rbind(input_nat,input_intr)[,7:25], scannf = FALSE, nf = 2)
      
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
      
      
      # identify analogue niche space -------------------------------------------
      
      values(grid_clim_nat$Z)[which(values(grid_clim_nat$Z) > 0)] <- 1
      values(grid_clim_intr$Z)[which(values(grid_clim_intr$Z) > 0)] <- 1
      
      rast_analogue <- grid_clim_nat$Z + grid_clim_intr$Z
      values(rast_analogue)[which(values(rast_analogue) < 2)] <- 0
      
      ## plot niche space ------------------------------------------------------
      
      # quant = 0
      # col.unf = "lightgoldenrod1"
      # col.exp = "darkseagreen3"
      # col.stab = "lightblue2"
      # col_Z1 = "black" # native
      # col_Z2 = "darkred" # non-native
      # col_AC = "purple" # analogue 
      # name.axis1 = "Axis 1"
      # name.axis2 = "Axis 2"
      # col_category <- c("#FFFFFF",col.exp,col.unf,col.stab)[1 + (sort(terra::unique(2*grid_clim_nat$w + grid_clim_intr$w)[,1]))]
      # 
      # terra::plot(2*grid_clim_nat$w + grid_clim_intr$w, col = col_category, legend = FALSE)
      # 
      # # outline of env available in the nat range
      # terra::contour(rast_nat, add = TRUE, levels = quantile(grid_clim_nat$Z[grid_clim_nat$Z > 0], c(0, quant)),
      #   drawlabels = FALSE, lty = c(1, 2), col = col_Z1)
      # 
      # # outline of env available in the intr range
      # terra::contour(rast_intr, add = TRUE, levels = quantile(grid_clim_intr$Z[grid_clim_intr$Z > 0], c(0, quant)),
      #   drawlabels = FALSE, lty = c(1, 2), col = col_Z2)
      # 
      # # outline of analogue env conditions
      # terra::contour(rast_analogue, add = TRUE, levels = quantile(rast_analogue[rast_analogue > 0], c(0, quant)),
      #   drawlabels = FALSE, lty = c(1, 2), col = col_AC)
      # 
      # # intr occurrence points
      # terra::points(grid_clim_intr[["sp"]],  col = "darkred")
      # 
      # # nat occurrence points
      # terra::points(grid_clim_nat[["sp"]],  col = "black")
      
      
      # identify occurrences within analogue niche space ------------------------
      
      intr_points <- vect(grid_clim_intr[["sp"]])
      nat_points <- vect(grid_clim_nat[["sp"]])
      
      # change cell values outside analogue niche space to NA
      values(rast_analogue)[which(values(rast_analogue) < 2)] <- NA
      
      # transform raster to vector to enable intersect operation
      vect_analogue <- as.polygons(rast_analogue) 
      
      # identify which points are located within the analogue niche space
      intr_points_AC <- terra::intersect(intr_points, vect_analogue)
      nat_points_AC <- terra::intersect(nat_points, vect_analogue)
      
      # count number of occurrence points within analogue niche space
      nr_intr_AC <- nrow(intr_points_AC)
      nr_nat_AC <- nrow(nat_points_AC)
      
      # status to check whether species/region combi makes for a suitable niche pair for the analysis
      # 0 (default) = unsuitable, 1 = suitable
      status <- 0
      if (nr_intr_AC >= 20 & nr_nat_AC >= 20) {status <- 1} 
      
      
      
      
      # store results -----------------------------------------------------------
      
      AC_occs_df <- rbind(AC_occs_df,
                          data.frame(species = spp,
                                     intr_region = region,
                                     intr_occs_AC = nr_intr_AC,
                                     nat_occs_AC = nr_nat_AC,
                                     suitability = status))
      
    } # end of if condition
    
  } # end of for loop over regions
} # end of for loop over species


save(AC_occs_df, file = "data/species_selection/temporary/AC_occs_df.RData")



# select suitable species -------------------------------------------------

# a species is suitable for the analysis if they have more than 20 occurrence point in the analogue native and non-native 
# climatic niche space for the Pacific Islands and at least one of the other study regions

# load("data/species_selection/temporary/AC_occs_df.RData")

# prep df to store info
regional_suitability_df <- data.frame(species = unique(AC_occs_df$species),
                                      nat = 1,
                                      pac = 0,
                                      afr = 0,
                                      ate = 0,
                                      atr = 0,
                                      aus = 0,
                                      eur = 0,
                                      nam = 0,
                                      sam = 0,
                                      sum_mainland = 0)


for (spp in unique(AC_occs_df$species)) {
  
  regions <- unique(AC_occs_df[AC_occs_df$species == spp,"intr_region"])
  
  for (region in regions) {
    
    if (AC_occs_df[AC_occs_df$species == spp & AC_occs_df$intr_region == region, "suitability"] == 1) {
      regional_suitability_df[regional_suitability_df$species == spp, region] <- 1
    }  # end of if condition
  } # end of for loop over regions
} # end of for loop over species


regional_suitability_df$sum_mainland <- rowSums(regional_suitability_df[,4:10])

spp_suitable_AC <- regional_suitability_df[!(regional_suitability_df$nat == 0 | 
                                            regional_suitability_df$pac == 0 |
                                            regional_suitability_df$sum_mainland == 0),] %>% 
  pull(species)

save(spp_suitable_AC, file = "data/species_selection/spp_suitable_AC.RData")

