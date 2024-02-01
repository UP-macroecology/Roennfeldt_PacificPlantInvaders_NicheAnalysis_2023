# library(maps)
library(ade4)
library(dplyr)
library(ecospat)
library(purrr) # for simplify()library(stringr)
library(terra)
library(foreach)
library(doParallel)

rm(list = ls())

# source("scripts/ecospat_mod.R")
source("scripts/ecospat_mod.R")


# paths and data ----------------------------------------------------------

path_ds  <- file.path("Y:/roennfeldt/C1/data/") 

load("data/testing/df_AC_occs.RData")
df_AC_occs_done <- df_AC_occs

load("data/spp_suitable_after_thinning.RData")
spp <- setdiff(spp_suitable, unique(df_AC_occs_done$species))



# prepare df to store info in

df_AC_occs <- data.frame(species = as.character(NULL), region = as.character(NULL), nat_AC = as.numeric(NULL), intr_AC = as.numeric(NULL), stringsAsFactors = TRUE) 

# randomly select example species
# spp <- sample(spp_suitable, 10)
# spp_index <- 1

counter <- 1
# loop over all species
foreach(spp_index = 1:length(spp), .packages = c("terra", "dplyr", "ade4", "ecospat", "stringr", "purrr")) %do% {
  try({
    
    print(counter)
    counter <- counter + 1
    # load nat occurrences
    load(paste0(path_ds, "/final_input_nat/input_nat_",spp[spp_index],".RData")) 
    
    
    # rename object to a shorter version and remove original
    input_nat <- data_prep_nat
    rm(data_prep_nat)
    
    
    # get the regions in which this species occurs as introduced species
    regions <- list.files(path = paste0(path_ds, "/final_input_intr/"), pattern = paste0(spp[spp_index],".RData")) %>% #TODO
      str_remove(".RData") %>% 
      str_split(pattern = "_") %>%
      purrr::map(~ .x[[3]]) %>%
      simplify() %>%
      unique()
    
    
    # region <- sample(regions, 1)
    
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
        
        
        # map("world")
        # points(subset(input_intr, present == 1)[,c("lon", "lat")], col = "red")
        # points(subset(input_nat, present == 1)[,c("lon", "lat")], col = "blue")
        # 
        # ecospat.plot.niche.dyn(grid_clim_nat, grid_clim_intr, interest = 2,
        #                        col.unf = "lightgoldenrod1",
        #                        col.exp = "darkseagreen3",
        #                        col.stab = "lightblue2",
        #                        colZ1 = "black",
        #                        colZ2 = "red")
        
      
        
        # case 1 - intr env extent is completely in nat: -----
        
        ext_nat <- grid_clim_nat$Z
        ext_intr <- grid_clim_intr$Z
        
        ext_nat[ext_nat > 0] <- 1
        ext_nat[ext_nat != 1] <- NA

        ext_intr[ext_intr > 0] <- 1
        ext_intr[ext_intr != 1] <- NA
        

        ext_AC <- terra::intersect(ext_nat, ext_intr) # returns TRUE or FALSE cell values
        
        values(ext_AC)[which(values(ext_AC)) == TRUE] <- 1
        
        # quant <- 0
        # terra::contour(ext_AC, add = TRUE, levels = quantile(ext_AC[ext_AC > 0], c(0, quant)),
        #                drawlabels = FALSE, lty = c(1, 2), col = "purple")
        
        ext_AC[ext_AC == 0] <- NA
        
        # plot(ext_AC)
        

        
        vec_AC <- as.polygons(ext_AC, trunc = TRUE, dissolve = TRUE, values = TRUE,
                         na.rm = TRUE, na.all = FALSE, extent = FALSE)
        
        # terra::lines(vec_AC, col ="purple")
        
        


        # intr occs 
        # terra::points(grid_clim_intr[["sp"]],  col = "darkred")
        
        intr_AC <- terra::intersect(terra::vect(grid_clim_intr[["sp"]]), vec_AC)
        nr_intr <- as.numeric(length(intr_AC))
        
        # nat occs 
        nat_AC <- terra::intersect(terra::vect(grid_clim_nat[["sp"]]), vec_AC)
        nr_nat <- as.numeric(length(nat_AC))
        
        # terra::points(grid_clim_nat[["sp"]],  col = "darkblue")
        # terra::points(nat_AC,  col = "lightblue")
        # 
        # add info to df
        df_AC_occs <- rbind(df_AC_occs,
                            data.frame(species = spp[spp_index],
                                       region = region,
                                       nat_AC = nr_nat,
                                       intr_AC = nr_intr))

      } # end of if condition over input_intr
    } # end of for loop over regions
    
  })} # end of foreach 

df_AC_occs <- rbind(df_AC_occs_done, df_AC_occs)
save(df_AC_occs, file = "data/testing/df_AC_occs.RData")

# 

rm(list = ls())

load("data/testing/df_AC_occs.RData")

length(unique(df_AC_occs$species))

df_AC_occs <- df_AC_occs %>%
  mutate(nat_enough = case_when(nat_AC < 20 ~ "exclude")) %>%
  mutate(intr_enough = case_when(intr_AC < 20 ~ "exclude")) 

df_exclude <- subset(df_AC_occs, nat_enough == "exclude" | intr_enough == "exclude")

# combine this information with the regional occs count from script XX

load("data/nr_occs_df_after_thinning.RData")

suitable <- nr_occs_df[,-1]
suitable[suitable < 20] <- 0
suitable[suitable >= 20] <- 1
suitable$species <- nr_occs_df$species
suitable <- suitable %>% relocate(species)
suitable$mainland_regions <- rowSums(suitable[,4:10])

spp_suitable <- suitable[!(suitable$nat == 0 | suitable$pac == 0 | suitable$mainland_regions == 0),]$species


suitable_AC <- suitable

for (r in 1:nrow(df_exclude)) {
  
  spec <- df_exclude[r,"species"]
  reg <- df_exclude[r,"region"]
  
  suitable_AC[suitable_AC$species == spec,reg] <- 0
  
} # end of for loop over df_exclude rows


spp_suitable_AC <- suitable_AC[!(suitable_AC$nat == 0 | suitable_AC$pac == 0 | suitable_AC$mainland_regions == 0),]$species

save(spp_suitable_AC, file = "data/spp_suitable_AC.RData")


table(suitable$afr)
table(suitable$ate)
table(suitable$atr)
table(suitable$aus)
table(suitable$eur)
table(suitable$nam)
table(suitable$sam)

table(suitable_AC$afr)
table(suitable_AC$ate)
table(suitable_AC$atr)
table(suitable_AC$aus)
table(suitable_AC$eur)
table(suitable_AC$nam)
table(suitable_AC$sam)
