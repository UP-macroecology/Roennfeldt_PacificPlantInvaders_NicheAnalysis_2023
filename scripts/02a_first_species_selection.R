#' ---------------------------
#
# Purpose of script: first species selection
# Author: Anna Rönnfeldt
# Date Created: ~ 2023-10
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------

library(dplyr)
library(foreach)
library(doParallel)



# required path -----------------------------------------------------------


path_data  <- ""

# required data -----------------------------------------------------------

load(paste0(path_data, "/occurrence_data/regional_occs/criterion_1/occ_count_crit_1.RData")) 

# determine the original number of suitable species as a starting point for the loop
# these are the same species the thinning was done for in the previous two scripts
occ_count_crit_1 <- occ_count_crit_1 %>% 
  arrange(species) %>%
  distinct(species, .keep_all = TRUE)

suitable <- occ_count_crit_1[,-1]
suitable[suitable < 20] <- 0
suitable[suitable >= 20] <- 1
suitable$species <- occ_count_crit_1$species
suitable <- suitable %>% relocate(species)
suitable$mainland_regions <- rowSums(suitable[,4:10])
spp_suitable <- suitable[!(suitable$native_occs == 0 | suitable$pacific_occs == 0 | suitable$mainland_regions == 0),]


spp <- spp_suitable$species


# create df for that will store the number of occs per species and region
nr_occs_df <- data.frame(species = spp,
                         nat = 0,
                         pac = 0,
                         afr = 0,
                         ate = 0,
                         atr = 0,
                         aus = 0,
                         eur = 0,
                         nam = 0,
                         sam = 0)



foreach(spp_index = 1:length(spp), .packages = c("tidyverse")) %do% {
                  
                  try({
                    
                  # load in native occ file for this species
                  
                  load(paste0(path_data, "/occurrence_data/coords_final_nat/coords_final_nat_200_",spp[spp_index],".RData")) # object: coords_final_nat_200 #TODO
                  
                  # get the number of native occurrences and add info to df
                  nr_nat <- nrow(coords_final_nat_200)
                  nr_occs_df[spp_index,"nat"] <- nr_nat
                  
                  # define region abbreviations used to save the different files
                  # regions <- c("pac", "afr", "ate", "atr", "aus", "eur", "nam", "sam")
                  
                  # get the regions for which intr files have been saved for the current species
                  regions <- list.files(paste0(path_data, "/occurrence_data/coords_final_intr/"), pattern = spp[spp_index]) %>%  #TODO
                    str_remove(".RData") %>% 
                    str_split(pattern = "_") %>%
                    map(~ .x[[5]]) %>%
                    simplify()
                  
                  for (region in regions) {
                    
                    load(paste0(path_data, "/occurrence_data/coords_final_intr/coords_final_intr1_200_",region,"_",spp[spp_index],".RData")) #TODO
                    
                    # accidentally saved these objects as "nat" instead of "intr"
                    nr_reg <- nrow(coords_final_nat_200)
                    
                    nr_occs_df[spp_index,region] <- nr_reg
                    
                  } # end of for loop over regions
})} # end of foreach over spp


save(nr_occs_df, file = paste0(path_data, "/occurrence_data/nr_occs_df_first_selection.RData"))


# identify species that fit the final selection criterion AFTER the thinnning:
# - >= 20 occs in the native range
# - >= 20 intr occs in the pacific region
# - >= 20 intr occs in at least one of the other regions

suitable <- nr_occs_df[,-1]
suitable[suitable < 20] <- 0
suitable[suitable >= 20] <- 1
suitable$species <- nr_occs_df$species
suitable <- suitable %>% relocate(species)
suitable$mainland_regions <- rowSums(suitable[,4:10], na.rm = TRUE)
spp_final <- suitable[!(suitable$nat == 0 | suitable$pac == 0 | suitable$mainland_regions == 0),]$species


save(spp_final, file = paste0(path_data, "/species_selection/spp_first_selection.RData")) 

