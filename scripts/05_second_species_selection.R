#' ---------------------------
#
# Purpose of script: final species selection before running the niche comparison
# Author: Anna Rönnfeldt
# Date Created: ~ 2023-11
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------


library(dplyr)
library(stringr)



files_nat <- list.files("data/occurrence_data/final_input_nat/", pattern = ".RData")

specs <- files_nat %>%
  str_remove_all(pattern = "input_nat_") %>%
  str_remove_all(pattern = ".RData")

# create df for that will store the number of occs per species and region
# rows: 554 (number of specs in spp)
# columns: 9 (native, pac, eur, afr, ate, atr, sam, nam)
nr_occs_df <- data.frame(species = specs,
                         nat = 0,
                         pac = 0,
                         afr = 0,
                         ate = 0,
                         atr = 0,
                         aus = 0,
                         eur = 0,
                         nam = 0,
                         sam = 0)


counter <- 0

for (file in files_nat) {
  
  counter <- counter + 1
  
  print(counter)
  
  load(paste0("data/occurrence_data/final_input_nat/",file))
  
  # get the species name
  spp <- file %>%
    str_remove_all(pattern = "input_nat_") %>%
    str_remove_all(pattern = ".RData")
  
  # get the number of native presences
  nr_nat <- as.numeric(nrow(subset(data_prep_nat, present == 1)))
  
  nr_occs_df[nr_occs_df$species == spp, "nat"] <- nr_nat
} # end of for loop over files

counter <- 0
for (spp in specs) {
  
  counter <- counter + 1
  print(counter)
  print(spp)
  
  files_intr <- list.files("data/occurrence_data/final_input_intr/", pattern = spp)
  
  regions <- files_intr %>%
    str_remove_all(pattern = "input_intr_") %>%
    str_remove_all(pattern = paste0("_",spp,".RData"))
  
  for (file in files_intr) {
    
    load(paste0("data/occurrence_data/final_input_intr/",file))
    
    region <-  file %>%
      str_remove_all(pattern = "input_intr_") %>%
      str_remove_all(pattern = paste0("_",spp,".RData"))
    
    nr_region <- as.numeric(nrow(subset(data_prep_intr, present == 1)))
    
    nr_occs_df[nr_occs_df$species == spp, region] <- nr_region
  } # end of for loop over files_intr
  
} # end of for loop over specs

nr_occs_df <- nr_occs_df[,1:10]

save(nr_occs_df, file = "data/occurrence_data/nr_occs_df_after_thinning.RData")

suitable <- nr_occs_df[,-1]
suitable[suitable < 20] <- 0
suitable[suitable >= 20] <- 1
suitable$species <- nr_occs_df$species
suitable <- suitable %>% relocate(species)
suitable$mainland_regions <- rowSums(suitable[,4:10])

spp_suitable <- suitable[!(suitable$nat == 0 | suitable$pac == 0 | suitable$mainland_regions == 0),]

spp_suitable <- spp_suitable$species

save(spp_suitable, file = "data/species_selection/spp_suitable.RData")

