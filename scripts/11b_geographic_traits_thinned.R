library(dplyr)


# required data -----------------------------------------------------------
load("data/regional_occs/criterion_1/occ_count_crit_1.RData")


# determine the original number of suitable species

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

spp_original <- spp_suitable$species # n = 554

specs <- spp_original[1:2]



# prepare empty df to store information
native_range_df <- data.frame(species = specs,
                              range_wcvp = NA,
                              range_both = NA)
for (spec in specs) {
  
  # load thinned native occurrences for the current species
  load(paste0("data/testing/nat_occs/coords_final_nat_200_",spec,".RData"))
  
  df <- coords_final_nat_200 %>%
    filter(present == 1)
  
} # end of loop over specs
