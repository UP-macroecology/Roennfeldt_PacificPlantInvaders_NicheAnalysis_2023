library(dplyr)

rm(list = ls())

species_names <- read.table("data/PaciFLora.txt", header = TRUE)
load("data/spp_suitable_AC.RData")

spp_suitable_AC <- as.data.frame(spp_suitable_AC)
colnames(spp_suitable_AC) <- "Species"


load("shiny/NicheDyn_exploration/data/input_TA_unstand.RData")

input_TA <- input_TA %>% 
  select(!c(lat_dist, rel_expansion, rel_stability, rel_unfilling, rel_abandonment, rel_pioneering, expansion, unfilling, stability, overlap, region, species_region, years_since_intro, total_esu)) %>% 
  distinct()




shiny_spp_traits <- spp_suitable_AC %>% 
  left_join(select(species_names, Species, Family), by = "Species") %>% 
  rename("species" = "Species",
         "family" = "Family") %>% 
  left_join(select(input_TA, species, mean_height, mean_seedmass, growth_form, lifecycle, dispersal, range_size_nat, niche_breadth_nat, niche_centroid_a_nat, niche_centroid_b_nat ), by = "species") %>% 
  distinct()

save(shiny_spp_traits, file = "shiny/NicheDyn_exploration/data/shiny_spp_traits.RData" )
