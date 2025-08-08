

#' ---------------------------
#
# Purpose of script: Selecting 32 sample species (10 % of the study species) to test whether the analysis results are sensitive to the buffer size 
# Author: Anna Rönnfeldt
# Date Created: 2025-06-10
# Email: roennfeldt@uni-potsdam.de
#
# Notes: The buffer size refers to the spatial buffer around the occurrence points within which pseudo-absences are sampled. 
# The species selection will be base on a random subset of the species included in the trait analysis.
#
#' ---------------------------




# preamble ----------------------------------------------------------------

#libraries



# required data  ----------------------------------------------------------


# species included in the trait analysis
load("data/species_selection/spp_trait_analysis.RData")


spp_buffer <- sample(spp_traits, 32)


save(spp_buffer, file = "data/species_selection/spp_buffer.RData")



# second sample -----------------------------------------------------------

load("data/species_selection/spp_suitable_AC.RData")
load("data/species_selection/spp_buffer.RData")

spp_buffer_2 <- sample(spp_suitable_AC[! spp_suitable_AC %in% spp_buffer], 25)

save(spp_buffer_2, file = "data/species_selection/spp_buffer_2.RData")
