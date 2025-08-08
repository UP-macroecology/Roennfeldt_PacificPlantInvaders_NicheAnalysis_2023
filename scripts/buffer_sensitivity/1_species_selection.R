

#' ---------------------------
#
# Purpose of script: Selecting 49 sample species (~15 % of the study species) to test sensitivity to the buffer size 
# Author: Anna Rönnfeldt
# Date Created: 2025-06-10
# Email: roennfeldt@uni-potsdam.de
#
# Notes: The buffer size refers to the spatial buffer around the occurrence points within which pseudo-absences are sampled. 
# The species selection will be base on a random subset of the species included in the main analysis.
#
#' ---------------------------


# load species list
load("data/species_selection/spp_suitable_AC.RData")

# random subset of 49 species
spp_buffer <- sample(spp_suitable_AC, 49)


save(spp_buffer, file = "data/species_selection/spp_buffer.RData")


