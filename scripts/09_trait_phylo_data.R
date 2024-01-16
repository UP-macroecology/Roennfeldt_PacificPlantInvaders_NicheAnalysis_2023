

# preamble ----------------------------------------------------------------

library(ape)
library(dplyr)
library(stringr)

rm(list = ls())


# required data -----------------------------------------------------------

# load traits
load("data/trait_analysis/trait_data_all.RData")

# load phylogeny
phylo_pacific <- read.tree("data/Phylogeny.tre", keep.multi = TRUE)



# subset phylogeny for selected species -----------------------------------

no_match <- trait_data_all$species[!sub(' ','_',trait_data_all$species) %in% phylo_pacific[[1]][["tip.label"]]]

phylo_subset <- drop.tip(phylo_pacific, phylo_pacific$tip.label[!phylo_pacific$tip.label %in% sub(' ','_',trait_data_all$species)])
save(phylo_subset, file = "data/trait_analysis/phylo_subset.RData")

