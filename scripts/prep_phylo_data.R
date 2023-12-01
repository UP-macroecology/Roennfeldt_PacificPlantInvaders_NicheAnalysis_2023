

# preamble ----------------------------------------------------------------

library(ape)
library(dplyr)
library(stringr)

rm(list = ls())


# required data -----------------------------------------------------------

# load traits
load("data/trait_analysis/trait_example_df_spec.RData")

# load phylogeny
phylo_pacific <- read.tree("data/Phylogeny.tre", keep.multi = TRUE)



# subset phylogeny for selected species -----------------------------------

no_match <- trait_example_df$species[!sub(' ','_',trait_example_df$species) %in% phylo_pacific[[1]][["tip.label"]]]

phylo_subset <- drop.tip(phylo_pacific, phylo_pacific$tip.label[!phylo_pacific$tip.label %in% sub(' ','_',trait_example_df$species)])
save(phylo_subset, file = "data/trait_analysis/phylo_subset.RData")

