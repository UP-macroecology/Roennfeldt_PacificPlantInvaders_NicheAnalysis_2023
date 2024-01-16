
library(ape)
library(phangorn)
library(dplyr)

rm(list = ls())


# load data ---------------------------------------------------------------

# final species selection
load("data/spp_suitable_after_thinning.RData")

# master results table to identify wich regions each species has been introduced to
load("results/ecospat/master_results.RData")
# convert regions column to character for later use
master_results$region <- as.character(master_results$region)

# phylogeny
phylo_pacific <- read.tree("data/Phylogeny.tre")



# prepare species selection -----------------------------------------------

spp_phylo <- phylo_pacific[["tip.label"]]

spp_not_needed <- setdiff(spp_phylo, sub(' ','_',spp_suitable))

# identify species not included in the PaciFLora phylogeny
spp_no_match <- spp_suitable[!sub(' ','_',spp_suitable) %in% spp_phylo]  # -> 39 out of the initial species are not part of the phylogeny
# final species list included in the tree
spp_final <- setdiff(spp_suitable, spp_no_match)


# prepare phylogeny -------------------------------------------------------

# drop species tips that are not part of the final species selection
tree_pac <- drop.tip(phylo_pacific, spp_not_needed)
# plot(tree_pac)

# object to be moified within the loop
tree_pac_mod <- tree_pac

# begin loop over species
for (spp in spp_final) {
  
  # 1. identify which regions the species has been intoruced to
  regions <- master_results %>%
    filter(species == spp) %>%
    select(region) %>%
    pull()
  
  
  # 2. create vector containing the new tip labels based on the species name and region
  new_tips <- NULL
  
  for (region in regions) {
    
    new_label <- paste0(sub(" ", "_",spp), "_", region) 
    new_tips <- c(new_tips, new_label)
    
  } # end of for loop over regions
  
  # 3. add "polytomy" with species - region names and replace original tip
  
  # identify at which edge the species is located in the phylogeny
  # get edge table 
  tree_edges <- tree_pac_mod[["edge"]]
  
  # get number of edge
  nr_edge <- as.numeric(which.edge(tree_pac_mod, sub(" ","_",spp)))
  # use it to get the node
  nr_node <- as.numeric(tree_edges[nr_edge])
  
  # add new tips at the same location as the original species label
  tree_pac_mod <- add.tips(tree_pac_mod, new_tips, where = nr_node)
  
  # remove individual tip with species name
  tree_pac_mod <- drop.tip(tree_pac_mod, sub(" ","_",spp))
  
} # end of for loop over species

# plot(tree_pac_mod)

# make sure that the tree has as many tips as there are unique species - region combinations
nrow(unique(subset(master_results, species %in% spp_final)[,c("species", "region")])) # 1465
length(tree_pac_mod[["tip.label"]]) # 1465 

# save modified tree to be used in the trait analysis
write.tree(tree_pac_mod, file = "data/Phylogeny_mod.tre")

