library(ape)
library(dplyr)
library(phytools)
library(tidytree)
library(TreeTools)

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

# only work with a sample to test the code:
spp_suitable <- sample(spp_suitable, 3)
spp_phylo <- phylo_pacific[["tip.label"]]

spp_not_needed <- setdiff(spp_phylo, sub(' ','_',spp_suitable))

# identify species not included in the PaciFLora phylogeny
spp_no_match <- spp_suitable[!sub(' ','_',spp_suitable) %in% spp_phylo]  # -> 39 out of the initial species are not part of the phylogeny
# final species list included in the tree
spp_final <- setdiff(spp_suitable, spp_no_match)


# prepare phylogeny -------------------------------------------------------

# drop species tips that are not part of the final species selection
tree_pac <- drop.tip(phylo_pacific, spp_not_needed)

plot(tree_pac, type = "c", main = "Original Tree")
edgelabels()
nodelabels()

rm(phylo_pacific)


# object to be modified within the loop
tree_pac_mod <- tree_pac

# spp <- spp_final[1]

for (spp in spp_final) {
  
  print(spp)
  
  # 1. identify which regions the species has been introduced to
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

  # 3. add new tips

  # !!only do this with the first species to keep distance the same for all cherries
  if (spp == spp_final[1]) {
    # identify at which edge the species is located in the phylogeny
    edge_to_split <- as.numeric(which.edge(tree_pac_mod, sub(" ","_",spp)))

    # get edge length to determine the poistion at which to add the new branch and how long it is supposed to be
    initial_edge_length <- tree_pac[["edge.length"]][edge_to_split]
    # edge length below the new branch
    e_b <- initial_edge_length / 2
    # edge length of new branch
    e_l <- initial_edge_length / 2
  }


  for (i in 1:length(new_tips)) {
    
    print(new_tips[i])
    if (i == 1) {
      
      print("yes")
      tree_pac_mod <- bind.tip(tree_pac_mod,
                               tip.label = new_tips[i],
                               edge.length = e_l,
                               where = which(tree_pac_mod$tip.label == sub(" ", "_",spp)),
                               position = e_b)


      # plot(tree_pac_mod, type = "c", main = paste0("Modified Tree (i = ",i,")"))

    }else{ 
      print("no")

      tree_pac_mod <- bind.tip(tree_pac_mod,
                               tip.label = new_tips[i],
                               edge.length = e_l,
                               where = which(tree_pac_mod$tip.label == sub(" ", "_",spp)),
                               position = 7)

      # plot(tree_pac_mod, type = "c", main = paste0("Modified Tree (i = ",i,")"))
      # edgelabels()

      # collaps internal edge to create polytomy
      # !!this manipulates edge lenghts, which will have to be adjusted in the next step
      tree_pac_mod <- CollapseEdge(tree_pac_mod, edge = min(which.edge(tree_pac_mod, c(new_tips[i], sub(" ", "_",spp)))) - 1)

      # plot(tree_pac_mod, type = "c", main = "Collapsed Tree")

      # readjust edge lenghts
      tree_pac_mod$edge.length[which.edge(tree_pac_mod, new_tips[i])] <- e_l
      # plot(tree_pac_mod, type = "c", main = "Cherry Tree")

    } # end of if else i == 1

  } # end of for loop over tips



  # 4. drop the "original" tip with the species name
  tree_pac_mod <- drop.tip(tree_pac_mod, tip = sub(" ", "_",spp))

  # plot(tree_pac_mod, type = "c", main = "Modified Tree (dropped tip)")
  # edgelabels()

} # end of for loop over species


# make sure that the tree has as many tips as there are unique species - region combinations
nrow(unique(subset(master_results, species %in% spp_final)[,c("species", "region")])) # 1465
length(tree_pac_mod[["tip.label"]]) # 1465 

# save modified tree to be used in the trait analysis
write.tree(tree_pac_mod, file = "data/Phylogeny_mod.tre")








# testing: 

t <- tree_pac_mod