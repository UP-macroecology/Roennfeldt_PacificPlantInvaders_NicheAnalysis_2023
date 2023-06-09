library(ape) # to work with phylogenetic trees
library(tidyverse)

rm(list = ls())

# Species data ------------------------------------------------------------
# data set on pacific island invaders from: https://bdj.pensoft.net/article/67318/
species_names <- read.table("data/PaciFLora.txt", header = TRUE) %>%
  dplyr::select(Species) %>%
  arrange(Species) %>%
  distinct() %>%
  drop_na() %>%
  rename(species_orig = Species) %>%
  mutate(species_id = row_number(), .before = "species_orig") %>%
  mutate(species_changed = species_orig) %>% # duplicate species column twice
  mutate(species_no_x = species_orig)

# 314 Arten aus der ursprünglichen Liste kommen nicht mehr in der neuen vor, dafür 1504 neue. 
# das kann auch damit zu tun haben, dass die Namen nochmal vereinheitlicht wurden



# reformat hybrids --------------------------------------------------------
species_names <- species_names[c(121:123),]


for (i in 1:nrow(species_names)){
  
  print(i)
  print(species_names[i,"species_orig"])

  if (grepl("_x", species_names[i,"species_orig"])) {
    
    # move the "x" from the end of the string to between genus and species in the second column
    species_names[i,"species_changed"] <- gsub(x = species_names[i,"species_changed"], pattern = "_x", replacement = "")
    species_names[i,"species_changed"] <- gsub(x = species_names[i,"species_changed"], pattern = " ", replacement = " x ")
    
    # remove "x" completely from the second column
    species_names[i,"species_no_x"] <- gsub(x = species_names[i,"species_no_x"], pattern = "_x", replacement = "")
  } # end of if 
}


save(species_names, file = "data/initial_species_list.RData")



# phylogeny ---------------------------------------------------------------

tree_pacific <- read.tree("data/Phylogeny.tre", keep.multi = TRUE)
tr <- tree_pacific[[1]]
tree_pacific
names(tree_pacific)

windows()
ape::plot.phylo(tree_pacific[[1]], type = "fan", no.margin = TRUE)
# ape::plot.phylo(tree_pacific[[1]], type = "fan", show.tip.label = FALSE, no.margin = TRUE) # no labels

