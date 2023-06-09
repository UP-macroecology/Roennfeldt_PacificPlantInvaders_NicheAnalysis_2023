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
  rename(species = Species)


length(unique(species_names$Island)) # 482
length(unique(species_names$IslandGroup)) # 50

# compare to current species list
load("data/specs_all.RData")

t <- setdiff(specs_all, PaciFlora)
t2 <- setdiff(PaciFlora, specs_all)

# 314 Arten aus der ursprünglichen Liste kommen nicht mehr in der neuen vor, dafür 1504 neue. 
# das kann auch damit zu tun haben, dass die Namen nochmal vereinheitlicht wurden


# Paciflora names for hybrids are in a format that causes errors when running the wcvp_distribution() function
hybrids <- species_names %>%
  filter(grepl('_x', species)) %>%
  pull() 

# hybrids_corrected <- data.frame(PaciFlora = str_replace(hybrids, "_x", ""))
hybrids_corrected <- hybrids %>%
  str_replace("_x", "") %>%
  str_replace(" ", " x ")



# remove hybrids with the old format and add the corrected format
species_all <- species_names %>%
  filter(!grepl('_x', species)) %>% # remove old format
  rbind(data.frame(species = hybrids_corrected)) %>% # add corrected format
  arrange(species) %>% # sort alphabetically
  rename("species_no_x" = "species") # rename column


species_all <- cbind(species_names$species , species_all) %>%
  rename("species" = "species_names$species")

save(species_all, file = "data/initial_species_list.RData")



# phylogeny ---------------------------------------------------------------

tree_pacific <- read.tree("data/Phylogeny.tre", keep.multi = TRUE)
tr <- tree_pacific[[1]]
tree_pacific
names(tree_pacific)

windows()
ape::plot.phylo(tree_pacific[[1]], type = "fan", no.margin = TRUE)
# ape::plot.phylo(tree_pacific[[1]], type = "fan", show.tip.label = FALSE, no.margin = TRUE) # no labels

