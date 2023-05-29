library(ape) # to work with phylogenetic trees

rm(list = ls())

# Species data ------------------------------------------------------------


# data set on pacific island invaders from: https://bdj.pensoft.net/article/67318/
PaciFlora <- read.table("data/PaciFLora.txt", header = TRUE) %>%
  dplyr::select(Species) %>%
  distinct() %>%
  drop_na() %>%
  pull(Species)


length(unique(PaciFlora$Island)) # 482
length(unique(PaciFlora$IslandGroup)) # 50

# compare to current species list
load("data/specs_all.RData")

t <- setdiff(specs_all, PaciFlora)
t2 <- setdiff(PaciFlora, specs_all)

# 314 Arten aus der ursprünglichen Liste kommen nicht mehr in der neuen vor, dafür 1504 neue. 
# das kann auch damit zu tun haben, dass die Namen nochmal vereinheitlicht wurden



# phylogeny ---------------------------------------------------------------

tree_pacific <- read.tree("data/Phylogeny.tre", keep.multi = TRUE)
tr <- tree_pacific[[1]]
tree_pacific
names(tree_pacific)

windows()
ape::plot.phylo(tree_pacific[[1]], type = "fan", no.margin = TRUE)
# ape::plot.phylo(tree_pacific[[1]], type = "fan", show.tip.label = FALSE, no.margin = TRUE) # no labels

