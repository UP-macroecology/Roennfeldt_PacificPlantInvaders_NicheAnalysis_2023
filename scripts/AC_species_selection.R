
load("data/spp_suitable_after_thinning.RData") # 328
load("data/spp_suitable_AC.RData")

# compare the species selections against each other
# and select sample sepcies from there
spp <- setdiff(spp_suitable, spp_suitable_AC)[1]


library(ade4)
library(dplyr)
library(ecospat)
library(purrr) # required for simplify
library(stringr) #required for str_split
library(terra)


path_data <- "X:/roennfeldt/Projects/PhD_C1/data/"



# get the regions in which this species occurs as introduced species
regions <- list.files(path = "data/final_input_intr/", pattern = paste0(spp,".RData")) %>% 
  str_remove(".RData") %>% 
  str_split(pattern = "_") %>%
  map(~ .x[[3]]) %>%
  simplify() %>%
  unique()

# define example region
region <- regions[1]

# load intr and nat occurrences of that species-region combination
load(paste0("data/final_input_nat/input_nat_",spp,".RData")) 
load(paste0("data/final_input_intr/input_intr_",region,"_",spp,".RData"))

# rename object to a shorter version and remove original
input_intr <- data_prep_intr
input_nat <- data_prep_nat
rm(data_prep_nat, data_prep_intr)

nrow(subset(input_intr, present == 1)) >= 20


# PCA environment ---------------------------------------------------------

# define the regional PCA environment for this species
pca_env_regional <- dudi.pca(rbind(input_nat,input_intr)[,7:25], scannf = FALSE, nf = 2)

# predict scores on the axis
# PCA scores for the whole study area
scores_globclim <- pca_env_regional$li
# PCA scores for the species' native distribution
scores_sp_nat <- suprow(pca_env_regional, input_nat[which(input_nat[,"present"] == 1),7:25])$li
# PCA scores for the species' invasive distribution
scores_sp_intr <- suprow(pca_env_regional, input_intr[which(input_intr[,"present"] == 1),7:25])$li
# PCA scores for the whole native study area
scores_clim_nat <- suprow(pca_env_regional,input_nat[,7:25])$li
# PCA scores for the whole invaded study area
scores_clim_intr <- suprow(pca_env_regional,input_intr[,7:25])$li

# calculate occurrence densities grid -------------------------------------

# gridding the native niche
grid_clim_nat  <- ecospat.grid.clim.dyn(glob = scores_globclim,
                                        glob1 = scores_clim_nat,
                                        sp = scores_sp_nat,
                                        R = 100,
                                        th.sp = 0)

# gridding the invasive niche
grid_clim_intr <- ecospat.grid.clim.dyn(glob = scores_globclim,
                                        glob1 = scores_clim_intr,
                                        sp = scores_sp_intr, 
                                        R = 100,
                                        th.sp = 0)



quant = 0
col.unf = "lightgoldenrod1"
col.exp = "darkseagreen3"
col.stab = "lightblue2"
colZ1 = "black"
colZ2 = "red"


z1 <- grid_clim_nat
z2 <- grid_clim_intr

values(z1$Z)[which(values(z1$Z) > 0)] <- 1
values(z2$Z)[which(values(z2$Z) > 0)] <- 1

rast_analogue <- z1$Z + z2$Z
values(rast_analogue)[which(values(rast_analogue) < 2)] <- 0

rast_nat <- z1$Z
rast_intr <- z2$Z

terra::plot(rast_intr)
terra::plot(rast_nat)

# rast_analogue <- terra::crop(rast_intr, rast_nat)

terra::plot(rast_analogue)


name.axis1 = "Axis 1"
name.axis2 = "Axis 2"
col_category<-c("#FFFFFF",col.exp,col.unf,col.stab)[1+(sort(terra::unique(2*z1$w+z2$w)[,1]))]

terra::plot(2*z1$w+z2$w,col=col_category, legend=FALSE)


terra::contour(
  rast_nat, add = TRUE, levels = quantile(z1$Z[z1$Z > 0], c(0, quant)),
  drawlabels = FALSE, lty = c(1, 2), col = colZ1
)

# outline of env available in the intr range
terra::contour(
  rast_intr, add = TRUE, levels = quantile(z2$Z[z2$Z > 0], c(0, quant)),
  drawlabels = FALSE, lty = c(1, 2), col = colZ2
)

# outline of analogue env conditions
terra::contour(
  rast_analogue, add = TRUE, levels = quantile(rast_analogue[rast_analogue> 0], c(0, quant)),
  drawlabels = FALSE, lty = c(1, 2), col = "purple"
)



# intr occurrence points
terra::points(grid_clim_intr[["sp"]],  col = "darkred")

# nat occurrence points
terra::points(grid_clim_nat[["sp"]],  col = "black")




# Plan: 

# follow intersect approach (even though it keeps occs that are directly outside contour line)
# compare to original AC spp selection
# stick ith it if it is identical















# get intr points

rast_AC <- terra::subset(rast_analogue, rast_analogue = 2) 

intr_points <- vect(grid_clim_intr[["sp"]])

# extract cell values from the analogue raster
intr_IDs_AC <- terra::extract(rast_analogue, intr_points) %>% 
  filter(lyr.1 == 2) %>%  # only select point that lie where the raster cells have value 2 (= analogue climate space)
  pull(ID)


t <- subset(intr_points, intr_IDs_AC, drop=FALSE, NSE=FALSE)


t <- intr

outline_AC <- as.contour(rast_analogue)

nrow(grid_clim_intr[["sp"]])

t <- vect(grid_clim_intr[["sp"]])

t_AC <- terra::crop(t, outline_AC)

x <- vect(grid_clim_nat[["sp"]])
x_AC <- terra::crop(x, outline_AC)
terra::points(t_AC,  col = "pink")
terra::points(x_AC,  col = "pink")

terra::plot(rast_analogue)

t <- extract(rast_analogue, t)





