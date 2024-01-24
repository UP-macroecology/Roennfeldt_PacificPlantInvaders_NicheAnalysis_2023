library(ade4)
library(dplyr)
library(ecospat)
library(purrr) # required for simplify
library(stringr) #required for str_split
library(terra)

# set path and load data --------------------------------------------------
path_user <- "Y:/roennfeldt/C1/data/"

# load species list
load("data/spp_suitable_after_thinning.RData")

# define random example species
spp <- sample(spp_suitable, 1)

spp_index <- 1

# get the regions in which this species occurs as introduced species
regions <- list.files(path = paste0(path_user, "final_input_intr/"), pattern = paste0(spp[spp_index],".RData")) %>% #TODO
  str_remove(".RData") %>% 
  str_split(pattern = "_") %>%
  map(~ .x[[3]]) %>%
  simplify() %>%
  unique()

# define random example region
region <- sample(regions, 1)

# load intr and nat occurrences of that species-region combination
load(paste0(path_user, "final_input_nat/input_nat_",spp[spp_index],".RData")) 
load(paste0(path_user, "final_input_intr/input_intr_",region,"_",spp[spp_index],".RData"))


# rename object to a shorter version and remove original
input_intr <- data_prep_intr
input_nat <- data_prep_nat
rm(data_prep_nat, data_prep_intr)

nrow(subset(input_intr, present == 1)) >= 20




# PCA environment ---------------------------------------------------------

# define the regional PCA environment for this species
pca_env_regional <- dudi.pca(rbind(input_nat,input_intr)[,7:25], scannf = FALSE, nf = 2)

#TODO: check whether this line is still required
# save the regional PCA for later
# save(pca_env_regional, file = paste0(path_imp, "output/PCA/regional_pca_",region,"_",spp[spp_index],".RData")) #TODO

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

# matrix_nat <- as.matrix(z1$zmatrix_nat <- as.matrix(z1$Z))
# matrix_intr <- as.matrix(z2$Z)
# 
# # change all values > 0 to 1
# matrix_nat[matrix_nat[] > quant] <- 1
# matrix_intr[matrix_intr[] > quant] <- 1
# 
# matrix_inter <- matrix_nat + matrix_intr
# matrix_inter[matrix_inter[] != 2] <- 0
# 
# 
# # transform this into a raster
# t <- rast(matrix_inter, res = c(100,100))



terra::plot(rast_intr)
terra::plot(rast_nat)


  # required objects:
# grid_clim_nat and grid_clim_intr 
# scores_sp_nat and scores_sp_intr



name.axis1 = "Axis 1"
name.axis2 = "Axis 2"
col_category<-c("#FFFFFF",col.exp,col.unf,col.stab)[1+(sort(terra::unique(2*z1$w+z2$w)[,1]))]



rast_nat <- z1$Z
rast_intr <- z2$Z

windows()
rast_analogue <- terra::crop(rast_intr, rast_nat)

plot(rast_analogue)


terra::plot(2*z1$w+z2$w,col=col_category, legend=FALSE)

# outline of env available in the native range
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
