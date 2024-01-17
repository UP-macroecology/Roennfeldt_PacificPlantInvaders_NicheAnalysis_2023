
# required objects:
# grid_clim_nat and grid_clim_intr 
# scores_sp_nat and scores_sp_intr


quant = 0
col.unf = "lightgoldenrod1"
col.exp = "darkseagreen3"
col.stab = "lightblue2"
colZ1 = "black"
colZ2 = "red"

name.axis1 = "Axis 1"
name.axis2 = "Axis 2"
col_category<-c("#FFFFFF",col.exp,col.unf,col.stab)[1+(sort(terra::unique(2*z1$w+z2$w)[,1]))]



rast_nat <- z1$Z
rast_intr <- z2$Z

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
