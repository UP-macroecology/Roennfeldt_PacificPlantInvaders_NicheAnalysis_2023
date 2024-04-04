glob <- scores_globclim
glob1 <- scores_clim_nat
sp <- scores_sp_nat
R <- 100
th.sp <- 0
extend.extent <- c(0, 0, 0, 0)
kernel.method <- "adehabitat"
th.env <- 0
geomask <- NULL

glob <- as.matrix(glob)
glob1 <- as.matrix(glob1)
sp <- as.matrix(sp)
l <- list()

# if scores in two dimensions (e.g. PCA)
xmin <- apply(glob, 2, min, na.rm = T)
xmax <- apply(glob, 2, max, na.rm = T)
ext <- c(xmin[1], xmax[1], xmin[2], xmax[2]) + extend.extent

glob1.dens <- ecospat.kd(x = glob1, ext = ext, method = kernel.method, th = th.env, R = R)
if (!is.null(geomask)) {
  terra::crs(geomask) <- NA
  glob1.dens <- terra::mask(glob1.dens, geomask, updatevalue = 0) # Geographical mask in the case if the analysis takes place in the geographical space
}
sp.dens <- ecospat.kd(
  x = sp, ext = ext, method = kernel.method, th = th.sp, R =R,
  env.mask = glob1.dens > 0
)

x <- seq(from = ext[1], to = ext[2], length.out = R)
y <- seq(from = ext[3], to = ext[4], length.out = R)
l$y <- y
Z <- glob1.dens * nrow(glob1) / terra::global(glob1.dens, "sum")$sum # rescale density to the number of occurrences in sp, ie. number of occurrence/pixel
z <- sp.dens * nrow(sp) / terra::global(sp.dens, "sum")$sum  # rescale density to the number of occurrences in sp, ie. number of occurrence/pixel
z.uncor <- z / terra::global(z, "max")$max
z.cor <- z / Z # correct for environment prevalence
z.cor[is.na(z.cor)] <- 0 # remove n/0 situations
z.cor <- z.cor / terra::global(z.cor, "max")$max