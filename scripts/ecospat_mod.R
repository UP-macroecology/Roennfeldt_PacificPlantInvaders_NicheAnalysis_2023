ecospat.niche.similarity.test.mod <- function(z1, z2, rep, intersection = NA, rand.type = 1, ncores = 1,
                                              overlap.alternative = "higher",
                                              expansion.alternative = "lower",
                                              stability.alternative = "higher",
                                              unfilling.alternative = "lower"
) {
  if (isFALSE(overlap.alternative %in% c("higher","lower","different"))){
    stop("Please choose an alternative hypothesis (higher,lower or different) for the overlap")
  }
  if (isFALSE(expansion.alternative %in% c("higher","lower","different"))){
    stop("Please choose an alternative hypothesis (higher,lower or different) for the expansion")
  }
  if (isFALSE(stability.alternative %in% c("higher","lower","different"))){
    stop("Please choose an alternative hypothesis (higher,lower or difefrent) for the stability")
  }
  if (isFALSE(stability.alternative %in% c("higher","lower","different"))){
    stop("Please choose an alternative hypothesis (higher,lower or different) for the unfilling")
  }
  
  R <- length(z1$x)
  l <- list()
  obs.o <- c(ecospat.niche.overlap(z1, z2, cor = TRUE),  #observed niche overlap
             ecospat.niche.dyn.index.mod(z1, z2, intersection = intersection)$dynamic.index.w) # dynamic indices between random and observed niches
  z1$z.uncor <- as.matrix(z1$z.uncor,wide=TRUE)
  z1$Z <- as.matrix(z1$Z,wide=TRUE)
  z1$z <- as.matrix(z1$z,wide=TRUE)
  z2$z.uncor <- as.matrix(z2$z.uncor,wide=TRUE)
  z2$Z <- as.matrix(z2$Z,wide=TRUE)
  z2$z <- as.matrix(z2$z,wide=TRUE)
  
  if (ncores == 1) {
    sim.o <- as.data.frame(matrix(unlist(lapply(1:rep, overlap.sim.gen.mod, z1, z2, rand.type = rand.type)),
                                  byrow = TRUE, ncol = 5))  #simulate random overlap  
  } else {
    cl <- parallel::makeCluster(ncores)  #open a cluster for parallelization
    invisible(parallel::clusterEvalQ(cl,library("ecospat")))  #import the internal function into the cluster
    sim.o <- as.data.frame(matrix(unlist(parallel::parLapply(cl, 1:rep, overlap.sim.gen.mod, z1, z2, rand.type = rand.type)),
                                  byrow = TRUE, ncol = 5))  #simulate random overlap
    parallel::stopCluster(cl)  #shutdown the cluster
  }
  colnames(sim.o) <- c("D", "I", "expansion", "stability", "unfilling")
  l$sim <- sim.o  # storage
  l$obs <- obs.o  # storage
  
  l$p.D <- p.val.gen(sim.o$D,obs.o$D,overlap.alternative)  
  l$p.I <- p.val.gen(sim.o$I,obs.o$I,overlap.alternative) 
  l$p.expansion <-p.val.gen(sim.o$expansion,obs.o$expansion,expansion.alternative)
  l$p.stability <-p.val.gen(sim.o$stability,obs.o$stability,stability.alternative)
  l$p.unfilling <-p.val.gen(sim.o$unfilling,obs.o$unfilling,unfilling.alternative)
  
  return(l)
}

#### internal function to generate random distribution following the niche similarity approach
overlap.sim.gen.mod <- function(repi, z1, z2, rand.type = rand.type, intersection = NA) {
  R1 <- length(z1$x)
  R2 <- length(z2$x)
  if (is.null(z1$y) & is.null(z2$y)) {
    if (rand.type == 1) {
      # if rand.type = 1, both z1 and z2 are randomly shifted, if rand.type =2, only z2 is randomly
      # shifted
      center.z1 <- which(z1$z.uncor == 1)  # define the centroid of the observed niche
      Z1 <- z1$Z/max(z1$Z)
      rand.center.z1 <- sample(1:R1, size = 1, replace = FALSE, prob = Z1)  # randomly (weighted by environment prevalence) define the new centroid for the niche
      xshift.z1 <- rand.center.z1 - center.z1  # shift on x axis
      z1.sim <- z1
      z1.sim$z <- rep(0, R1)  # set intial densities to 0
      for (i in 1:length(z1$x)) {
        i.trans.z1 <- i + xshift.z1
        if (i.trans.z1 > R1 | i.trans.z1 < 0)
          (next)()  # densities falling out of the env space are not considered
        z1.sim$z[i.trans.z1] <- z1$z[i]  # shift of pixels
      }
      z1.sim$z <- (z1$Z != 0) * 1 * z1.sim$z  # remove densities out of existing environments
      z1.sim$z.cor <- (z1.sim$z/z1$Z)/max((z1.sim$z/z1$Z), na.rm = TRUE)  #transform densities into occupancies
      z1.sim$z.cor[which(is.na(z1.sim$z.cor))] <- 0
      z1.sim$z.uncor <- z1.sim$z/max(z1.sim$z, na.rm = TRUE)
      z1.sim$z.uncor[which(is.na(z1.sim$z.uncor))] <- 0
      z1.sim$w<-(z1.sim$z.uncor>0)*1
    }
    
    center.z2 <- which(z2$z.uncor == 1)  # define the centroid of the observed niche
    Z2 <- z2$Z/max(z2$Z)
    rand.center.z2 <- sample(1:R2, size = 1, replace = FALSE, prob = Z2)  # randomly (weighted by environment prevalence) define the new centroid for the niche
    
    xshift.z2 <- rand.center.z2 - center.z2  # shift on x axis
    z2.sim <- z2
    z2.sim$z <- rep(0, R2)  # set intial densities to 0
    for (i in 1:length(z2$x)) {
      i.trans.z2 <- i + xshift.z2
      if (i.trans.z2 > R2 | i.trans.z2 < 0)
        (next)()  # densities falling out of the env space are not considered
      z2.sim$z[i.trans.z2] <- z2$z[i]  # shift of pixels
    }
    z2.sim$z <- (z2$Z != 0) * 1 * z2.sim$z  # remove densities out of existing environments
    z2.sim$z.cor <- rast(z2.sim$z/z2$Z)/max((z2.sim$z/z2$Z), na.rm = TRUE)  #transform densities into occupancies
    z2.sim$z.cor <- subst(z2.sim$z.cor, NA, 0)
    # z2.sim$z.cor[which(is.na(z2.sim$z.cor))] <- 0
    z2.sim$z.uncor <- z2.sim$z/max(z2.sim$z, na.rm = TRUE)
    z2.sim$z.uncor[which(is.na(z2.sim$z.uncor))] <- 0
    z2.sim$w<-(z2.sim$z.uncor>0)*1
    
  }
  
  if (!is.null(z2$y) & !is.null(z1$y)) {
    if (rand.type == 1) {
      # if rand.type = 1, both z1 and z2 are randomly shifted, if rand.type =2, only z2 is randomly
      # shifted
      centroid.z1 <- which(z1$z.uncor == 1, arr.ind = TRUE)[1, ]  # define the centroid of the observed niche
      Z1 <- z1$Z/max(z1$Z)
      rand.centroids.z1 <- which(Z1 > 0, arr.ind = TRUE)  # all pixels with existing environments in the study area
      weight.z1 <- Z1[Z1 > 0]
      rand.centroid.z1 <- rand.centroids.z1[sample(1:nrow(rand.centroids.z1), size = 1, replace = FALSE,
                                                   prob = weight.z1), ]  # randomly (weighted by environment prevalence) define the new centroid for the niche
      xshift.z1 <- rand.centroid.z1[1] - centroid.z1[1]  # shift on x axis
      yshift.z1 <- rand.centroid.z1[2] - centroid.z1[2]  # shift on y axis
      z1.sim <- z1
      z1.sim$z <- matrix(rep(0, R1 * R1), ncol = R1, nrow = R1)  # set intial densities to 0
      for (i in 1:R1) {
        for (j in 1:R1) {
          i.trans.z1 <- i + xshift.z1
          j.trans.z1 <- j + yshift.z1
          if (i.trans.z1 > R1 | i.trans.z1 < 0 | j.trans.z1 > R1 | j.trans.z1 < 0)
            (next)()  # densities falling out of the env space are not considered
          #if (j.trans.z1 > R1 | j.trans.z1 < 0)
          #  (next)()
          z1.sim$z[i.trans.z1, j.trans.z1] <- z1$z[i, j]  # shift of pixels
        }
      }
      z1.sim$z <- (z1$Z != 0) * 1 * z1.sim$z  # remove densities out of existing environments
      z1.sim$z.cor <- (z1.sim$z/z1$Z)/max((z1.sim$z/z1$Z), na.rm = TRUE)  #transform densities into occupancies
      z1.sim$z.cor[which(is.na(z1.sim$z.cor))] <- 0
      z1.sim$z.uncor <- z1.sim$z/max(z1.sim$z, na.rm = TRUE)
      z1.sim$z.uncor[which(is.na(z1.sim$z.uncor))] <- 0
      z1.sim$w <- (z1.sim$z.uncor>0)*1 # niche envelope
      
    }
    centroid.z2 <- which(z2$z.uncor == 1, arr.ind = TRUE)[1, ]  # define the centroid of the observed niche
    Z2 <- z2$Z/max(z2$Z)
    rand.centroids.z2 <- which(Z2 > 0, arr.ind = TRUE)  # all pixels with existing environments in the study area
    weight.z2 <- Z2[Z2 > 0]
    rand.centroid.z2 <- rand.centroids.z2[sample(1:nrow(rand.centroids.z2), size = 1, replace = FALSE,
                                                 prob = weight.z2), ]  # randomly (weighted by environment prevalence) define the new centroid for the niche
    xshift.z2 <- rand.centroid.z2[1] - centroid.z2[1]  # shift on x axis
    yshift.z2 <- rand.centroid.z2[2] - centroid.z2[2]  # shift on y axis
    z2.sim <- z2
    z2.sim$z <- matrix(rep(0, R2 * R2), ncol = R2, nrow = R2)  # set intial densities to 0
    for (i in 1:R2) {
      for (j in 1:R2) {
        i.trans.z2 <- i + xshift.z2
        j.trans.z2 <- j + yshift.z2
        #if (i.trans.z2 > R2 | i.trans.z2 < 0)
        if (i.trans.z2 > R2 | i.trans.z2 < 0 | j.trans.z2 > R2 | j.trans.z2 < 0)
          (next)()  # densities falling out of the env space are not considered
        #if (j.trans.z2 > R2 | j.trans.z2 < 0)
        #  (next)()
        z2.sim$z[i.trans.z2, j.trans.z2] <- z2$z[i, j]  # shift of pixels
      }
    }
    z2.sim$z <- (z2$Z != 0) * 1 * z2.sim$z  # remove densities out of existing environments
    z2.sim$z.cor <- rast(z2.sim$z/z2$Z)/max((z2.sim$z/z2$Z), na.rm = TRUE)  #transform densities into occupancies
    # z2.sim$z.cor[which(is.na(z2.sim$z.cor))] <- 0
    z2.sim$z.cor <- subst(z2.sim$z.cor, NA, 0)
    z2.sim$z.uncor <- z2.sim$z/max(z2.sim$z, na.rm = TRUE)
    z2.sim$z.uncor[which(is.na(z2.sim$z.uncor))] <- 0
    z2.sim$w <- (z2.sim$z.uncor>0)*1 # niche envelope
  }
  
  if (rand.type == 1) {
    o.i <- ecospat.niche.overlap(z1.sim, z2.sim, cor = TRUE)
    sim.dyn<- ecospat.niche.dyn.index.mod(z1.sim, z2.sim, intersection = intersection)$dynamic.index.w         
  }
  if (rand.type == 2) {
    o.i <- ecospat.niche.overlap(z1, z2.sim, cor = TRUE)
    sim.dyn <- ecospat.niche.dyn.index.mod(z1, z2.sim, intersection = intersection)$dynamic.index.w
  }  # overlap between random and observed niches
  sim.o.D <- o.i$D  # storage of overlaps
  names(sim.o.D) <- "o.D"
  sim.o.I <- o.i$I  # storage of overlaps
  names(sim.o.I) <- "o.I"
  sim.exp <- sim.dyn[1] # storage of the dynamic indices
  sim.sta <- sim.dyn[2] # storage of the dynamic indices
  sim.unf <- sim.dyn[3] # storage of the dynamic indices
  
  return(c(sim.o.D, sim.o.I, sim.exp,sim.sta,sim.unf))
}



ecospat.niche.dyn.index.mod <- function(z1, z2, intersection = NA) {
  rotate <- function(x) t(apply(x, 2, rev))
  # the following 4 lines originally used the as.matrix function
  w1 <- as.vector(z1$w) # native environmental distribution mask
  w2 <- as.vector(z2$w) # invaded environmental distribution mask
  glob1 <- as.vector(z1$Z) # Native environmental extent densities
  glob2 <- as.vector(z2$Z) # Invaded environmental extent densities
  if (!is.na(intersection)) {
    if (intersection == 0) {
      glob1[glob1 > 0] <- 1 # Native environmental extent mask
      glob2[glob2 > 0] <- 1 # Invaded environmental extent mask
    } else {
      quant.val <- quantile(glob1[glob1 > 0], probs = seq(0, 1, intersection))[2] # threshold do delimit native environmental mask
      glob1[glob1[] <= quant.val] <- 0
      glob1[glob1[] > quant.val] <- 1 #  native environmental mask
      quant.val <- quantile(glob2[glob2 > 0], probs = seq(0, 1, intersection))[2] # threshold do delimit invaded environmental mask
      glob2[glob2[] <= quant.val] <- 0
      glob2[glob2[] > quant.val] <- 1 #  invaded environmental mask
    }
    
    glob <- glob1 * glob2 # delimitation of the intersection between the native and invaded extents
    w1 <- w1 * glob # Environmental native distribution at the intersection
    w2 <- w2 * glob # Environmental invasive distribution at the intersection
  }
  z.exp.cat <- (w1 + 2 * w2) / 2
  z.exp.cat[z.exp.cat != 1] <- 0 # categorizing expansion pixels
  z.stable.cat <- (w1 + 2 * w2) / 3
  z.stable.cat[z.stable.cat != 1] <- 0 # categorizing stable pixels
  z.res.cat <- w1 + 2 * w2
  z.res.cat[z.res.cat != 1] <- 0 # categorizing restriction pixels
  obs.exp <- as.vector(z2$z.uncor) * as.vector(z.exp.cat) # density correction
  obs.stab <- as.vector(z2$z.uncor) * as.vector(z.stable.cat) # density correction
  obs.res <- as.vector(z1$z.uncor) * as.vector(z.res.cat) # density correction
  
  dyn <- as.matrix((-1 * z.exp.cat) + (2 * z.stable.cat) + z.res.cat)
  # if (ncol(w1) == 2) {
  #   dyn <- terra::rast(dyn)
  # } # draw matrix with 3 categories of niche dynamic
  expansion.index.w <- sum(obs.exp) / sum(obs.stab + obs.exp) # expansion
  stability.index.w <- sum(obs.stab) / sum(obs.stab + obs.exp) # stability
  restriction.index.w <- sum(obs.res) / sum(obs.res + (z.stable.cat * as.matrix(z1$z.uncor))) # unfilling
  expansion.index.w[is.nan(expansion.index.w)]<-0 # correction for 0/0
  stability.index.w[is.nan(stability.index.w)]<-0 # correction for 0/0
  restriction.index.w[is.nan(restriction.index.w)]<-0 # correction for 0/0
  part <- list()
  # part$dyn <- rotate(dyn)
  part$dynamic.index.w <- c(expansion.index.w, stability.index.w, restriction.index.w)
  names(part$dynamic.index.w) <- c("expansion", "stability", "unfilling")
  return(part)
}


p.val.gen<-function(sim,obs,alt){
  if (isFALSE(alt %in% c("higher","lower","different"))){
    stop("Please choose an alternative hypothesis (higher,lower or different)")
  }else  if(alt == "higher"){
    p<-(sum(sim>=obs)+1)/(length(sim)+1)
  }else  if(alt == "lower"){
    p<-(sum(sim<=obs)+1)/(length(sim)+1)
  }else  if(alt == "different"){
    p<-2*min(sum(sim >= obs) + 1,(sum(sim <= obs) + 1))/(length(sim)+1)
  }
  return(p)
}

