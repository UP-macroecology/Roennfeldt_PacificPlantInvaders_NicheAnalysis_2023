library(dplyr)
library(phylolm)
library(stringr)

rm(list = ls())

# Define logit function
logit <- function(x) {x = ifelse(x < 0.0001,0.0001,ifelse(x > 0.9999,.9999,x));log(x/(1 - x))}

#  required data ----------------------------------------------------------

load("data/trait_analysis/trait_data_all.RData")
load("results/ecospat/master_results.RData")

# load phylogeny
phylo_pacific <- read.tree("data/Phylogeny.tre", keep.multi = TRUE)

# phylo data --------------------------------------------------------------

no_match <- trait_data_all$species[!sub(' ','_',trait_data_all$species) %in% phylo_pacific[[1]][["tip.label"]]]

phylo_subset <- drop.tip(phylo_pacific, phylo_pacific$tip.label[!phylo_pacific$tip.label %in% sub(' ','_',trait_data_all$species)])

# Standardise traits
trait_data_all$mean_height <- scale(as.numeric(trait_data_all$mean_height))
trait_data_all$max_elev_range <- scale(as.numeric(trait_data_all$max_elev_range))
trait_data_all$range_both <- scale(as.numeric(trait_data_all$range_both))
trait_data_all$lat_centroid <- trait_data_all$lat_centroid/90

# recode categorical traits

# woodiness
trait_data_all[trait_data_all == "non-woody"] <- 1
trait_data_all[trait_data_all == "woody"] <- 2

# growth form
trait_data_all[trait_data_all == "herb"] <- 1
trait_data_all[trait_data_all == "shrub"] <- 2
trait_data_all[trait_data_all == "tree"] <- 3

# life-cycle
trait_data_all[trait_data_all == "annual"] <- 1
trait_data_all[trait_data_all == "perennial"] <- 2

# mycorrhiza
trait_data_all[trait_data_all == "no"] <- 1
trait_data_all[trait_data_all == "partial"] <- 2
trait_data_all[trait_data_all == "yes"] <- 3

# Logit-transform response variables
# trait_data_all$overlap <- logit(trait_data_all$overlap)
trait_data_all$rel_abandonment <- logit(trait_data_all$rel_abandonment)
trait_data_all$rel_unfilling <- logit(trait_data_all$rel_unfilling)
trait_data_all$rel_stability <- logit(trait_data_all$rel_stability)
trait_data_all$rel_expansion <- logit(trait_data_all$rel_expansion)
trait_data_all$rel_pioneering <- logit(trait_data_all$rel_pioneering)

spp_traits <- unique(trait_data_all$species)

# subset for pac region
trait_data_all <- trait_data_all %>% filter(region == "pac")


#add rownames - needed for matching phylogenetic information
rownames(trait_data_all) = sub(' ','_', trait_data_all$species)

# save(trait_data_all, file = "data/trait_analysis/trait_data_all_example.RData")

# Models ------------------------------------------------------------------

# overlap
null_overlap <- phylolm(overlap ~ 1, data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")

# full models
lm_overlap <- phylolm(overlap ~ mean_height + woodiness + growth_form + lifecycle + max_elev_range + mycorrhiza + lat_centroid + range_both,
                    data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")


# step model
step_lm_overlap <- phylostep(lm_overlap$formula, data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_overlap$residuals^2)/sum(null_overlap$residuals^2)
1 - sum(step_lm_overlap$residuals^2)/sum(null_overlap$residuals^2)



#'---------------------------------

# trait_data_all$species <- sub(' ','_',trait_data_all$species)
  
# stability
null_stability <- phylolm(rel_stability ~ 1, data = trait_data_all, phy = phylo_pacific[[1]], model = "lambda")

# full models
lm_stability <- phylolm(rel_stability ~ mean_height + woodiness + growth_form + lifecycle + max_elev_range + mycorrhiza + lat_centroid + range_both, 
                     data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")


# step model
step_lm_stability <- phylostep(lm_stability$formula, data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_stability$residuals^2)/sum(null_stability$residuals^2)
1 - sum(step_lm_stability$residuals^2)/sum(null_stability$residuals^2)


#'---------------------------------


# unfilling
null_unfilling <- phylolm(rel_unfilling ~ 1, data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")

# full models
lm_unfilling <- phylolm(rel_unfilling ~ mean_height + woodiness + growth_form + lifecycle + max_elev_range + mycorrhiza + lat_centroid + range_both, 
                       data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")


# step model
step_lm_unfilling <- phylostep(lm_unfilling$formula, data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_unfilling$residuals^2)/sum(null_unfilling$residuals^2)
1 - sum(step_lm_unfilling$residuals^2)/sum(null_unfilling$residuals^2)

#'---------------------------------


# expansion
null_expansion <- phylolm(rel_expansion ~ 1, data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")

# full models
lm_expansion <- phylolm(rel_expansion ~ mean_height + woodiness + growth_form + lifecycle + max_elev_range + mycorrhiza + lat_centroid + range_both, 
                       data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")


# step model
step_lm_expansion <- phylostep(lm_expansion$formula, data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_expansion$residuals^2)/sum(null_expansion$residuals^2)
1 - sum(step_lm_expansion$residuals^2)/sum(null_expansion$residuals^2)


#'---------------------------------


# abandonment
null_abandonment <- phylolm(rel_abandonment ~ 1, data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")

# full models
lm_abandonment <- phylolm(rel_abandonment ~ mean_height + woodiness + growth_form + lifecycle + max_elev_range + mycorrhiza + lat_centroid + range_both, 
                        data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")


# step model
step_lm_abandonment <- phylostep(lm_abandonment$formula, data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_abandonment$residuals^2)/sum(null_abandonment$residuals^2)
1 - sum(step_lm_abandonment$residuals^2)/sum(null_abandonment$residuals^2)

#'---------------------------------


# pioneering
null_pioneering <- phylolm(rel_pioneering ~ 1, data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")

# full models
lm_pioneering <- phylolm(rel_pioneering ~ mean_height + woodiness + growth_form + lifecycle + max_elev_range + mycorrhiza + lat_centroid + range_both, 
                          data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")


# step model
step_lm_pioneering <- phylostep(lm_pioneering$formula, data = trait_data_all, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_pioneering$residuals^2)/sum(null_pioneering$residuals^2)
1 - sum(step_lm_pioneering$residuals^2)/sum(null_pioneering$residuals^2)


save(null_overlap, lm_overlap, step_lm_overlap,
     null_stability, lm_stability, step_lm_stability,
     null_unfilling, lm_unfilling, step_lm_unfilling,
     null_expansion, lm_expansion, step_lm_expansion,
     null_abandonment, lm_abandonment, step_lm_abandonment,
     null_pioneering, lm_pioneering, step_lm_pioneering,
     file = "results/trait_analysis/phylo_trait_models.RData")


#------------ variable importance ---------

rm(list = setdiff(ls(), c("phylo_subset", "traits_all")))

load("results/trait_analysis/phylo_trait_models.RData")

# code adapted from Zurell et al. (2018) JBI

# Helper functions:
R2 <- function(mod0, mod1){
  p <- length(mod1$coefficients)
  n <- length(mod1$fitted)
  
  R2 <- round(1 - sum(mod1$residuals^2)/sum(mod0$residuals^2), 3)
  R2adj <- 1 - ((n - 1)/(n - p)) * (1 - R2)
  R2adj <- ifelse(R2adj < 0, 0, R2adj)
  return(c(R2 = R2, R2adj = R2adj)) 
}

R2glm <- function(model){
  p <- length(model$coefficients)
  n <- length(model$fitted)
  
  R2 <- round(1 - ( model$deviance / model$null.deviance ), 2)
  R2adj <- 1 - ((n - 1)/(n - p)) * (1 - R2)
  R2adj <- ifelse(R2adj < 0, 0, R2adj)
  return(c(R2 = R2, R2adj = R2adj)) 
}

ETP <- function(x) {eval(parse(text = x))}

# number of replicates
nrep <- 99


covariates <- c("mean_height", "woodiness", "growth_form", "lifecycle", "max_elev_range", "mycorrhiza", "lat_centroid", "range_both")

# names of models
MOD <- c("step_lm_stability", "step_lm_unfilling", "step_lm_expansion", "step_lm_abandonment", "step_lm_pioneering")

output <- list()

for (i in MOD) {
  modl <- ETP(i)
  respvar <- strsplit(formula(modl), " ")[[1]][1]
  explvar <- names(modl$coefficients)[2:length(modl$coefficients)]
  
  # make sure each covariate only occurs ones and wihtout a 2 or 3 at the end
  explvar <- explvar %>%
    str_replace("2", "") %>%
    str_replace("3", "") 
  
  explvar <- unique(explvar)
  
  # NOTE: if you include quadratic terms, then some more fiddling might be needed here.
  
  # Calculate the R2 with a null model for which we fix the lambda value
  if (modl$optpar > 0.000001) {
    # pgls0 <- ETP(paste("pgls(", respvar, "~ 1, data=all_data, lambda=modl$optpar)"))
    pgls0 <- ETP(paste("phylolm(",respvar, "~ 1, data = traits_all, phy = phylo_subset[[1]], model ='lambda')"))
    # pgls1 <- ETP(paste("pgls(", formula(modl), ", data=all_data, lambda=modl$optpar)"))
    pgls1 <- ETP(paste("phylolm(",formula(modl), ", data = traits_all, phy = phylo_subset[[1]], model = 'lambda')"))
    r2 <- R2(pgls0, pgls1)
    # Calculate the variable importance with variable randomization and fixed lambda
    rR2 <- rR2a <- data.frame(matrix(NA, nrep, length(explvar), T, list(1:nrep, explvar)))
    for (j in explvar) {
      # tempdat <- pgls1$data
      tempdat <- traits_all
      for (k in 1:nrep) {	
        # tempdat$data[,j] <- sample(tempdat$data[,j])
        tempdat[,j] <- sample(tempdat[,j])
        # mt <- ETP(paste("pgls(", formula(modl), ", data=tempdat, lambda=modl$optpar)"))
        mt <- ETP(paste("phylolm(",formula(modl), ", data=tempdat, phy=phylo_subset[[1]], model='lambda')"))
        rR2[k,j] <- R2(pgls0, mt)[1] ; rR2a[k,j] <- R2(pgls0, mt)[2]
        cat(k)
      }
      print(j)
    }
  } else {
    pgls0 <- ETP(paste("glm(", respvar, "~ 1, data=traits_all)"))
    pgls1 <- ETP(paste("glm(", formula(modl), ", data=traits_all)"))
    r2 <- R2glm(pgls1)
    # Calculate the variable importance with variable randomization and fixed lambda
    rR2 <- rR2a <- data.frame(matrix(NA, nrep, length(explvar), T, list(1:nrep, explvar)))
    for (j in explvar) {
      tempdat <- pgls1$data
      for (k in 1:nrep) {	
        tempdat[,j] <- sample(tempdat[,j])
        mt <- ETP(paste("glm(", formula(modl), ", data=tempdat)"))
        rR2[k,j] <- R2glm(mt)[1] ; rR2a[k,j] <- R2glm(mt)[2]
        cat(k)
      }
      print(j)
    }
  }	
  
  output[[i]] <- list(obs = r2, randR2 = rR2, randR2adj = rR2a)
  print(i)
}

# Calculate variable importance (based on mean and SD of simulated R2s)
step.varImp.allmetrics <- output
# get variable importances
for (i in 1:length(step.varImp.allmetrics))	{
  tt <- step.varImp.allmetrics[[i]]
  r2 <- tt[[1]][1] - tt$randR2 
  r2a <- tt[[1]][2] - tt$randR2adj
  # Rescale the values (?)
  if (ncol(r2)>1) {
    r2 <- t(apply(r2, 1, function(x) {x=ifelse(x<0,10^-20,x); x/sum(x)} ))
    r2a <- t(apply(r2a, 1, function(x) {x=ifelse(x<0,10^-20,x); x/sum(x)} ))
    # Get the means and sd
    Mr2 <- apply(r2, 2, mean) ; Mr2a <- apply(r2a, 2, mean)
    Sr2 <- apply(r2, 2, sd) ; Sr2a <- apply(r2a, 2, sd) } else {
      r2 = r2/sum(r2)
      r2a=r2a/sum(r2a)
      Mr2 = mean(r2[,1])
      Mr2a = mean(r2a[,1])
      Sr2 = sd(r2[,1])
      Sr2a = sd(r2a[,1])
    }
  step.varImp.allmetrics[[i]]['Mr2'] <- list(Mr2)
  step.varImp.allmetrics[[i]]['Sr2'] <- list(Sr2) 
  step.varImp.allmetrics[[i]]['Mr2a'] <- list(Mr2a)
  step.varImp.allmetrics[[i]]['Sr2a'] <- list(Sr2a)
}

# the metric Mr2 is of main interest (mean R2)
save(step.varImp.allmetrics, file = "results/trait_analysis/VarImp_phylo_trait_models.RData")

for (i in MOD) {
  print(i)
  print(ETP(paste0("step.varImp.allmetrics","$",i,"$obs[1]")))
  print(ETP(paste0("step.varImp.allmetrics","$",i,"$Mr2")))
}


# Store results -----------------------------------------------------------


# Make data.frame to summarise results of trait models
results_TraitAnal_df <- data.frame(matrix(nrow=length(covariates)+3,ncol=length(MOD)*4+1))
names(results_TraitAnal_df) <- c("Trait",paste0(rep(sub("step.lm.","", MOD),each=4),c("_coef","_stderr","_p","_varimp")))
results_TraitAnal_df[,1] <-  c("Intercept",covariates, "R2", "lambda")

# Make vector of null model names
MOD_null <- sub("step.lm","null",MOD)

# loop through models to collect coefficients and variable importance
for (i in 1:length(MOD)) {
  modl <- ETP(MOD[i])
  
  explvar <- names(modl$coefficients)[2:length(modl$coefficients)]

  # make sure each covariate only occurs ones and wihtout a 2 or 3 at the end
  explvar <- explvar %>%
    str_replace("2", "") %>%
    str_replace("3", "") 
  
  explvar <- unique(explvar)
  
  # store model coefficients
  results_TraitAnal_df[c(1,which(covariates %in% explvar)+1),1+i*4-3] <- round(as.numeric(modl$coefficients),3)
  
  # store R2 of model
  results_TraitAnal_df[nrow(results_TraitAnal_df)-1,1+i*4-3] <- R2(ETP(MOD_null[i]),modl)[[1]]
  
  # store lambda of model
  results_TraitAnal_df[nrow(results_TraitAnal_df),1+i*4-3] <- round(modl$optpar,3)
  
  # store std errors
  results_TraitAnal_df[c(1,which(covariates %in% explvar)+1),1+i*4-2] <- summary(modl)$coefficients[,2]
  
  # store p-values
  results_TraitAnal_df[c(1,which(covariates %in% explvar)+1),1+i*4-1] <- round(as.numeric(summary(modl)$coefficients[,4]),3)
  
  # store variable importance
  varimp <- round(step.varImp.allmetrics[[MOD[i]]]$Mr2,3)
  results_TraitAnal_df[which(covariates %in% explvar)+1,1+i*4] <- ifelse(varimp<0,0,varimp)
}

write.csv(results_TraitAnal_df, file=file.path(data_dir,"results_TraitAnal_df.csv"), row.names=F)




