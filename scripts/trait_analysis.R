library(dplyr)
library(phylolm)

rm(list = ls())

# Define logit function
logit = function(x) {x = ifelse(x < 0.0001,0.0001,ifelse(x > 0.9999,.9999,x));log(x/(1 - x))}

#  required data ----------------------------------------------------------

load("data/trait_analysis/trait_example_df_spec.RData")
load("results/master_results_new.RData")

# load phylogeny
phylo_pacific <- read.tree("data/Phylogeny.tre", keep.multi = TRUE)

# phylo data --------------------------------------------------------------

no_match <- trait_example_df$species[!sub(' ','_',trait_example_df$species) %in% phylo_pacific[[1]][["tip.label"]]]

phylo_subset <- drop.tip(phylo_pacific, phylo_pacific$tip.label[!phylo_pacific$tip.label %in% sub(' ','_',trait_example_df$species)])

# Standardise traits
trait_example_df$mean_height_GIFT <- scale(as.numeric(trait_example_df$mean_height_GIFT))
trait_example_df$max_elev_range_GIFT <- scale(as.numeric(trait_example_df$max_elev_range_GIFT))
trait_example_df$range_both <- scale(as.numeric(trait_example_df$range_both))
trait_example_df$lat_centroid <- trait_example_df$lat_centroid/90

# recode categorical traits

# woodiness_GIFT
trait_example_df[trait_example_df == "non-woody"] <- 1
trait_example_df[trait_example_df == "woody"] <- 2

# growth form
trait_example_df[trait_example_df == "herb"] <- 1
trait_example_df[trait_example_df == "shrub"] <- 2
trait_example_df[trait_example_df == "tree"] <- 3

# life-cycle
trait_example_df[trait_example_df == "annual"] <- 1
trait_example_df[trait_example_df == "perennial"] <- 2

# mycorrhiza
trait_example_df[trait_example_df == "no"] <- 1
trait_example_df[trait_example_df == "partial"] <- 2
trait_example_df[trait_example_df == "yes"] <- 3


# merge traits with response variables ------------------------------------

spp_traits <- unique(trait_example_df$species)

# subset for pacific region to test the trait analysis
traits_all <- master_results %>%
  filter(species %in% spp_traits, region == "pac") %>%
  select(species, region, overlap, rel.abandonment, rel.unfilling, rel.stability, rel.expansion, rel.pioneering) %>%
  left_join(trait_example_df, by = "species") %>%
  relocate(species_id)



# add rownames - needed for matching phylogenetic information
rownames(traits_all) = sub(' ','_', traits_all$species)

# Logit-transform response variables
traits_all$overlap <- logit(traits_all$overlap)
traits_all$rel.abandonment <- logit(traits_all$rel.abandonment)
traits_all$rel.unfilling <- logit(traits_all$rel.unfilling)
traits_all$rel.stability <- logit(traits_all$rel.stability)
traits_all$rel.expansion <- logit(traits_all$rel.expansion)
traits_all$rel.pioneering <- logit(traits_all$rel.pioneering)



# Models ------------------------------------------------------------------

# overlap
null_overlap <- phylolm(overlap ~ 1, data = traits_all, phy = phylo_subset[[1]], model = "lambda")

# full models
lm_overlap <- phylolm(overlap ~ mean_height_GIFT + woodiness_GIFT + growth_form_GIFT + lifecycle_GIFT + max_elev_range_GIFT + mycorrhiza_both + lat_centroid + range_both, 
                    data = traits_all, phy = phylo_subset[[1]], model = "lambda")


# step model
step_lm_overlap <- phylostep(lm_overlap$formula, data = traits_all, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_overlap$residuals^2)/sum(null_overlap$residuals^2)
1 - sum(step_lm_overlap$residuals^2)/sum(null_overlap$residuals^2)



#'---------------------------------


# stability
null_stability <- phylolm(rel.stability ~ 1, data = traits_all, phy = phylo_subset[[1]], model = "lambda")

# full models
lm_stability <- phylolm(rel.stability ~ mean_height_GIFT + woodiness_GIFT + growth_form_GIFT + lifecycle_GIFT + max_elev_range_GIFT + mycorrhiza_both + lat_centroid + range_both, 
                     data = traits_all, phy = phylo_subset[[1]], model = "lambda")


# step model
step_lm_stability <- phylostep(lm_stability$formula, data = traits_all, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_stability$residuals^2)/sum(null_stability$residuals^2)
1 - sum(step_lm_stability$residuals^2)/sum(null_stability$residuals^2)


#'---------------------------------


# unfilling
null_unfilling <- phylolm(rel.unfilling ~ 1, data = traits_all, phy = phylo_subset[[1]], model = "lambda", lower.bound = 10^-8)

# full models
lm_unfilling <- phylolm(rel.unfilling ~ mean_height_GIFT + woodiness_GIFT + growth_form_GIFT + lifecycle_GIFT + max_elev_range_GIFT + mycorrhiza_both + lat_centroid + range_both, 
                       data = traits_all, phy = phylo_subset[[1]], model = "lambda", lower.bound = 10^-8)


# step model
step_lm_unfilling <- phylostep(lm_unfilling$formula, data = traits_all, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_unfilling$residuals^2)/sum(null_unfilling$residuals^2)
1 - sum(step_lm_unfilling$residuals^2)/sum(null_unfilling$residuals^2)

#'---------------------------------


# expansion
null_expansion <- phylolm(rel.expansion ~ 1, data = traits_all, phy = phylo_subset[[1]], model = "lambda")

# full models
lm_expansion <- phylolm(rel.expansion ~ mean_height_GIFT + woodiness_GIFT + growth_form_GIFT + lifecycle_GIFT + max_elev_range_GIFT + mycorrhiza_both + lat_centroid + range_both, 
                       data = traits_all, phy = phylo_subset[[1]], model = "lambda")


# step model
step_lm_expansion <- phylostep(lm_expansion$formula, data = traits_all, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_expansion$residuals^2)/sum(null_expansion$residuals^2)
1 - sum(step_lm_expansion$residuals^2)/sum(null_expansion$residuals^2)


#'---------------------------------


# abandonment
null_abandonment <- phylolm(rel.abandonment ~ 1, data = traits_all, phy = phylo_subset[[1]], model = "lambda")

# full models
lm_abandonment <- phylolm(rel.abandonment ~ mean_height_GIFT + woodiness_GIFT + growth_form_GIFT + lifecycle_GIFT + max_elev_range_GIFT + mycorrhiza_both + lat_centroid + range_both, 
                        data = traits_all, phy = phylo_subset[[1]], model = "lambda")


# step model
step_lm_abandonment <- phylostep(lm_abandonment$formula, data = traits_all, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_abandonment$residuals^2)/sum(null_abandonment$residuals^2)
1 - sum(step_lm_abandonment$residuals^2)/sum(null_abandonment$residuals^2)

#'---------------------------------


# pioneering
null_pioneering <- phylolm(rel.pioneering ~ 1, data = traits_all, phy = phylo_subset[[1]], model = "lambda")

# full models
lm_pioneering <- phylolm(rel.pioneering ~ mean_height_GIFT + woodiness_GIFT + growth_form_GIFT + lifecycle_GIFT + max_elev_range_GIFT + mycorrhiza_both + lat_centroid + range_both, 
                          data = traits_all, phy = phylo_subset[[1]], model = "lambda")


# step model
step_lm_pioneering <- phylostep(lm_pioneering$formula, data = traits_all, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_pioneering$residuals^2)/sum(null_pioneering$residuals^2)
1 - sum(step_lm_pioneering$residuals^2)/sum(null_pioneering$residuals^2)


save(null_stability, lm_stability, step_lm_stability,
     null_unfilling, lm_unfilling, step_lm_unfilling,
     null_expansion, lm_expansion, step_lm_expansion,
     null_abandonment, lm_abandonment, step_lm_abandonment,
     null_pioneering, lm_pioneering, step_lm_pioneering,
     file = "results/trait_analysis/phylo_trait_models.RData")


#------------ variable importance ---------

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


covariates <- c("mean_height_GIFT", "woodiness_GIFT", "growth_form_GIFT", "lifecycle_GIFT", "max_elev_range_GIFT", "mycorrhiza_both", "lat_centroid", "range_both")

# names of models
MOD <- c("step_lm_overlap", "step_lm_stability", "step_lm_unfilling", "step_lm_expansion", "step_lm_abandonment", "step_lm_pioneering")

output <- list()

for (i in MOD) {
  modl <- ETP(i)
  respvar <- strsplit(formula(modl), " ")[[1]][1]
  explvar <- names(modl$coefficients)[2:length(modl$coefficients)]
  
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
step.varImp.allmetrics = output
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
save(step.varImp.allmetrics, file = file.path(data_dir,"VarImp_phylo_trait_models.RData"))

for (i in MOD) {
  print(i)
  print(ETP(paste0("step.varImp.allmetrics","$",i,"$obs[1]")))
  print(ETP(paste0("step.varImp.allmetrics","$",i,"$Mr2")))
}



