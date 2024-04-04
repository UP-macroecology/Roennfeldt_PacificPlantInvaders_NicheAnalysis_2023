library(dotwhisker)
library(dplyr)
library(phylolm)
library(stringr)
library(tidyr)

# preamble ----------------------------------------------------------------

rm(list = ls())

# load data ---------------------------------------------------------------

# input data 
load("data/trait_analysis/input_TA.RData")

#add row names - needed to match phylogenetic information
#rownames(input_TA) <- gsub(' ','_', input_TA$species_region)


# regional analysis -------------------------------------------------------


regions <- unique(input_TA$region)

input_TA_all_regions <- input_TA

# loop over regions

for (reg in regions) {
  
  print(reg)
  
  # subset trait df to current region
  input_TA <- subset(input_TA_all_regions, region == reg)
  
  #add row names - needed to match phylogenetic information
  rownames(input_TA) <- gsub(' ','_', input_TA$species)
  
  pac_tree <-  read.tree("data/phylogenies/Phylogeny.tre", keep.multi = TRUE)
  
  # remove tip labels that don't match the final species subset for the trait analysis 
  pac_tree <- drop.tip(pac_tree, pac_tree[[1]][["tip.label"]][!pac_tree[[1]][["tip.label"]] %in% gsub(' ','_',input_TA$species)])
  
  #identify which species are not in the tree and remove their names from the input df
  no_match <- input_TA$species[!gsub(' ','_',input_TA$species) %in% pac_tree[[1]][["tip.label"]]]
  
  input_TA <- input_TA %>% filter(!species %in% no_match)
  
  
  # define models -----------------------------------------------------------
  
  # ++ stability ++
  
  null_stability <- phylolm(stability ~ 1, data = input_TA, phy = pac_tree[[1]], model = "lambda")
  
  
  # full models
  lm_stability <- phylolm(stability ~ mean_height + mean_seedmass + growth_form + lifecycle + max_elev_range + lat_nat + lon_nat + range_size_nat 
                          + niche_breadth_nat + niche_centroid_a_nat + niche_centroid_b_nat + years_since_intro + eucl_dist, 
                          data = input_TA, phy = pac_tree[[1]], model = "lambda")
  
  # step model
  step_lm_stability <- phylostep(lm_stability$formula, data = input_TA, phy = pac_tree[[1]], model = "lambda")
  
  
  
  # ++ unfilling ++
  
  null_unfilling <- phylolm(unfilling ~ 1, data = input_TA, phy = pac_tree[[1]], model = "lambda")
  
  # full models
  lm_unfilling <- phylolm(unfilling ~ mean_height + mean_seedmass + growth_form + lifecycle + max_elev_range + lat_nat + lon_nat + range_size_nat 
                          + niche_breadth_nat + niche_centroid_a_nat + niche_centroid_b_nat + years_since_intro + eucl_dist,
                          data = input_TA, phy = pac_tree[[1]], model = "lambda")
  
  # step model
  step_lm_unfilling <- phylostep(lm_unfilling$formula, data = input_TA, phy = pac_tree[[1]], model = "lambda")
  
  
  
  # ++ expansion ++
  
  null_expansion <- phylolm(expansion ~ 1, data = input_TA, phy = pac_tree[[1]], model = "lambda")
  
  # full models
  lm_expansion <- phylolm(expansion ~ mean_height + mean_seedmass + growth_form + lifecycle + max_elev_range + lat_nat + lon_nat + range_size_nat 
                          + niche_breadth_nat + niche_centroid_a_nat + niche_centroid_b_nat + years_since_intro + eucl_dist,
                          data = input_TA, phy = pac_tree[[1]], model = "lambda")
  
  # step model
  step_lm_expansion <- phylostep(lm_expansion$formula, data = input_TA, phy = pac_tree[[1]], model = "lambda")
  
  
  # save models
  save(null_stability, lm_stability, step_lm_stability,
       null_unfilling, lm_unfilling, step_lm_unfilling,
       null_expansion, lm_expansion, step_lm_expansion,
       file = paste0("results/trait_analysis/combined_regions/ESU_models_",reg,".RData"))
  
  # variable importance -----------------------------------------------------
  
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
  
  
  covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", "max_elev_range", "lon_nat", "lat_nat", "range_size_nat",
                  "niche_breadth_nat", "niche_centroid_a_nat", "niche_centroid_b_nat", "years_since_intro", "eucl_dist")
  
  # names of models
  MOD <- c("step_lm_stability", "step_lm_unfilling", "step_lm_expansion")
  
  output <- list()
  
  for (i in MOD) {
    modl <- ETP(i)
    respvar <- strsplit(formula(modl), " ")[[1]][1]
    explvar <- names(modl$coefficients)[2:length(modl$coefficients)]
    
    # make sure each covariate only occurs ones and wihtout a 2 or 3 at the end
    explvar <- explvar %>%
      str_replace("2", "") %>%
      str_replace("3", "") %>%
      str_replace("other", "")
    
    explvar <- unique(explvar)
    
    # NOTE: if you include quadratic terms, then some more fiddling might be needed here.
    
    # Calculate the R2 with a null model for which we fix the lambda value
    if (modl$optpar > 0.000001) {
      # pgls0 <- ETP(paste("pgls(", respvar, "~ 1, data=all_data, lambda=modl$optpar)"))
      pgls0 <- ETP(paste("phylolm(",respvar, "~ 1, data = input_TA, phy = pac_tree[[1]], model ='lambda')"))
      # pgls1 <- ETP(paste("pgls(", formula(modl), ", data=all_data, lambda=modl$optpar)"))
      pgls1 <- ETP(paste("phylolm(",formula(modl), ", data = input_TA, phy = pac_tree[[1]], model = 'lambda')"))
      r2 <- R2(pgls0, pgls1)
      # Calculate the variable importance with variable randomization and fixed lambda
      rR2 <- rR2a <- data.frame(matrix(NA, nrep, length(explvar), T, list(1:nrep, explvar)))
      for (j in explvar) {
        # tempdat <- pgls1$data
        tempdat <- input_TA
        for (k in 1:nrep) {	
          # tempdat$data[,j] <- sample(tempdat$data[,j])
          tempdat[,j] <- sample(tempdat[,j])
          # mt <- ETP(paste("pgls(", formula(modl), ", data=tempdat, lambda=modl$optpar)"))
          mt <- ETP(paste("phylolm(",formula(modl), ", data=tempdat, phy=pac_tree[[1]], model='lambda')"))
          rR2[k,j] <- R2(pgls0, mt)[1] ; rR2a[k,j] <- R2(pgls0, mt)[2]
          cat(k)
        }
        print(j)
      }
    } else {
      pgls0 <- ETP(paste("glm(", respvar, "~ 1, data=input_TA)"))
      pgls1 <- ETP(paste("glm(", formula(modl), ", data=input_TA)"))
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
  save(step.varImp.allmetrics, file = paste0("results/trait_analysis/regional_variance_importance/VarImp_phylo_trait_models_ESU_",reg,".RData"))
  
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
    
    # # make sure each covariate only occurs ones and wihtout a 2 or 3 at the end
    # explvar <- explvar %>%
    #   str_replace("2", "") %>%
    #   str_replace("3", "")%>%
    #   str_replace("other", "")
    # 
    # explvar <- unique(explvar)
    
    # store model coefficients
    results_TraitAnal_df[c(1,which(covariates %in% explvar) + 1),1 + i*4 - 3] <- round(as.numeric(modl$coefficients),3)
    
    # store R2 of model
    results_TraitAnal_df[nrow(results_TraitAnal_df)-1,1+i*4-3] <- R2(ETP(MOD_null[i]),modl)[[1]]
    
    # store lambda of model
    results_TraitAnal_df[nrow(results_TraitAnal_df),1+i*4-3] <- round(modl$optpar,3)
    
    # store std errors
    results_TraitAnal_df[c(1,which(covariates %in% explvar)+1),1+i*4-2] <- summary(modl)$coefficients[,2]
    
    # store p-values
    results_TraitAnal_df[c(1,which(covariates %in% explvar) + 1),1 + i*4 - 1] <- round(as.numeric(summary(modl)$coefficients[,4]),3)
    
    # store variable importance
    varimp <- round(step.varImp.allmetrics[[MOD[i]]]$Mr2,3)
    results_TraitAnal_df[which(covariates %in% explvar) + 1,1 + i*4] <- ifelse(varimp < 0,0,varimp)
  }
  
  write.csv(results_TraitAnal_df, file = file.path(paste0("results/trait_analysis/regional_trait_analysis_results/results_TraitAnal_df_ESU_",reg,".csv")), row.names = F)
  
  
} # end of for loop over regions







# complete trait models ---------------------------------------------------



# start loop over the different polytomt ages that were used 
# ages <- round(seq(from = 0.01, to = 0.4, length.out = 16), 2)

# ages <- ages[-1]

for (age in ages) {
  
  pac_tree <- read.tree(paste0("data/phylogenies/same_age/Pac_tree_age_",age,".tre"))
  
  # remove tip labels that don't match the final species subset for the trait analysis (n = 109)
  no_match <- input_TA$species_region[!gsub(' ','_',input_TA$species_region) %in% pac_tree[["tip.label"]]]
  
  pac_tree <- drop.tip(pac_tree, pac_tree$tip.label[!pac_tree$tip.label %in% gsub(' ','_',input_TA$species_region)])
  
  # define models -----------------------------------------------------------
  
  # ++ stability ++
  
  null_stability <- phylolm(stability ~ 1, data = input_TA, phy = pac_tree, model = "lambda")
  
  # full models
  lm_stability <- phylolm(stability ~ mean_height + mean_seedmass + growth_form + lifecycle + max_elev_range + lat_centroid + lon_centroid + range_size 
                          + niche_breadth_zcor + niche_centroid_a_global + niche_centroid_b_global + CAI + years_since_intro, 
                          data = input_TA, phy = pac_tree, model = "lambda")
  
  # step model
  step_lm_stability <- phylostep(lm_stability$formula, data = input_TA, phy = pac_tree, model = "lambda")
  
  
  
  # ++ unfilling ++
  
  null_unfilling <- phylolm(unfilling ~ 1, data = input_TA, phy = pac_tree, model = "lambda")
  
  # full models
  lm_unfilling <- phylolm(unfilling ~ mean_height + mean_seedmass + growth_form + lifecycle + max_elev_range + lat_centroid + lon_centroid + range_size 
                          + niche_breadth_zcor + niche_centroid_a_global + niche_centroid_b_global + CAI + years_since_intro, 
                          data = input_TA, phy = pac_tree, model = "lambda")
  
  # step model
  step_lm_unfilling <- phylostep(lm_unfilling$formula, data = input_TA, phy = pac_tree, model = "lambda")
  
  
  
  # ++ expansion ++
  
  null_expansion <- phylolm(expansion ~ 1, data = input_TA, phy = pac_tree, model = "lambda")
  
  # full models
  lm_expansion <- phylolm(expansion ~ mean_height + mean_seedmass + growth_form + lifecycle + max_elev_range + lat_centroid + lon_centroid + range_size 
                          + niche_breadth_zcor + niche_centroid_a_global + niche_centroid_b_global + CAI + years_since_intro, 
                          data = input_TA, phy = pac_tree, model = "lambda")
  
  # step model
  step_lm_expansion <- phylostep(lm_expansion$formula, data = input_TA, phy = pac_tree, model = "lambda")
  
  
  # save models
  save(null_stability, lm_stability, step_lm_stability,
       null_unfilling, lm_unfilling, step_lm_unfilling,
       null_expansion, lm_expansion, step_lm_expansion,
       file = paste0("results/trait_analysis/trait_models/ESU_models_age_",age,".RData"))
  
  
  # variable importance -----------------------------------------------------
  
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
  
  
  covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", "max_elev_range", "lon_centroid", "lat_centroid", "range_size",
                  "niche_breadth_zcor", "niche_centroid_a_global", "niche_centroid_b_global", "CAI", "years_since_intro")
  
  # names of models
  MOD <- c("step_lm_stability", "step_lm_unfilling", "step_lm_expansion")
  
  output <- list()
  
  for (i in MOD) {
    modl <- ETP(i)
    respvar <- strsplit(formula(modl), " ")[[1]][1]
    explvar <- names(modl$coefficients)[2:length(modl$coefficients)]
    
    # make sure each covariate only occurs ones and wihtout a 2 or 3 at the end
    explvar <- explvar %>%
      str_replace("2", "") %>%
      str_replace("3", "") %>%
      str_replace("other", "")
    
    explvar <- unique(explvar)
    
    # NOTE: if you include quadratic terms, then some more fiddling might be needed here.
    
    # Calculate the R2 with a null model for which we fix the lambda value
    if (modl$optpar > 0.000001) {
      # pgls0 <- ETP(paste("pgls(", respvar, "~ 1, data=all_data, lambda=modl$optpar)"))
      pgls0 <- ETP(paste("phylolm(",respvar, "~ 1, data = input_TA, phy = pac_tree, model ='lambda')"))
      # pgls1 <- ETP(paste("pgls(", formula(modl), ", data=all_data, lambda=modl$optpar)"))
      pgls1 <- ETP(paste("phylolm(",formula(modl), ", data = input_TA, phy = pac_tree, model = 'lambda')"))
      r2 <- R2(pgls0, pgls1)
      # Calculate the variable importance with variable randomization and fixed lambda
      rR2 <- rR2a <- data.frame(matrix(NA, nrep, length(explvar), T, list(1:nrep, explvar)))
      for (j in explvar) {
        # tempdat <- pgls1$data
        tempdat <- input_TA
        for (k in 1:nrep) {	
          # tempdat$data[,j] <- sample(tempdat$data[,j])
          tempdat[,j] <- sample(tempdat[,j])
          # mt <- ETP(paste("pgls(", formula(modl), ", data=tempdat, lambda=modl$optpar)"))
          mt <- ETP(paste("phylolm(",formula(modl), ", data=tempdat, phy=pac_tree, model='lambda')"))
          rR2[k,j] <- R2(pgls0, mt)[1] ; rR2a[k,j] <- R2(pgls0, mt)[2]
          cat(k)
        }
        print(j)
      }
    } else {
      pgls0 <- ETP(paste("glm(", respvar, "~ 1, data=input_TA)"))
      pgls1 <- ETP(paste("glm(", formula(modl), ", data=input_TA)"))
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
  save(step.varImp.allmetrics, file = paste0("results/trait_analysis/variance_importance/VarImp_phylo_trait_models_ESU_",age,".RData"))
  
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
    
    # # make sure each covariate only occurs ones and wihtout a 2 or 3 at the end
    # explvar <- explvar %>%
    #   str_replace("2", "") %>%
    #   str_replace("3", "")%>%
    #   str_replace("other", "")
    # 
    # explvar <- unique(explvar)
    
    # store model coefficients
    results_TraitAnal_df[c(1,which(covariates %in% explvar) + 1),1 + i*4 - 3] <- round(as.numeric(modl$coefficients),3)
    
    # store R2 of model
    results_TraitAnal_df[nrow(results_TraitAnal_df)-1,1+i*4-3] <- R2(ETP(MOD_null[i]),modl)[[1]]
    
    # store lambda of model
    results_TraitAnal_df[nrow(results_TraitAnal_df),1+i*4-3] <- round(modl$optpar,3)
    
    # store std errors
    results_TraitAnal_df[c(1,which(covariates %in% explvar)+1),1+i*4-2] <- summary(modl)$coefficients[,2]
    
    # store p-values
    results_TraitAnal_df[c(1,which(covariates %in% explvar) + 1),1 + i*4 - 1] <- round(as.numeric(summary(modl)$coefficients[,4]),3)
    
    # store variable importance
    varimp <- round(step.varImp.allmetrics[[MOD[i]]]$Mr2,3)
    results_TraitAnal_df[which(covariates %in% explvar) + 1,1 + i*4] <- ifelse(varimp < 0,0,varimp)
  }
  
  write.csv(results_TraitAnal_df, file = file.path(paste0("results/trait_analysis/trait_analysis_results/results_TraitAnal_df_ESU_",age,".csv")), row.names = F)
  
} # end of loop over ages




# plot effect size --------------------------------------------------------

ages <- round(seq(from = 0.01, to = 0.4, length.out = 16), 2)

covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", "max_elev_range", "lon_centroid", "lat_centroid", "range_size",
                "niche_breadth_zcor", "niche_centroid_a_global", "niche_centroid_b_global", "CAI", "years_since_intro")



traits_res <- read.csv("results/trait_analysis/trait_analysis_results/results_TraitAnal_df_ESU_0.01.csv")

traits_res_tb_e <- traits_res %>%
  as_tibble() %>% 
  mutate(model = "expansion", term = Trait, estimate = expansion_coef, std.error = expansion_stderr, statistic = expansion_coef/expansion_stderr, p.value = expansion_p, varimp = expansion_varimp) %>%
  select(model, term, estimate, std.error, statistic, p.value, varimp) %>% 
  filter(term %in% covariates)

traits_res_tb_s <- traits_res %>%
  as_tibble() %>% 
  mutate(model = "stability", term = Trait, estimate = stability_coef, std.error = stability_stderr, statistic = stability_coef/stability_stderr, p.value = stability_p, varimp = stability_varimp) %>%
  select(model, term, estimate, std.error, statistic, p.value, varimp) %>% 
  filter(term %in% covariates)

traits_res_tb_u <- traits_res %>%
  as_tibble() %>% 
  mutate(model = "unfilling", term = Trait, estimate = unfilling_coef, std.error = unfilling_stderr, statistic = unfilling_coef/unfilling_stderr, p.value = unfilling_p, varimp = unfilling_varimp) %>%
  select(model, term, estimate, std.error, statistic, p.value, varimp) %>% 
  filter(term %in% covariates)

# traits_res_tb_all <- rbind(traits_res_tb_a, traits_res_tb_e, traits_res_tb_p, traits_res_tb_s, traits_res_tb_u)
traits_res_tb_all <- rbind(traits_res_tb_e, traits_res_tb_s, traits_res_tb_u)


# windows(w = 4,h = 3)

(p <- dwplot(traits_res_tb_all, 
             vline = geom_vline(xintercept = 0, colour = "grey80", linetype = 2),
             # dot_args = list(aes(shape = model)),
             dot_args = list(size = 1),
             whisker_args = list(size = 1)) %>% 
    relabel_predictors(c(mean_height = "Height", 
                         mean_seedmass = "Seed mass",
                         growth_form = "Growth form",
                         lifecycle = "Life cycle",
                         max_elev_range = "Elev. range",
                         lon_centroid = "Lon. centroid",
                         lat_centroid = "Lat. centroid",
                         range_size = "Range size")) + 
    theme_classic() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          axis.text = element_text(colour = "#1B3C59"),
          #axis.text.y = element_text(angle = -35),
          axis.line = element_line(linewidth = 0.5)) +
    ggtitle(paste0("ESU coefficient estimate (Polytomy age ",age,")")) +
    xlab("Coefficient estimate") + 
    ylab("") + 
    scale_color_manual(values = c("unfilling" = "#FFE875", "stability" = "#A3DDEF","expansion" = "#87CF87"), name = "model"))

ggsave(paste0("plots/results/trait_analysis/Coefficient estimate_ESU_",age,".pdf"), p,
       bg = "transparent",
       width = 6,
       height = 5,
       units = "cm")


