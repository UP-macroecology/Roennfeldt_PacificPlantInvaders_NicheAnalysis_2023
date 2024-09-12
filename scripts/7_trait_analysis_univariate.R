library(dplyr)
library(stringr)
library(tidyr) # to use unite()

rm(list = ls())


# path_transfer <- "T:/Holle_Roennfeldt/"
# path_transfer <- "Y:/AG26/Transfer/Holle_Roennfeldt/"

logit <- function(x) {x = ifelse(x < 0.0001,0.0001,ifelse(x > 0.9999,.9999,x));log(x/(1 - x))}
ETP <- function(x) {eval(parse(text = x))}

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

# required data
load("data/spp_suitable_AC.RData") # species list 
load("results/ecospat/master_results_AC.RData") # results niche comparison


load("data/trait_analysis/species_pacific_traits_GIFT.RData") # GIFT trait data

# geographic traits
load("data/trait_analysis/native_niche_breadth_centroid.RData")
load("data/trait_analysis/native_range_size.RData")
load("data/trait_analysis/year_first_intro_Seebens.RData")
load("data/trait_analysis/lat_dist.RData")

# prepare input data ------------------------------------------------------

# subset results overview for species in the final spp selection 
df_results <- master_results_AC %>%
  filter(species %in% spp_suitable_AC) %>%
  dplyr::select(species, region, rel_expansion, rel_stability, rel_unfilling, rel_abandonment, rel_pioneering, expansion, unfilling, stability) %>% # select which columns to keep
  rename("orig_expansion" = "expansion",
         "orig_unfilling" = "unfilling",
         "orig_stability" = "stability") %>% 
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(expansion = rel_expansion / total_esu) %>%
  mutate(stability = rel_stability / total_esu) %>%
  mutate(unfilling = rel_unfilling / total_esu) 

# time since introduction
year_first_intro_Seebens[year_first_intro_Seebens == "NI"] <- NA

year_first_intro_Seebens <- year_first_intro_Seebens %>%
  dplyr::select(!pac_region) %>%
  pivot_longer(cols = !species,
               names_to = "region",
               values_to = "years_since_intro") 


year_first_intro_Seebens$years_since_intro <- as.numeric(year_first_intro_Seebens$years_since_intro)


spec_traits <- species_pacific_traits_GIFT %>%
  rename("species" = "species_orig") %>% 
  dplyr::select(species, mean_height_GIFT, mean_seedmass_GIFT, growth_form_GIFT, lifecycle_GIFT) %>% 
  rename_with(stringr::str_replace,
              pattern = "_GIFT", replacement = "",
              matches("_GIFT")) %>%
  filter(species %in% spp_suitable_AC)


# prepare the input data for the trait analysis (TA)
input_TA <- df_results %>% left_join(spec_traits, by = "species") %>%
  unite(species_region, c("species", "region"), remove = FALSE, sep = " ") %>%
  left_join(dplyr::select(native_range_df, species, range_both), by = "species") %>% # native range size
  rename("range_size_nat" = "range_both") %>% 
  left_join(select(df_native_niche, species, niche_breadth_zcor, niche_centroid1_global, niche_centroid2_global), by = "species") %>%  # native niche breadth and centroid
  rename("niche_breadth_nat" = "niche_breadth_zcor",
         "niche_centroid_a_nat" = "niche_centroid1_global",
         "niche_centroid_b_nat" = "niche_centroid2_global") %>% 
  left_join(year_first_intro_Seebens, by = c("species", "region")) %>% # years since first introduction
  left_join(df_lat_distance, by = c("species", "region")) 

  
# growth form
input_TA[input_TA == "herb"] <- 1
input_TA[input_TA == "shrub"] <- 2
input_TA[input_TA == "tree"] <- 3

# life-cycle
input_TA[input_TA == "annual"] <- 1
input_TA[input_TA == "biennial"] <- 2
input_TA[input_TA == "perennial"] <- 3



input_TA_uni <- input_TA %>% 
  mutate(across(!c(species_region, species, region), as.numeric)) %>%
  mutate(mean_height = scale(mean_height)) %>% 
  mutate(mean_seedmass = scale(log(mean_seedmass + 0.00001))) %>% 
  mutate(growth_form = scale(growth_form)) %>% 
  mutate(lifecycle = scale(lifecycle)) %>% 
  mutate(range_size_nat = scale(range_size_nat)) %>% 
  mutate(years_since_intro = scale(log(years_since_intro))) %>% 
  mutate(niche_breadth_nat = scale(niche_breadth_nat)) %>% 
  mutate(niche_centroid_a_nat = scale(niche_centroid_a_nat)) %>% 
  mutate(niche_centroid_b_nat = scale(niche_centroid_b_nat)) %>% 
  mutate(lat_dist = scale(lat_dist)) %>%
  mutate(unfilling = logit(unfilling)) %>% 
  mutate(expansion = logit(expansion)) %>% 
  mutate(stability = logit(stability)) %>% 
  mutate(rel_unfilling = logit(rel_unfilling)) %>% 
  mutate(rel_expansion = logit(rel_expansion)) %>% 
  mutate(rel_stability = logit(rel_stability)) %>% 
  mutate(rel_abandonment = logit(rel_abandonment)) %>% 
  mutate(rel_pioneering = logit(rel_pioneering)) %>% 
  mutate(orig_expansion = logit(orig_expansion)) %>% 
  mutate(orig_stability = logit(orig_stability)) %>% 
  mutate(orig_unfilling = logit(orig_unfilling)) 


# trait analysis ----------------------------------------------------------


covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", "years_since_intro", "niche_breadth_nat",
                "range_size_nat", "lat_dist", "niche_centroid_a_nat", "niche_centroid_b_nat")


for (covariate in covariates) {
  
  print(covariate)
  
    # prepare input data ------------------------------------------------------
    
    input_df <- input_TA_uni %>% 
      select(species_region, species, region, expansion, unfilling, stability, rel_abandonment, rel_pioneering, orig_expansion, orig_unfilling, orig_stability, eval(covariate)) %>% 
      na.omit()
      
      regions <- unique(input_df$region)
      
      input_TA_all_regions <- input_df
      
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
        lm_stability <- ETP(paste("phylolm(stability ~", covariate,", data = input_TA, phy = pac_tree[[1]], model ='lambda')"))
        
        # step model
        step_lm_stability <- phylostep(lm_stability$formula, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        
        
        # ++ unfilling ++
        
        null_unfilling <- phylolm(unfilling ~ 1, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        # full models
        lm_unfilling <- ETP(paste("phylolm(unfilling ~", covariate,", data = input_TA, phy = pac_tree[[1]], model ='lambda')"))
        
        # step model
        step_lm_unfilling <- phylostep(lm_unfilling$formula, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        
        
        # ++ expansion ++
        
        null_expansion <- phylolm(expansion ~ 1, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        # full models
        lm_expansion <- ETP(paste("phylolm(expansion ~", covariate,", data = input_TA, phy = pac_tree[[1]], model ='lambda')"))
        
        # step model
        step_lm_expansion <- phylostep(lm_expansion$formula, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        
        # ++ rel abandonment ++
        
        null_abandonment <- phylolm(rel_abandonment ~ 1, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        # full models
        lm_abandonment <- ETP(paste("phylolm(rel_abandonment ~", covariate,", data = input_TA, phy = pac_tree[[1]], model ='lambda')"))
        
        # step model
        step_lm_abandonment <- phylostep(lm_abandonment$formula, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        
        # ++ rel pioneering ++
        
        null_pioneering <- phylolm(rel_pioneering ~ 1, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        # full models
        lm_pioneering <- ETP(paste("phylolm(rel_pioneering ~", covariate,", data = input_TA, phy = pac_tree[[1]], model ='lambda')"))
        
        # step model
        step_lm_pioneering <- phylostep(lm_pioneering$formula, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        
        
        # ++ original ecospat expansion ++
        
        null_orig_exp <- phylolm(orig_expansion ~ 1, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        # full models
        lm_orig_exp <- ETP(paste("phylolm(orig_expansion ~", covariate,", data = input_TA, phy = pac_tree[[1]], model ='lambda')"))
        
        # step model
        step_lm_orig_exp <- phylostep(lm_orig_exp$formula, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        
        
        # ++ original ecospat unfilling ++
        
        null_orig_unf <- phylolm(orig_unfilling ~ 1, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        # full models
        lm_orig_unf <- ETP(paste("phylolm(orig_unfilling ~", covariate,", data = input_TA, phy = pac_tree[[1]], model ='lambda')"))
        
        # step model
        step_lm_orig_unf <- phylostep(lm_orig_unf$formula, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        
        # ++ original ecospat stability ++
        
        null_orig_stab <- phylolm(orig_stability ~ 1, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        # full models
        lm_orig_stab <- ETP(paste("phylolm(orig_stability ~", covariate,", data = input_TA, phy = pac_tree[[1]], model ='lambda')"))
        
        # step model
        step_lm_orig_stab <- phylostep(lm_orig_stab$formula, data = input_TA, phy = pac_tree[[1]], model = "lambda")
        
        
        
        # save models
        save(null_stability, lm_stability, step_lm_stability,
             null_unfilling, lm_unfilling, step_lm_unfilling,
             null_expansion, lm_expansion, step_lm_expansion,
             null_abandonment, lm_abandonment, step_lm_abandonment,
             null_pioneering, lm_pioneering, step_lm_pioneering,
             null_orig_exp, lm_orig_exp, step_lm_orig_exp,
             null_orig_unf, lm_orig_unf, step_lm_orig_unf,
             null_orig_stab, lm_orig_stab, step_lm_orig_stab,
             file = paste0("results/trait_analysis/univariate/trait_models/",covariate,"_ESU_models_",reg,".RData"))
        
        # variable importance -----------------------------------------------------

        # number of replicates
        nrep <- 99


        # # covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", "range_size_nat",  # , "dispersal",
        # #                 "niche_breadth_nat", "niche_centroid_a_nat", "niche_centroid_b_nat", "years_since_intro", "lat_dist")
        #
        # names of models
        MOD <- c("lm_stability", "lm_unfilling", "lm_expansion", "lm_abandonment", "lm_pioneering", "lm_orig_exp", "lm_orig_unf", "lm_orig_stab")

        output <- list()

        for (i in MOD) {
          modl <- ETP(i)
          respvar <- strsplit(as.character(formula(modl)), " ")[[2]][1]
          explvar <- names(modl$coefficients)[2:length(modl$coefficients)]

          if (!(is.na(explvar[1]) &  explvar[2] == "(Intercept)")) {

            # NOTE: if you include quadratic terms, then some more fiddling might be needed here.

            # Calculate the R2 with a null model for which we fix the lambda value
            if (modl$optpar > 0.000001) {
              # pgls0 <- ETP(paste("pgls(", respvar, "~ 1, data=all_data, lambda=modl$optpar)"))
              pgls0 <- ETP(paste("phylolm(",respvar, "~ 1, data = input_TA, phy = pac_tree[[1]], model ='lambda')"))
              # pgls1 <- ETP(paste("pgls(", formula(modl), ", data=all_data, lambda=modl$optpar)"))
              pgls1 <- ETP(paste("phylolm(", respvar, "~",covariate,", data = input_TA, phy = pac_tree[[1]], model = 'lambda')"))
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
                  mt <- ETP(paste("phylolm(", respvar, "~",covariate,", data=tempdat, phy=pac_tree[[1]], model='lambda')"))
                  rR2[k,j] <- R2(pgls0, mt)[1] ; rR2a[k,j] <- R2(pgls0, mt)[2]
                  cat(k)
                }
                print(j)
              }
            } else {
              pgls0 <- ETP(paste("glm(", respvar, "~ 1, data=input_TA)"))
              pgls1 <- ETP(paste("glm(", respvar, "~",covariate,", data=input_TA)"))
              r2 <- R2glm(pgls1)
              # Calculate the variable importance with variable randomization and fixed lambda
              rR2 <- rR2a <- data.frame(matrix(NA, nrep, length(explvar), T, list(1:nrep, explvar)))
              for (j in explvar) {
                tempdat <- pgls1$data
                for (k in 1:nrep) {
                  tempdat[,j] <- sample(tempdat[,j])
                  mt <- ETP(paste("glm(", respvar, "~",covariate,", data=tempdat)"))
                  rR2[k,j] <- R2glm(mt)[1] ; rR2a[k,j] <- R2glm(mt)[2]
                  cat(k)
                }
                print(j)
              }
            }

            output[[i]] <- list(obs = r2, randR2 = rR2, randR2adj = rR2a)

          } # end of if condition regarding explvar

          print(i)

        } # end of for loop over MOD

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
        save(step.varImp.allmetrics, file = paste0("results/trait_analysis/univariate/variance_importance/VarImp_phylo_trait_models_ESU_",reg,".RData"))

        for (i in MOD) {
          print(i)
          print(ETP(paste0("step.varImp.allmetrics","$",i,"$obs[1]")))
          print(ETP(paste0("step.varImp.allmetrics","$",i,"$Mr2")))
        }

      } # end of for loop over regions
  
} # end of for loop over covariates




#  store results ----------------------------------------------------------


# Make vector of null model names
MOD_null <- sub("step.lm","null",MOD)


for (reg in regions) {
  
  # Make data.frame to summarise results of trait models
  results_TraitAnal_df <- data.frame(matrix(nrow = length(covariates), ncol = length(MOD) + 2))
  names(results_TraitAnal_df) <- c("Region","Trait",paste0(sub("step.lm.","", MOD),"_coef"))
  results_TraitAnal_df[,1] <- reg
  results_TraitAnal_df[,2] <-  covariates
  
  for (covariate in covariates) {
    
    # load models for current covariate and region
    load(paste0("results/trait_analysis/univariate/trait_models/",covariate,"_ESU_models_",reg,".RData"))
    
    for (i in 1:length(MOD)) {
      
      modl <- ETP(MOD[i])
      # store model coefficients
      results_TraitAnal_df[which(results_TraitAnal_df$Trait == covariate), paste0(MOD[i], "_coef")] <- round(as.numeric(modl$coefficients),3)[2]
      
      
    } # end of for loop over MOD
  } # end of for loop over covariates
  
  # save output
  write.csv(results_TraitAnal_df, file = file.path(paste0("results/trait_analysis/univariate/trait_analysis_results/results_TraitAnal_df_ESU_",reg,".csv")), row.names = F)
  
} # end of for loop over regions




# n = species per covariate -----------------------------------------------



for (covariate in covariates) {
  
  
  # prepare input data ------------------------------------------------------
  
  input_df <- input_TA_uni %>% 
    select(species_region, species, region, expansion, unfilling, stability, rel_abandonment, rel_pioneering, orig_expansion, orig_unfilling, orig_stability, eval(covariate)) %>% 
    na.omit()
  
  print(covariate)
  print(table(input_df$region))
  
  # input_spp <- as.numeric(length(input_df %>%
  #                                  distinct(species) %>% 
  #                                  pull(species)))
  
  
    
  
  
  # print(input_spp)
  
  
  
}


covariate <- covariates[10]
  