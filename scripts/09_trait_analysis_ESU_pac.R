library(dotwhisker)
library(dplyr)
library(phylolm)
library(stringr)

# preamble ----------------------------------------------------------------

rm(list = ls())

# path to transfer folder
# path_transfer <- "Y:/AG26/Transfer/Holle_Roennfeldt/"
path_transfer <- "T:/Holle_Roennfeldt/"

logit <- function(x) {x = ifelse(x < 0.0001,0.0001,ifelse(x > 0.9999,.9999,x));log(x/(1 - x))}

# load required data ------------------------------------------------------

load(paste0(path_transfer, "trait_data_processed/species_pacific_traits_GIFT.RData")) 
load("results/ecospat/master_results.RData")

# georgaphic traits:
load("data/trait_analysis/native_centroid.RData")
load("data/trait_analysis/native_range_size.RData")

# phylogeny
phylo_pacific <- read.tree("data/Phylogeny.tre", keep.multi = TRUE)



# prepare stand ESU data --------------------------------------------------

stand_ESU_wide <- master_results %>%
  mutate(region = factor(region, levels = c("pac", "afr", "ate", "atr", "aus", "eur", "nam", "sam"))) %>%
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(expansion = rel_expansion / total_esu) %>%
  mutate(stability = rel_stability / total_esu) %>%
  mutate(unfilling = rel_unfilling / total_esu) %>%
  select(c(species, region, unfilling, stability, expansion))

# prepare trait data ------------------------------------------------------

trait_df <- species_pacific_traits_GIFT %>%
  select(species_orig, mean_height_GIFT, mean_seedmass_GIFT, growth_form_GIFT, lifecycle_GIFT, max_elev_range_GIFT) %>%
  rename_with(stringr::str_replace, 
              pattern = "_GIFT", replacement = "", 
              matches("_GIFT")) %>%
  rename("species" = "species_orig") %>%
  # add geographic traits
  left_join(select(native_centroid_df, species, lat_centroid, lon_centroid), by = "species") %>%
  left_join(select(native_range_df, species, range_both), by = "species") %>%
  rename("range_size" = "range_both")

# recode the categorical traits

# growth form
trait_df[trait_df == "herb"] <- 1
trait_df[trait_df == "shrub"] <- 2
trait_df[trait_df == "tree"] <- 3

# life-cycle
trait_df[trait_df == "annual"] <- 1
trait_df[trait_df == "biennial"] <- 2
trait_df[trait_df == "perennial"] <- 3


# make sure all trait columns are numeric
trait_df <- trait_df %>%
  mutate(across(!species, as.numeric))

# add ecospat results -----------------------------------------------------

trait_eco_df <- stand_ESU_wide %>%
  select(c(species, region, expansion, stability, unfilling)) %>%
  left_join(trait_df, by = "species") %>%
  na.omit() 



# standardise trait data --------------------------------------------------

# Standardise traits
trait_eco_df$mean_height <- scale(as.numeric(trait_eco_df$mean_height))
trait_eco_df$mean_seedmass <- scale(as.numeric(trait_eco_df$mean_seedmass))
trait_eco_df$max_elev_range <- scale(as.numeric(trait_eco_df$max_elev_range))
trait_eco_df$range_size <- scale(as.numeric(trait_eco_df$range_size))
trait_eco_df$lat_centroid <- trait_eco_df$lat_centroid/90
trait_eco_df$lon_centroid <- trait_eco_df$lon_centroid/180

# Logit-transform response variables
trait_eco_df$unfilling <- logit(trait_eco_df$unfilling)
trait_eco_df$stability <- logit(trait_eco_df$stability)
trait_eco_df$expansion <- logit(trait_eco_df$expansion)



# subset for pac region
trait_eco_df <- trait_eco_df %>% filter(region == "pac")

spp_traits <- unique(trait_eco_df$species)

# prepare phylogeny -------------------------------------------------------

no_match <- trait_eco_df$species[!sub(' ','_',trait_eco_df$species) %in% phylo_pacific[[1]][["tip.label"]]]

phylo_subset <- drop.tip(phylo_pacific, phylo_pacific$tip.label[!phylo_pacific$tip.label %in% sub(' ','_',trait_eco_df$species)])


#add rownames - needed for matching phylogenetic information
rownames(trait_eco_df) = sub(' ','_', trait_eco_df$species)



# define models -----------------------------------------------------------

# stability
null_stability <- phylolm(stability ~ 1, data = trait_eco_df, phy = phylo_subset[[1]], model = "lambda", lower.bound = 10^-9)

# full models
lm_stability <- phylolm(stability ~ mean_height + mean_seedmass + growth_form + lifecycle + max_elev_range + lat_centroid + lon_centroid + range_size, 
                        data = trait_eco_df, phy = phylo_subset[[1]], model = "lambda", lower.bound = 10^-9)


# step model
step_lm_stability <- phylostep(lm_stability$formula, data = trait_eco_df, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_stability$residuals^2)/sum(null_stability$residuals^2)
1 - sum(step_lm_stability$residuals^2)/sum(null_stability$residuals^2)


#'---------------------------------


# unfilling
null_unfilling <- phylolm(unfilling ~ 1, data = trait_eco_df, phy = phylo_subset[[1]], model = "lambda", lower.bound = 10^-9)

# full models
lm_unfilling <- phylolm(unfilling ~ mean_height + mean_seedmass + growth_form + lifecycle + max_elev_range + lat_centroid + lon_centroid + range_size, 
                        data = trait_eco_df, phy = phylo_subset[[1]], model = "lambda", lower.bound = 10^-9)


# step model
step_lm_unfilling <- phylostep(lm_unfilling$formula, data = trait_eco_df, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_unfilling$residuals^2)/sum(null_unfilling$residuals^2)
1 - sum(step_lm_unfilling$residuals^2)/sum(null_unfilling$residuals^2)

#'---------------------------------


# expansion
null_expansion <- phylolm(expansion ~ 1, data = trait_eco_df, phy = phylo_subset[[1]], model = "lambda", lower.bound = 10^-9)

# full models
lm_expansion <- phylolm(expansion ~ mean_height + mean_seedmass + growth_form + lifecycle + max_elev_range + lat_centroid + lon_centroid + range_size, 
                        data = trait_eco_df, phy = phylo_subset[[1]], model = "lambda", lower.bound = 10^-9)


# step model
step_lm_expansion <- phylostep(lm_expansion$formula, data = trait_eco_df, phy = phylo_subset[[1]], model = "lambda")

# Explained deviance
1 - sum(lm_expansion$residuals^2)/sum(null_expansion$residuals^2)
1 - sum(step_lm_expansion$residuals^2)/sum(null_expansion$residuals^2)


save(null_stability, lm_stability, step_lm_stability,
     null_unfilling, lm_unfilling, step_lm_unfilling,
     null_expansion, lm_expansion, step_lm_expansion,
     file = "results/trait_analysis/phylo_trait_models_ESU.RData")

# variable importance -----------------------------------------------------

rm(list = setdiff(ls(), c("phylo_subset", "trait_eco_df")))

load("results/trait_analysis/phylo_trait_models_ESU.RData")

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


covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", "max_elev_range", "lon_centroid", "lat_centroid", "range_size")

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
    str_replace("3", "") 
  
  explvar <- unique(explvar)
  
  # NOTE: if you include quadratic terms, then some more fiddling might be needed here.
  
  # Calculate the R2 with a null model for which we fix the lambda value
  if (modl$optpar > 0.000001) {
    # pgls0 <- ETP(paste("pgls(", respvar, "~ 1, data=all_data, lambda=modl$optpar)"))
    pgls0 <- ETP(paste("phylolm(",respvar, "~ 1, data = trait_eco_df, phy = phylo_subset[[1]], model ='lambda')"))
    # pgls1 <- ETP(paste("pgls(", formula(modl), ", data=all_data, lambda=modl$optpar)"))
    pgls1 <- ETP(paste("phylolm(",formula(modl), ", data = trait_eco_df, phy = phylo_subset[[1]], model = 'lambda')"))
    r2 <- R2(pgls0, pgls1)
    # Calculate the variable importance with variable randomization and fixed lambda
    rR2 <- rR2a <- data.frame(matrix(NA, nrep, length(explvar), T, list(1:nrep, explvar)))
    for (j in explvar) {
      # tempdat <- pgls1$data
      tempdat <- trait_eco_df
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
    pgls0 <- ETP(paste("glm(", respvar, "~ 1, data=trait_eco_df)"))
    pgls1 <- ETP(paste("glm(", formula(modl), ", data=trait_eco_df)"))
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
save(step.varImp.allmetrics, file = "results/trait_analysis/VarImp_phylo_trait_models_ESU.RData")

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
  results_TraitAnal_df[which(covariates %in% explvar)+1,1+i*4] <- ifelse(varimp<0,0,varimp)
}

write.csv(results_TraitAnal_df, file = file.path("results/trait_analysis/results_TraitAnal_df_ESU.csv"), row.names=F)

# plot effect size --------------------------------------------------------

rm(list = ls())

traits_res <- read.csv("results/trait_analysis/results_TraitAnal_df_ESU.csv")

# trait analyses - plot effect sizes:
#trait_res_BBS <- read.csv(file.path(data_dir,"results_TraitAnal_df_US.csv"))
#trait_res_EBBA <- read.csv(file.path(data_dir,"results_TraitAnal_df_Europe.csv"))
#traits_res <- rbind(data.frame(trait_res_BBS, region='US'),data.frame(trait_res_EBBA, region='Europe'))

covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", "max_elev_range", "lon_centroid", "lat_centroid", "range_size")

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



windows(w = 4,h = 3)

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
  xlab("Coefficient estimate") + 
  ylab("") + 
  scale_color_manual(values = c("unfilling" = "#FFE875", "stability" = "#A3DDEF","expansion" = "#87CF87"), name = "model"))

ggsave("plots/results/Coefficient estimate_ESU.pdf", p, 
       bg = "transparent",
       width = 6,
       height = 5,
       units = "cm")
