#' ---------------------------
#
# Purpose of script: compiling all results as individual tables or master table 
# & statistical analysis (differences in the niche dynamcis between regions)
# Author: Anna Rönnfeldt
# Date Created: ~ 2023-12
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------

library(dplyr)
library(stringr) # for str_trim
library(tidyr)


# functions ---------------------------------------------------------------

# function to remove code clutter from scripts when retrieving results from the similarity test
similarity_res <- function(df, spec, reg, setting, metric){
  v <- subset(df, species == spec & region == reg & 
                sim_setting == setting & sim_metric == metric)[,5]
  
  return(v)
}


# required data -----------------------------------------------------------

load("data/species_selection/AC_occs_df.RData")
load("data/species_selection/spp_suitable.RData")
# load("data/occurrence_data/nr_occs_df_after_thinning.RData")
load("data/species_selection/spp_suitable_AC.RData")

# overlap -----------------------------------------------------------------

# empty df to store data in
results_overlap <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(results_overlap) <- c("species", "region", "schoeners_D")

# set counter to keep track of process
counter <- 0

for (spp in spp_suitable) {
  
  counter <- counter + 1
  print(counter)
  
  
  files_spp <- list.files(path = "results/ecospat/niche_overlap/", pattern = spp)
  
  for (file in files_spp) {
    
    # load in niche overlap result
    load(paste0("results/ecospat/niche_overlap/", file)) # object name: D_overlap
    
    # remove ".RData from file name
    file_short <- unlist(strsplit(file, split = "[.]"))[1]
    
    # get the region name
    region_name <- unlist(strsplit(file_short, split = "_"))[3]
    
    # only include results for this niche pair if it is suitable (see previous script)
    if (AC_occs_df[AC_occs_df$species == spp & AC_occs_df$intr_region == region_name, "suitability"] == 1) {
      # add info to df
      results_overlap <- rbind(results_overlap,
                               data.frame(species = spp, 
                                          region = region_name, 
                                          schoeners_D = D_overlap))} # end of if condition
    
  } # end of for loop over files
}

# subset for AC species
results_overlap_AC <- subset(results_overlap, species %in% spp_suitable_AC)

# save results 
save(results_overlap_AC, file = "results/ecospat/niche_overlap_results_AC.RData")

# similarity  --------------------------------------------------------

rm(list = setdiff(ls(), c("path_data", "spp_suitable", "spp_suitable_AC", "similarity_res", "AC_occs_df")))

# 1. similarity test results
# empty df to store data in
results_similarity <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(results_similarity) <- c("species", "region", "sim_setting", "sim_metric", "value")

# sim test settings
settings <- c("sim_conservatism", "sim_shift") 
# metrics for niche dynamics
metrics <- c("D","expansion", "stability", "unfilling")


# set counter to keep track of process
counter <- 0

for (spp in spp_suitable) {
  
  counter <- counter + 1
  print(counter)
  
  for (setting in settings) {
    
    # all niche dynamics files saved in that folder for this species and setting
    files_similarity <- list.files(path = "results/ecospat/niche_similarity/", pattern = spp) 
    
    files_similarity <- files_similarity[grepl(paste0(setting), files_similarity)]
    
    for (file in files_similarity) {
      # load niche dynamic results
      load(paste0("results/ecospat/niche_similarity/",file)) # object name: sim_test_XX
      
      # remove ".RData from file name
      file_short <- unlist(strsplit(file, split = "[.]"))[1]
      
      # get region name
      region_name <- unlist(strsplit(file_short, split = "_"))[4]
      
      # add results to df (looping over the different metrics)
      for (metric in metrics) {
        
        # get results for current metric (object name differ depending on the method settings -> if conditions)
        if (setting == "sim_conservatism") {
          sim_set <- "conservatism"
          sim_value <- sim_test_conservatism[[paste0("p.",metric)]]}
        if (setting == "sim_shift") {
          sim_set <- "shift"
          sim_value <- sim_test_shift[[paste0("p.",metric)]]}
        
        # only include results for this niche pair if it is suitable (see previous script)
        if (AC_occs_df[AC_occs_df$species == spp & AC_occs_df$intr_region == region_name, "suitability"] == 1) {
          
          # add to df
          results_similarity <- rbind(results_similarity,
                                      data.frame(species = spp, 
                                                 region = region_name, 
                                                 sim_setting = sim_set,
                                                 sim_metric = metric, 
                                                 value = sim_value))} # end of if condition
        
      } # end loop over metrics
    } # end loop over files
  } # end of loop over settings
} # end of loop over species


# subset for AC species
results_similarity_AC <- subset(results_similarity, species %in% spp_suitable_AC)

# save results 
save(results_similarity_AC, file = "results/ecospat/niche_similarity_results_AC.RData")


# dynamics ----------------------------------------------------------------

rm(list = setdiff(ls(), c("path_data", "spp_suitable", "spp_suitable_AC", "similarity_res","AC_occs_df")))

# empty df to store data in
results_dynamics <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(results_dynamics) <- c("species", "region", "inter_method", "dyn_metric", "value")

# intersection settings
settings <- c("inter", "whole")
# metrics for niche dynamics
metrics <- c("expansion", "stability", "unfilling")

counter <- 0

for (spp in spp_suitable) {
  
  counter <- counter + 1
  print(counter)
  
    
  for (setting in settings) {
    

    # all niche dynamics files saved in that folder for this species and setting
    files_dynamics <- list.files(path = "results/ecospat/niche_dynamics/", pattern = spp) 
    
    files_dynamics <- files_dynamics[grepl(paste0(setting), files_dynamics)]
    
    for (file in files_dynamics) {
      # load niche dynamic results
      load(paste0("results/ecospat/niche_dynamics/",file)) # object name: niche_dyn_inter
      
      # remove ".RData from file name
      file_short <- unlist(strsplit(file, split = "[.]"))[1]
      
      # get the region
      region_name <- unlist(strsplit(file_short, split = "_"))[5]
      
      for (metric in metrics) {
        
        # get results for current metric
        if (setting == "inter") {
          dyn_value <- niche_dyn_inter[["dynamic.index.w"]][[metric]]
        }else{
          dyn_value <- niche_dyn_whole[["dynamic.index.w"]][[metric]]
        }
        
        # only include results for this niche pair if it is suitable (see previous script)
        if (AC_occs_df[AC_occs_df$species == spp & AC_occs_df$intr_region == region_name, "suitability"] == 1) {
        # add to df
        results_dynamics <- rbind(results_dynamics,
                                  data.frame(species = spp, 
                                             region = region_name,
                                             inter_method = setting, 
                                             dyn_metric = metric, 
                                             value = dyn_value))} # end of if condition
        
      } # end loop over metrics
    } # end loop over files    
  } # end loop over settings
} # end of loop over species


# sort alphabetically (1. species, 2. region, 3. dyn_metric)
results_dynamics <- results_dynamics[order(results_dynamics$species,results_dynamics$region, results_dynamics$dyn_metric),]

# subset for AC species
results_dynamics_AC <- subset(results_dynamics, species %in% spp_suitable_AC)

# save results 
save(results_dynamics_AC, file = "results/ecospat/niche_dynamics_results_AC.RData")

# relative dynamics -------------------------------------------------------
rm(list = setdiff(ls(), c("path_data", "results_dynamics", "spp_suitable", "spp_suitable_AC", "similarity_res")))

specs <- spp_suitable

# specs <- setdiff(spp_suitable, c("Agave sisalana", "Spathodea campanulata"))
# specs <- setdiff(spp_suitable, c("Agave sisalana",
#                                  "Dysphania carinata",
#                                  "Passiflora edulis",
#                                  "Spathodea campanulata"))

metrics <- c("stability", "unfilling", "expansion", "abandonment", "pioneering")

# empty df to store data in
rel_niche_dynamics <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(rel_niche_dynamics) <- c("species", "region", "metric", "percentage", "total")

counter <- 0

for (spec in specs) {
  
  counter <- counter + 1
  print(counter)
  
  regs <- unique(subset(results_dynamics, species == spec)$region)
  
  for (reg in regs) {
    
    inter <- subset(results_dynamics, species == spec & region == reg & inter_method == "inter")
    whole <- subset(results_dynamics, species == spec & region == reg & inter_method == "whole")
    
    e_a <- inter[inter$dyn_metric == "expansion","value"]
    s_a <- inter[inter$dyn_metric == "stability","value"]
    u_a <- inter[inter$dyn_metric == "unfilling","value"]
    
    e <- whole[inter$dyn_metric == "expansion","value"]
    s <- whole[inter$dyn_metric == "stability","value"]
    u <- whole[inter$dyn_metric == "unfilling","value"]
    
    
    # native niche (non-analogue)
    s_x <- 1 - u
    
    # proportion non-analogue invaded niche stability and native stability
    if (s == 0 & s_x == 0) {
     x <- 0
    } else {x <- s / s_x}
    
    
    # total niche (non-analogue)
    t <- u * x + s + e 
    
    # proportion of unfilling, stability and expansion towards total non-analogue niche
    u_rel <- u * x / t
    s_rel <- s / t
    e_rel <- e / t
    
    # native niche (analogue)
    s_y <- 1 - u_a
    
    # proportion analogue invaded niche stability and native stability
    if (s_a == 0 & s_y == 0) {
      y <- 0
    } else {
      if (s_y == 0) {
        s_y <- 0.00000000000000001
        y <- s_a / s_y
      } else {y <- s_a / s_y}}
    
    # total niche (analogue)
    t_a <- u_a * y + s_a + e_a 
    
    # proportion of unfilling, stability and expansion towards total analogue niche
    s_rel_a1 <- s_a / t_a
    e_rel_a1 <- e_a / t_a
    u_rel_a1 <- u_a * y / t_a
    
    if (s_rel == 0 & s_rel_a1 == 0) {
      i <- 0
    } else {i <- s_rel / s_rel_a1}
    
    
    # define target values 
    s_rel_a <- s_rel_a1 * i # stability (%)
    u_rel_a <- u_rel_a1 * i # unfilling (%)
    e_rel_a <- e_rel_a1 * i # expansion (%)
    
    a <- u_rel - u_rel_a # abandonment (%)
    
    
    p <- e_rel - e_rel_a # pioneering (%)
    
    # due to internal calculation "errors", ecospat sometimes returns values that result in negative abandonment/pioneering values
    # set these to zero
    if (a < 0) {
      a <- 0
    }
    if (p < 0) {
      p <- 0
    }
    
    results <- c(s_rel_a, u_rel_a, e_rel_a, a, p)
    
    #  to check whether values add up to 1
    total <- a + u_rel_a + s_rel_a + e_rel_a + p
    
    
    # add info to df
    rel_niche_dynamics <- rbind(rel_niche_dynamics,
                                data.frame(species = rep(spec, 5), 
                                           region = rep(reg, 5),
                                           metric = metrics, 
                                           percentage = results,
                                           total = total))
    
    
  } # end loop over regions
  
} # end loop over species

# subset for AC species
rel_niche_dynamics_AC <- subset(rel_niche_dynamics, species %in% spp_suitable_AC)

# save results 
save(rel_niche_dynamics_AC, file = "results/ecospat/rel_niche_dynamics_results_AC.RData")

# master overview ---------------------------------------------------------
rm(list = setdiff(ls(), c("path_data", "similarity_res", "spp_suitable", "spp_suitable_AC")))

load("results/ecospat/niche_overlap_results_AC.RData")
load("results/ecospat/niche_similarity_results_AC.RData")
load("results/ecospat/niche_dynamics_results_AC.RData")
load("results/ecospat/rel_niche_dynamics_results_AC.RData")

specs <- spp_suitable
# specs <- setdiff(spp_suitable, c("Agave sisalana", "Spathodea campanulata"))

# empty df to store data in
master_results <- data.frame(matrix(ncol = 19, nrow = 0))
colnames(master_results) <- c("species", 
                              "region",
                              "overlap", 
                              "similarity", 
                              "p_exp", 
                              "p_stab", 
                              "p_unf",
                              "expansion", 
                              "stability", 
                              "unfilling",
                              "rel_expansion", 
                              "rel_stability", 
                              "rel_unfilling", 
                              "rel_abandonment", 
                              "rel_pioneering",
                              "p_D_cons", 
                              "p_D_shift",
                              "p_ES_cons", 
                              "p_U_cons")

counter <- 0

for (spec in specs) {
  
  counter <- counter + 1
  print(counter)
  
  # determine regions this species has been introduced to
  regs <- unique(subset(results_dynamics_AC, species == spec)$region)
  
  for (reg in regs) {
    
    # results of the similarity tests with niche conservatism or shift settings
    p_D_cons <- similarity_res(results_similarity_AC, spec, reg, "conservatism", "D")
    p_D_shift <- similarity_res(results_similarity_AC, spec, reg, "shift", "D")
    p_ES_cons <- similarity_res(results_similarity_AC, spec, reg, "conservatism","expansion") 
    p_ES_shift <- similarity_res(results_similarity_AC, spec, reg, "shift", "expansion")
    p_U_cons <- similarity_res(results_similarity_AC, spec, reg, "conservatism", "unfilling")
    p_U_shift <- similarity_res(results_similarity_AC, spec, reg, "shift", "unfilling")
    
    if (p_D_shift < 0.05) {
      similarity <- "shift"
    }else{
      if (p_D_cons < 0.05) {
        similarity <- "conservatism"
      }else{
        similarity <- "neither"
      } # end of inner else
    }# end of outer else
    
    if (p_ES_shift < 0.05) {
      p_exp <- "higher"
      p_stab <- "lower"
    }else{
      if (p_ES_cons < 0.05) {
        p_exp <- "lower"
        p_stab <- "higher"
      }else{
        p_exp <- "neither"
        p_stab <- "neither"
      } # end of inner else
    } # end of outer else
    
    if (p_U_shift < 0.05) {
      p_unf <- "higher"
    }else{
      if (p_U_cons < 0.05) {
        p_unf <- "lower"
      }else{
        p_unf <- "neither"
      } # end of inner else
    } # end of outer else
    
    # SES
    
    
    # add info to df
    master_results <- rbind(master_results,
                            data.frame(species = spec,
                                       region = as.factor(reg),
                                       overlap = subset(results_overlap_AC, species == spec & region == reg)[,3],
                                       similarity = similarity,
                                       p_exp = p_exp,
                                       p_stab = p_stab,
                                       p_unf = p_unf,
                                       expansion = subset(results_dynamics_AC, species == spec & region == reg & inter_method == "inter" & dyn_metric == "expansion")[,5],
                                       stability = subset(results_dynamics_AC, species == spec & region == reg & inter_method == "inter" & dyn_metric == "stability")[,5],
                                       unfilling = subset(results_dynamics_AC, species == spec & region == reg & inter_method == "inter" & dyn_metric == "unfilling")[,5],
                                       rel_expansion = subset(rel_niche_dynamics_AC, species == spec & region == reg & metric == "expansion")[,4],
                                       rel_stability = subset(rel_niche_dynamics_AC, species == spec & region == reg & metric == "stability")[,4],
                                       rel_unfilling = subset(rel_niche_dynamics_AC, species == spec & region == reg & metric == "unfilling")[,4],
                                       rel_abandonment = subset(rel_niche_dynamics_AC, species == spec & region == reg & metric == "abandonment")[,4],
                                       rel_pioneering = subset(rel_niche_dynamics_AC, species == spec & region == reg & metric == "pioneering")[,4],
                                       p_D_cons = p_D_cons,
                                       p_D_shift = p_D_shift,
                                       p_ES_cons = p_ES_cons,
                                       p_U_cons = p_U_cons))
    
  } # end of region loop
} # end of species loop
    

# subset for AC species
master_results_AC <- subset(master_results, species %in% spp_suitable_AC)


# save results 
save(master_results_AC, file = "results/ecospat/master_results_AC.RData")



# stand. ESU --------------------------------------------------------------

# standardise expansion, stability, and unfilling values so that they refer to 
# the entirety of the analogue niche space 

stand_ESU <- master_results_AC %>%
  mutate(region = factor(region, levels = c("pac", "afr", "ate", "atr", "aus", "eur", "nam", "sam"))) %>%
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(expansion = rel_expansion / total_esu) %>%
  mutate(stability = rel_stability / total_esu) %>%
  mutate(unfilling = rel_unfilling / total_esu) %>%
  select(c(species, region, unfilling, stability, expansion)) %>%
  pivot_longer(!c(species, region), names_to = "metric", values_to = "percentage") %>%
  replace(is.na(.), 0) %>%
  mutate(across(!c(species,percentage), as.factor)) %>%
  mutate(metric = factor(metric, levels = c("unfilling", "stability", "expansion"))) %>% 
  mutate(percentage = percentage *100)

save(stand_ESU, file = "results/ecospat/stand_ESU.RData")

# percentage niche conservatism -------------------------------------------

# empty df to store data in
perc_df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(perc_df) <- c("region", "nr_species","nr_cons","nr_exp_lower", "nr_stab_higher", "nr_unf_lower", "perc_con")

# region names
regs <- unique(master_results_AC$region)

for (reg in regs) {
  df <- subset(master_results_AC, region == reg)
  spp_all <- nrow(df)
  spp_cons <- nrow(df[df$similarity == "conservatism",])
  spp_exp <- nrow(df[df$p.exp == "lower",])
  spp_stab <- nrow(df[df$p.stab == "higher",])
  spp_unf <- nrow(df[df$p.unf == "lower",])
  
  perc_df <- rbind(perc_df,
                   data.frame(region = as.factor(reg),
                              nr_species = spp_all,
                              nr_cons = spp_cons,
                              nr_exp_lower = spp_exp,
                              nr_stab_higher = spp_stab,
                              nr_unf_lower = spp_unf,
                              perc_con = spp_cons/spp_all,
                              perc_exp = spp_exp/spp_all,
                              perc_stab = spp_stab/spp_all,
                              perc_unf = spp_unf/spp_all))
}

# same for AC species
# empty df to store data in
perc_df_AC <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(perc_df) <- c("region", "nr_species","nr_cons","nr_exp_lower", "nr_stab_higher", "nr_unf_lower", "perc_con")

# region names
regs <- unique(master_results_AC$region)

for (reg in regs) {
  df <- subset(master_results_AC, region == reg)
  spp_all <- nrow(df)
  spp_cons <- nrow(df[df$similarity == "conservatism",])
  spp_exp <- nrow(df[df$p.exp == "lower",])
  spp_stab <- nrow(df[df$p.stab == "higher",])
  spp_unf <- nrow(df[df$p.unf == "lower",])
  
  perc_df_AC <- rbind(perc_df_AC,
                   data.frame(region = as.factor(reg),
                              nr_species = spp_all,
                              nr_cons = spp_cons,
                              nr_exp_lower = spp_exp,
                              nr_stab_higher = spp_stab,
                              nr_unf_lower = spp_unf,
                              perc_con = spp_cons/spp_all,
                              perc_exp = spp_exp/spp_all,
                              perc_stab = spp_stab/spp_all,
                              perc_unf = spp_unf/spp_all))
}

# save results
save(perc_df_AC, file = "results/ecospat/percentages_niche_conservatism_AC.RData")



# statistical analysis ----------------------------------------------------

# test for differences between Pacific Islands and the other regions

# load("results/ecospat/master_results_AC.RData")

# select relevant columns and calulate standardised stability, unfilling, and expansion 
master_results_AC <- master_results_AC %>%
  mutate(region = factor(region, levels = c("pac", "afr", "ate", "atr", "aus", "eur", "nam", "sam"))) %>% # sets pac as "intercept"
  select(c(species, region, overlap, rel_expansion, rel_stability, rel_unfilling, rel_abandonment, rel_pioneering)) %>%
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(stand_expansion = rel_expansion / total_esu) %>%
  mutate(stand_stability = rel_stability / total_esu) %>%
  mutate(stand_unfilling = rel_unfilling / total_esu) 


# working with the rel. dynamicsfor abandonment and pioneering
m_abandonment <- glm(rel_abandonment ~ region, data = master_results_AC, family = "binomial")
m_pioneering <- glm(rel_pioneering ~ region, data = master_results_AC, family = "binomial")

# working with the standardised ESU values (based on rel. dynamics)
m_expansion <- glm(stand_expansion ~ region, data = master_results_AC, family = "binomial")
m_stability <- glm(stand_stability ~ region, data = master_results_AC, family = "binomial")
m_unfilling <- glm(stand_unfilling ~ region, data = master_results_AC, family = "binomial")



anova(m_expansion)
summary(m_expansion)

anova(m_stability)
summary(m_stability)

anova(m_unfilling)
summary(m_unfilling)

anova(m_abandonment)
summary(m_abandonment)

anova(m_pioneering)
summary(m_pioneering)


summary(subset(master_results_AC, region == "sam"))
