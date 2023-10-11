library(stringr) # for str_trim

# path_output <- "/import/ecoc9z/data-zurell/roennfeldt/C1/ouput/"
path_data <- "Y:/roennfeldt/C1/data/ecospat/"


# functions ---------------------------------------------------------------

# function to remove code clutter from scripts when retrieving results from the similarity test
similarity_res <- function(df, spec, reg, setting, metric){
  v <- subset(df, species == spec & region == reg & 
                sim_setting == setting & sim_metric == metric)[,5]
  
  return(v)
}

# overlap -----------------------------------------------------------------

# empty df to store data in
results_overlap <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(results_overlap) <- c("species", "region", "schoeners_D")

# all niche overlap files saved in that folder
files_overlap <- list.files(path = paste0(path_data,"niche_overlap/"), pattern = ".RData")

# set counter to keep track of process
counter <- 0

for (file in files_overlap) {
  
  counter <- counter + 1
  print(counter)
  # load in niche overlap result
  load(paste0(path_data, "niche_overlap/",file)) # object name: D_overlap
  
  # remove ".RData from file name
  file_short <- unlist(strsplit(file, split = "[.]"))[1]
  
  # get the species and region name
  spec_name <- unlist(strsplit(file_short, split = "_"))[2]
  region_name <- unlist(strsplit(file_short, split = "_"))[3]
  
  # add info to df
  results_overlap <- rbind(results_overlap,
                           data.frame(species = spec_name, 
                                      region = region_name, 
                                      schoeners_D = D_overlap))
}

# save results 
save(results_overlap, file = "results/niche_overlap_overview.RData")


# similarity & SES --------------------------------------------------------

rm(list = setdiff(ls(), "path_data"))

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
for (setting in settings) {
  
  counter <- counter + 1
  print(counter)
  
  # all niche dynamics files saved in that folder
  files_similarity <- list.files(path = paste0(path_data, "niche_similarity/"), pattern = setting)
  
  for (file in files_similarity) {
    # load niche dynamic results
    load(paste0(path_data,"niche_similarity/",file)) # object name: sim_test_XX
    
    # remove ".RData from file name
    file_short <- unlist(strsplit(file, split = "[.]"))[1]
    
    # get species and region name
    spec_name <- unlist(strsplit(file_short, split = "_"))[3]
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
      
      # add to df
      results_similarity <- rbind(results_similarity,
                                  data.frame(species = spec_name, 
                                             region = region_name, 
                                             sim_setting = sim_set,
                                             sim_metric = metric, 
                                             value = sim_value))
      
    } # end loop over metrics
  } # end loop over files
} # end loop over settings


# save results 
save(results_similarity, file = "results/niche_similarity_overview.RData")


# 2. SES based on similarity test results

rm(list = setdiff(ls(), "path_data"))

# empty df to store data in
results_ses <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(results_ses) <- c("species", "region", "sim_setting", "sim_metric", "value")


settings <- c("ses_conservatism", "ses_shift") 
metrics <- c("rank.D","rank.I", "z.D", "z.I")


for (setting in settings) {
  
  # all niche dynamics files saved in that folder
  files_ses <- list.files(path = paste0(path_data, "niche_similarity/"), pattern = setting)
  
  for (file in files_ses) {
    # load niche dynamic results
    load(paste0(path_data,"niche_similarity/",file)) # object name: sim_test_XX
    
    # remove ".RData from file name
    file_short <- unlist(strsplit(file, split = "[.]"))[1]
    
    # get species and region name
    spec_name <- unlist(strsplit(file_short, split = "_"))[3]
    region_name <- unlist(strsplit(file_short, split = "_"))[4]
    
    # accidentally saved the ses_shift results with a whitespace in front of the species name 
    # removing the whitespace:
    if (setting == "ses_shift") {
      spec_name <- str_trim(spec_name, "left") 
    }
    
    # get ses results for the different settings/metrics
    
    for (metric in metrics) {
      
      
      if (setting == "ses_conservatism") {
        sim_set <- "conservatism"
        sim_value <- ses_conservatism[[paste0("ses.",metric)]]
        } # end of if over setting == "conservatism"
      
      if (setting == "ses_shift") {
        sim_set <- "shift"
        sim_value <- ses_shift[[paste0("ses.",metric)]]
        } # end of if over setting == "shift"
      
      
      
      # add to df
      results_ses <- rbind(results_ses,
                                  data.frame(species = spec_name, 
                                             region = region_name, 
                                             sim_setting = sim_set,
                                             sim_metric = metric, 
                                             value = sim_value))
      
    } # end loop over metrics
    
  } # end loop over files
} # end loop over settings

# save results 
save(results_ses, file = "results/niche_ses_overview.RData")

# dynamics ----------------------------------------------------------------

rm(list = setdiff(ls(), "path_data"))

# empty df to store data in
results_dynamics <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(results_dynamics) <- c("species", "region", "inter_method", "dyn_metric", "value")

# intersection settings
settings <- c("inter", "whole")
# metrics for niche dynamics
metrics <- c("expansion", "stability", "unfilling")

for (setting in settings) {
  
  # all niche dynamics files saved in that folder
  files_dynamics <- list.files(path = paste0(path_data, "niche_dynamics/"), pattern = setting)
  
  
  for (file in files_dynamics) {
    # load niche dynamic results
    load(paste0(path_data, "niche_dynamics/",file)) # object name: niche_dyn_inter
    
    # remove ".RData from file name
    file_short <- unlist(strsplit(file, split = "[.]"))[1]
    
    # get the intersection method, species, and region
    spec_name <- unlist(strsplit(file_short, split = "_"))[4]
    region_name <- unlist(strsplit(file_short, split = "_"))[5]
    
    for (metric in metrics) {
      
      # get results for current metric
      if (setting == "inter") {
        dyn_value <- niche_dyn_inter[["dynamic.index.w"]][[metric]]
      }else{
        dyn_value <- niche_dyn_whole[["dynamic.index.w"]][[metric]]
      }
      
      # add to df
      results_dynamics <- rbind(results_dynamics,
                                data.frame(species = spec_name, 
                                           region = region_name,
                                           inter_method = setting, 
                                           dyn_metric = metric, 
                                           value = dyn_value))
      
    } # end loop over metrics
  } # end loop over files    
} # end loop over settings

# sort alphabetically (1. species, 2. region, 3. dyn_metric)
results_dynamics <- results_dynamics[order(results_dynamics$species,results_dynamics$region, results_dynamics$dyn_metric),]

# save results 
save(results_dynamics, file = "results/niche_dynamics_overview.RData")


# relative dynamics -------------------------------------------------------
rm(list = setdiff(ls(), c("path_data", "results_dynamics")))

specs_all <- unique(results_dynamics$species)

specs <- setdiff(specs_all, c("Atriplex suberecta", 
                              "Dysphania carinata", 
                              "Fuchsia boliviana", 
                              "Heterotheca grandiflora",
                              "Passiflora edulis",
                              "Spathodea campanulata"))

metrics <- c("stability", "unfilling", "expansion", "abandonment", "pioneering")

# empty df to store data in
rel_niche_dynamics <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(rel_niche_dynamics) <- c("species", "region", "metric", "percentage", "total")



for (spec in specs) {
  
  print(spec)
  
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
    x <- s / s_x
    
    # total niche (non-analogue)
    t <- u * x + s + e 
    
    # proportion of unfilling, stability and expansion towards total non-analogue niche
    u_rel <- u * x / t
    s_rel <- s / t
    e_rel <- e / t
    
    # native niche (analogue)
    s_y <- 1 - u_a
    
    # propotion analogue invaded niche stability and native stability
    y <- s_a / s_y
    
    # total niche (analogue)
    t_a <- u_a * y + s_a + e_a 
    
    # proportion of unfilling, stability and expansion towards total analogue niche
    s_rel_a1 <- s_a / t_a
    e_rel_a1 <- e_a / t_a
    u_rel_a1 <- u_a * y / t_a
    
    
    i <- s_rel / s_rel_a1
    
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

# save results 
save(rel_niche_dynamics, file = "results/rel_niche_dynamics_overview.RData")

# master overview ---------------------------------------------------------
rm(list = setdiff(ls(), c("path_data", "similarity_res")))

load("results/niche_overlap_overview.RData")
load("results/niche_similarity_overview.RData")
load("results/niche_ses_overview.RData")
load("results/niche_dynamics_overview.RData")
load("results/rel_niche_dynamics_overview.RData")

specs_all <- unique(results_overlap$species)

specs <- setdiff(specs_all, c("Atriplex suberecta", 
                              "Dysphania carinata", 
                              "Fuchsia boliviana", 
                              "Heterotheca grandiflora",
                              "Passiflora edulis",
                              "Spathodea campanulata"))

# empty df to store data in
master_results <- data.frame(matrix(ncol = 19, nrow = 0))
colnames(master_results) <- c("species", 
                              "region",
                              "overlap", 
                              "similarity", 
                              "p.exp", 
                              "p.stab", 
                              "p.unf",
                              "expansion", 
                              "stability", 
                              "unfilling",
                              "rel.expansion", 
                              "rel.stability", 
                              "rel.unfilling", 
                              "rel.abandonment", 
                              "rel.pioneering",
                              "p.D.cons", 
                              "p.D.shift",
                              "p.ES.cons", 
                              "p.U.cons")

for (spec in specs) {

  # determine regions this species has been introduced to
  regs <- unique(subset(results_dynamics, species == spec)$region)
  
  for (reg in regs) {
    
    # results of the similarity tests with niche conservatism or shift settings
    p_D_cons <- similarity_res(results_similarity, spec, reg, "conservatism", "D")
    p_D_shift <- similarity_res(results_similarity, spec, reg, "shift", "D")
    p_ES_cons <- similarity_res(results_similarity, spec, reg, "conservatism","expansion") 
    p_ES_shift <- similarity_res(results_similarity, spec, reg, "shift", "expansion")
    p_U_cons <- similarity_res(results_similarity, spec, reg, "conservatism", "unfilling")
    p_U_shift <- similarity_res(results_similarity, spec, reg, "shift", "unfilling")
    
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
                                       overlap = subset(results_overlap, species == spec & region == reg)[,3],
                                       similarity = similarity,
                                       p.exp = p_exp,
                                       p.stab = p_stab,
                                       p.unf = p_unf,
                                       expansion = subset(results_dynamics, species == spec & region == reg & inter_method == "inter" & dyn_metric == "expansion")[,5],
                                       stability = subset(results_dynamics, species == spec & region == reg & inter_method == "inter" & dyn_metric == "stability")[,5],
                                       unfilling = subset(results_dynamics, species == spec & region == reg & inter_method == "inter" & dyn_metric == "unfilling")[,5],
                                       rel.expansion = subset(rel_niche_dynamics, species == spec & region == reg & metric == "expansion")[,4],
                                       rel.stability = subset(rel_niche_dynamics, species == spec & region == reg & metric == "stability")[,4],
                                       rel.unfilling = subset(rel_niche_dynamics, species == spec & region == reg & metric == "unfilling")[,4],
                                       rel.abandonment = subset(rel_niche_dynamics, species == spec & region == reg & metric == "abandonment")[,4],
                                       rel.pioneering = subset(rel_niche_dynamics, species == spec & region == reg & metric == "pioneering")[,4],
                                       p.D.cons = p_D_cons,
                                       p.D.shift = p_D_shift,
                                       p.ES.cons = p_ES_cons,
                                       p.U.cons = p_U_cons))
    
  } # end of region loop
} # end of species loop
    
# save results 
save(master_results, file = "results/master_results.RData")

# percentage niche conservatism -------------------------------------------

# empty df to store data in
perc_df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(perc_df) <- c("region", "nr_species","nr_cons","nr_exp_lower", "nr_stab_higher", "nr_unf_lower", "perc_con")

# region names
regs <- unique(master_results$region)
# c("Pacific Islands", "Africa", "Asia", "Australia", "Oceania", "North America", "South America", "Europe")
for (reg in regs) {
  df <- subset(master_results, region == reg)
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


save(perc_df, file = "results/percentages_niche_conservatism.RData")
