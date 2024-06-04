library(Cairo)
library(corrplot)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)
library(tidyr)

rm(list = ls())


select <- dplyr::select
source("scripts/functions.R")

regions <- c("afr", "ate", "atr", "aus", "eur", "nam", "pac", "sam")

covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", 
                "years_since_intro", 
                "niche_breadth_nat",  "range_size_nat", "lat_dist",
                "niche_centroid_a_nat", "niche_centroid_b_nat")


traits_res_oe <- tibble()
traits_res_os <- tibble()
traits_res_ou <- tibble()


for (reg in regions) {
  
  print(reg)
  
  # load in results 
  traits_res <- read.csv(paste0("results/trait_analysis/main_analysis/results_TraitAnal_df_ESU_",reg,".csv"))
  
  traits_res_reg_oe <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "orig_exp", term = Trait, estimate = orig_exp_coef, std.error = orig_exp_stderr, statistic = orig_exp_coef/orig_exp_stderr, p.value = orig_exp_p, varimp = orig_exp_varimp) %>%
    select(region,model, term, estimate, std.error, statistic, p.value, varimp) %>% 
    filter(term %in% covariates)
  
  traits_res_reg_os <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "orig_stab", term = Trait, estimate = orig_stab_coef, std.error = orig_stab_stderr, statistic = orig_stab_coef/orig_stab_stderr, p.value = orig_stab_p, varimp = orig_stab_varimp) %>%
    select(region, model, term, estimate, std.error, statistic, p.value, varimp)  %>% 
    filter(term %in% covariates)
  
  traits_res_reg_ou <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "orig_unf", term = Trait, estimate = orig_unf_coef, std.error = orig_unf_stderr, statistic = orig_unf_coef/orig_unf_stderr, p.value = orig_unf_p, varimp = orig_unf_varimp) %>%
    select(region, model, term, estimate, std.error, statistic, p.value, varimp)  %>% 
    filter(term %in% covariates)
  
  # add results to main table
  traits_res_oe <- rbind(traits_res_oe, traits_res_reg_oe)
  traits_res_os <- rbind(traits_res_os, traits_res_reg_os)
  traits_res_ou <- rbind(traits_res_ou, traits_res_reg_ou)
  
} # end of for loop

rm(traits_res_reg_oe, traits_res_reg_os, traits_res_reg_ou, traits_res)

traits_res_ou[traits_res_ou$region == "eur" & traits_res_ou$term == "mean_seedmass", "estimate"] <- -3.011
traits_res_oe[traits_res_oe$region == "pac" & traits_res_oe$term == "mean_seedmass", "estimate"] <- -3.011
traits_res_os[traits_res_os$region == "pac" & traits_res_os$term == "lat_dist", "estimate"] <- -3.011
traits_res_os[traits_res_os$region == "eur" & traits_res_os$term == "lat_dist", "estimate"] <- -3.011
traits_res_os[traits_res_os$region == "ate" & traits_res_os$term == "lat_dist", "estimate"] <- -3.011
traits_res_os[traits_res_os$region == "nam" & traits_res_os$term == "lat_dist", "estimate"] <- -3.011


traits_res_os[traits_res_os$region == "pac" & traits_res_os$term == "mean_seedmass", "estimate"] <- 1.501
traits_res_oe[traits_res_oe$region == "pac" & traits_res_oe$term == "lat_dist", "estimate"] <- 1.501
traits_res_oe[traits_res_oe$region == "ate" & traits_res_oe$term == "lat_dist", "estimate"] <- 1.501
traits_res_oe[traits_res_oe$region == "nam" & traits_res_oe$term == "lat_dist", "estimate"] <- 1.501
traits_res_oe[traits_res_oe$region == "eur" & traits_res_oe$term == "lat_dist", "estimate"] <- 1.501

min_oe <- round(min(traits_res_oe$estimate, na.rm = TRUE), 2)
max_oe <- round(max(traits_res_oe$estimate, na.rm = TRUE), 2)
min_os <- round(min(traits_res_os$estimate, na.rm = TRUE), 2)
max_os <- round(max(traits_res_os$estimate, na.rm = TRUE), 2)
min_ou <- round(min(traits_res_ou$estimate, na.rm = TRUE), 2)
max_ou <- round(max(traits_res_ou$estimate, na.rm = TRUE), 2)


oe_efs_mtx <- traits_res_oe %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

oe_imp_mtx <- traits_res_oe %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)


os_efs_mtx <- traits_res_os %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

os_imp_mtx <- traits_res_os %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)


ou_efs_mtx <- traits_res_ou %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

ou_imp_mtx <- traits_res_ou %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

rownames(oe_efs_mtx) <- covariates
rownames(oe_imp_mtx) <- covariates
rownames(os_efs_mtx) <- covariates
rownames(os_imp_mtx) <- covariates
rownames(ou_efs_mtx) <- covariates
rownames(ou_imp_mtx) <- covariates

# prepare r² output -------------------------------------------------------


r2_oe <- NULL
r2_os <- NULL
r2_ou <- NULL

for (reg in regions) {
  
  df_res <- read.csv(paste0("results/trait_analysis/main_analysis/results_TraitAnal_df_ESU_",reg,".csv"))
  
  r2_oe <- c(r2_oe, round(df_res[df_res$Trait == "R2", "orig_exp_coef"], 2))
  r2_os <- c(r2_os, round(df_res[df_res$Trait == "R2", "orig_stab_coef"], 2))
  r2_ou <- c(r2_ou, round(df_res[df_res$Trait == "R2", "orig_unf_coef"], 2))
  
} # end of for loop over regions

r2_oe <- r2_oe %>% unlist()
r2_os <- r2_os %>% unlist()
r2_ou <- r2_ou %>% unlist()



col_lim <- c(-3.01, 1.5)

txt_col <- "black"
na_col <- "white"
col <- rev(COL2('RdBu', 200))

col_names <- c("Africa (n = 124)",
               "temp. Asia (n = 95)",
               "trop. Asia (n = 78)",
               "Australasia (n = 124)",
               "Europe (n = 56)",
               "N. America (n = 110)",
               "Pacific Islands (n = 143)",
               "S. America (n = 41)")


row_names <- c("Plant height", "Seed mass", "Growth form", "Life cycle",  "Residence time",
               "Native niche breadth", "Native range size" ,  "Distance lat. centroids", 
               "Native niche centroid 1", "Native niche centroid 2")


corrplot_mod(m_imp = oe_imp_mtx,
             m_efs = oe_efs_mtx,
             r2names = r2_oe,
             mar = c(2,2,3,2),
             cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             cl.offset = -1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)


corrplot_mod(m_imp = os_imp_mtx,
             m_efs = os_efs_mtx,
             r2names = r2_os,
             mar = c(2,2,3,2),
             cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             cl.offset = -1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

corrplot_mod(m_imp = ou_imp_mtx,
             m_efs = ou_efs_mtx,
             r2names = r2_ou,
             mar = c(2,2,3,2),
             cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             cl.offset = -1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)



# full models -------------------------------------------------------------

rm(list = ls())


select <- dplyr::select
source("scripts/functions.R")

regions <- c("afr", "ate", "atr", "aus", "eur", "nam", "pac", "sam")

covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", 
                "years_since_intro", 
                "niche_breadth_nat",  "range_size_nat", "lat_dist",
                "niche_centroid_a_nat", "niche_centroid_b_nat")


traits_res_oe <- tibble()
traits_res_os <- tibble()
traits_res_ou <- tibble()


for (reg in regions) {
  
  print(reg)
  
  # load in results 
  traits_res <- read.csv(paste0("results/trait_analysis/full_models/results_TraitAnal_df_ESU_",reg,".csv"))
  
  traits_res_reg_oe <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "orig_exp", term = Trait, estimate = orig_exp_coef, std.error = orig_exp_stderr, statistic = orig_exp_coef/orig_exp_stderr, p.value = orig_exp_p, varimp = 0.85) %>%
    select(region,model, term, estimate, std.error, statistic, p.value, varimp) %>% 
    filter(term %in% covariates)
  
  traits_res_reg_os <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "orig_stab", term = Trait, estimate = orig_stab_coef, std.error = orig_stab_stderr, statistic = orig_stab_coef/orig_stab_stderr, p.value = orig_stab_p, varimp = 0.85) %>%
    select(region, model, term, estimate, std.error, statistic, p.value, varimp)  %>% 
    filter(term %in% covariates)
  
  traits_res_reg_ou <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "orig_unf", term = Trait, estimate = orig_unf_coef, std.error = orig_unf_stderr, statistic = orig_unf_coef/orig_unf_stderr, p.value = orig_unf_p, varimp = 0.85) %>%
    select(region, model, term, estimate, std.error, statistic, p.value, varimp)  %>% 
    filter(term %in% covariates)
  
  # add results to main table
  traits_res_oe <- rbind(traits_res_oe, traits_res_reg_oe)
  traits_res_os <- rbind(traits_res_os, traits_res_reg_os)
  traits_res_ou <- rbind(traits_res_ou, traits_res_reg_ou)
  
} # end of for loop

rm(traits_res_reg_oe, traits_res_reg_os, traits_res_reg_ou, traits_res)



traits_res_oe[traits_res_oe$region == "eur" & traits_res_oe$term == "mean_seedmass", "estimate"] <- 3.1
traits_res_oe[traits_res_oe$region == "sam" & traits_res_oe$term == "mean_seedmass", "estimate"] <- 3.1
traits_res_oe[traits_res_oe$region == "eur" & traits_res_oe$term == "lat_dist", "estimate"] <- 3.1
traits_res_oe[traits_res_oe$region == "pac" & traits_res_oe$term == "lat_dist", "estimate"] <- 3.1
traits_res_oe[traits_res_oe$region == "ate" & traits_res_oe$term == "lat_dist", "estimate"] <- 3.1
traits_res_oe[traits_res_oe$region == "nam" & traits_res_oe$term == "lat_dist", "estimate"] <- 3.1
traits_res_oe[traits_res_oe$region == "atr" & traits_res_oe$term == "lat_dist", "estimate"] <- 3.1
traits_res_os[traits_res_os$region == "pac" & traits_res_os$term == "mean_seedmass", "estimate"] <- 3.1
traits_res_ou[traits_res_ou$region == "sam" & traits_res_ou$term == "mean_seedmass", "estimate"] <- 3.1


traits_res_oe[traits_res_oe$region == "pac" & traits_res_oe$term == "mean_seedmass", "estimate"] <- -3.606
traits_res_os[traits_res_os$region == "eur" & traits_res_os$term == "mean_seedmass", "estimate"] <- -3.606
traits_res_os[traits_res_os$region == "sam" & traits_res_os$term == "mean_seedmass", "estimate"] <- -3.606
traits_res_os[traits_res_os$region == "eur" & traits_res_os$term == "lat_dist", "estimate"] <- -3.606
traits_res_os[traits_res_os$region == "pac" & traits_res_os$term == "lat_dist", "estimate"] <- -3.606
traits_res_os[traits_res_os$region == "ate" & traits_res_os$term == "lat_dist", "estimate"] <- -3.606
traits_res_os[traits_res_os$region == "nam" & traits_res_os$term == "lat_dist", "estimate"] <- -3.606
traits_res_os[traits_res_os$region == "atr" & traits_res_os$term == "lat_dist", "estimate"] <- -3.606
traits_res_ou[traits_res_ou$region == "eur" & traits_res_ou$term == "mean_seedmass", "estimate"] <- -3.606


min_oe <- round(min(traits_res_oe$estimate, na.rm = TRUE), 2)
max_oe <- round(max(traits_res_oe$estimate, na.rm = TRUE), 2)
min_os <- round(min(traits_res_os$estimate, na.rm = TRUE), 2)
max_os <- round(max(traits_res_os$estimate, na.rm = TRUE), 2)
min_ou <- round(min(traits_res_ou$estimate, na.rm = TRUE), 2)
max_ou <- round(max(traits_res_ou$estimate, na.rm = TRUE), 2)


oe_efs_mtx <- traits_res_oe %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

oe_imp_mtx <- traits_res_oe %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)


os_efs_mtx <- traits_res_os %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

os_imp_mtx <- traits_res_os %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)


ou_efs_mtx <- traits_res_ou %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

ou_imp_mtx <- traits_res_ou %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

rownames(oe_efs_mtx) <- covariates
rownames(oe_imp_mtx) <- covariates
rownames(os_efs_mtx) <- covariates
rownames(os_imp_mtx) <- covariates
rownames(ou_efs_mtx) <- covariates
rownames(ou_imp_mtx) <- covariates

# prepare r² output -------------------------------------------------------


r2_oe <- NULL
r2_os <- NULL
r2_ou <- NULL

for (reg in regions) {
  
  df_res <- read.csv(paste0("results/trait_analysis/full_models/results_TraitAnal_df_ESU_",reg,".csv"))
  
  r2_oe <- c(r2_oe, round(df_res[df_res$Trait == "R2", "orig_exp_coef"], 2))
  r2_os <- c(r2_os, round(df_res[df_res$Trait == "R2", "orig_stab_coef"], 2))
  r2_ou <- c(r2_ou, round(df_res[df_res$Trait == "R2", "orig_unf_coef"], 2))
  
} # end of for loop over regions

r2_oe <- r2_oe %>% unlist()
r2_os <- r2_os %>% unlist()
r2_ou <- r2_ou %>% unlist()



col_lim <- c(-3.61, 3.1)

txt_col <- "black"
na_col <- "white"
col <- rev(COL2('RdBu', 200))

col_names <- c("Africa (n = 124)",
               "temp. Asia (n = 95)",
               "trop. Asia (n = 78)",
               "Australasia (n = 124)",
               "Europe (n = 56)",
               "N. America (n = 110)",
               "Pacific Islands (n = 143)",
               "S. America (n = 41)")


row_names <- c("Plant height", "Seed mass", "Growth form", "Life cycle",  "Residence time",
               "Native niche breadth", "Native range size" ,  "Distance lat. centroids", 
               "Native niche centroid 1", "Native niche centroid 2")


corrplot_mod(m_imp = oe_imp_mtx,
             m_efs = oe_efs_mtx,
             r2names = r2_oe,
             mar = c(2,2,3,2),
             cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             cl.offset = -1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)


corrplot_mod(m_imp = os_imp_mtx,
             m_efs = os_efs_mtx,
             r2names = r2_os,
             mar = c(2,2,3,2),
             cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             cl.offset = -1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

corrplot_mod(m_imp = ou_imp_mtx,
             m_efs = ou_efs_mtx,
             r2names = r2_ou,
             mar = c(2,2,3,2),
             cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             cl.offset = -1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)
