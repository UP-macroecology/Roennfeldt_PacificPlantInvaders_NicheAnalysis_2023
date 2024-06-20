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

# covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", "max_elev_range", "lat_nat", "lon_nat", "range_size_nat",
#               "niche_breadth_nat", "niche_centroid_a_nat", "niche_centroid_b_nat", "years_since_intro", "eucl_dist")

# without lon/lat
covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", 
                "years_since_intro", 
                "niche_breadth_nat",  "range_size_nat", "lat_dist",
                "niche_centroid_a_nat", "niche_centroid_b_nat")


traits_res_e <- tibble()
traits_res_s <- tibble()
traits_res_u <- tibble()


for (reg in regions) {
  
  print(reg)
  
  # load in results 
  traits_res <- read.csv(paste0("results/trait_analysis/main_analysis/results_TraitAnal_df_ESU_",reg,".csv"))
  
  traits_res_reg_e <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "expansion", term = Trait, estimate = expansion_coef, std.error = expansion_stderr, statistic = expansion_coef/expansion_stderr, p.value = expansion_p, varimp = expansion_varimp) %>%
    select(region,model, term, estimate, std.error, statistic, p.value, varimp) %>% 
    filter(term %in% covariates)
  
  traits_res_reg_s <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "stability", term = Trait, estimate = stability_coef, std.error = stability_stderr, statistic = stability_coef/stability_stderr, p.value = stability_p, varimp = stability_varimp) %>%
    select(region, model, term, estimate, std.error, statistic, p.value, varimp)  %>% 
    filter(term %in% covariates)
  
  traits_res_reg_u <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "unfilling", term = Trait, estimate = unfilling_coef, std.error = unfilling_stderr, statistic = unfilling_coef/unfilling_stderr, p.value = unfilling_p, varimp = unfilling_varimp) %>%
    select(region, model, term, estimate, std.error, statistic, p.value, varimp) %>% 
    filter(term %in% covariates)
  
  
  
  # add results to main table
  traits_res_e <- rbind(traits_res_e, traits_res_reg_e)
  traits_res_s <- rbind(traits_res_s, traits_res_reg_s)
  traits_res_u <- rbind(traits_res_u, traits_res_reg_u)
  
} # end of for loop

rm(traits_res_reg_e, traits_res_reg_s, traits_res_reg_u, traits_res)

traits_res_e[traits_res_e$region == "pac" & traits_res_e$term == "mean_seedmass", "estimate"] <- -3.827
traits_res_e[traits_res_e$region == "pac" & traits_res_e$term == "lat_dist", "estimate"] <- 5.179
traits_res_e[traits_res_e$region == "eur" & traits_res_e$term == "lat_dist", "estimate"] <- 5.179
traits_res_e[traits_res_e$region == "ate" & traits_res_e$term == "lat_dist", "estimate"] <- 5.179

min_exp <- round(min(traits_res_e$estimate, na.rm = TRUE), 2)
max_exp <- round(max(traits_res_e$estimate, na.rm = TRUE), 2)
min_stb <- round(min(traits_res_s$estimate, na.rm = TRUE), 2)
max_stb <- round(max(traits_res_s$estimate, na.rm = TRUE), 2)
min_unf <- round(min(traits_res_u$estimate, na.rm = TRUE), 2)
max_unf <- round(max(traits_res_u$estimate, na.rm = TRUE), 2)


e_efs_mtx <- traits_res_e %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)


e_imp_mtx <- traits_res_e %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)


u_efs_mtx <- traits_res_u %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

u_imp_mtx <- traits_res_u %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

s_efs_mtx <- traits_res_s %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

s_imp_mtx <- traits_res_s %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)



rownames(e_efs_mtx) <- covariates
rownames(e_imp_mtx) <- covariates
rownames(u_efs_mtx) <- covariates
rownames(u_imp_mtx) <- covariates
rownames(s_efs_mtx) <- covariates
rownames(s_imp_mtx) <- covariates




# R² data  ----------------------------------------------------------------

regions <- c("afr", "ate", "atr", "aus", "eur", "nam", "pac", "sam")

r2_exp <- NULL
r2_sta <- NULL
r2_unf <- NULL

for (reg in regions) {
  
  df_res <- read.csv(paste0("results/trait_analysis/main_analysis/results_TraitAnal_df_ESU_",reg,".csv"))
  
  r2_exp <- c(r2_exp, round(df_res[df_res$Trait == "R2", "expansion_coef"], 2))
  r2_sta <- c(r2_sta, round(df_res[df_res$Trait == "R2", "stability_coef"], 2))
  r2_unf <- c(r2_unf, round(df_res[df_res$Trait == "R2", "unfilling_coef"], 2))
  
} # end of for loop over regions

r2_exp <- r2_exp %>% unlist()
r2_sta <- r2_sta %>% unlist()
r2_unf <- r2_unf %>% unlist()


col_lim <- c(-3.83, 5.179)

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



corrplot_mod(m_imp = e_imp_mtx,
             m_efs = e_efs_mtx,
             r2names = r2_exp,
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





corrplot_mod(m_imp = s_imp_mtx,
             m_efs = s_efs_mtx, 
             r2names = r2_sta,
             mar = c(2,2,2,2),
             # cl.pos = "n",
             cl.cex = 1.4,
             # cl.offset = 1,
             col.lim = col_lim, 
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)




corrplot_mod(m_imp = u_imp_mtx, 
             m_efs = u_efs_mtx,
             r2names = r2_unf,
             mar = c(2,2,2,2),
             # cl.pos = "n",
             cl.cex = 1.4,
             col.lim = col_lim, 
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)





#  full models ------------------------------------------------------------
rm(list = ls())

select <- dplyr::select
source("scripts/functions.R")

regions <- c("afr", "ate", "atr", "aus", "eur", "nam", "pac", "sam")

covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", 
                "years_since_intro", 
                "niche_breadth_nat",  "range_size_nat", "lat_dist",
                "niche_centroid_a_nat", "niche_centroid_b_nat")


traits_res_e <- tibble()
traits_res_s <- tibble()
traits_res_u <- tibble()


for (reg in regions) {
  
  print(reg)
  
  # load in results 
  traits_res <- read.csv(paste0("results/trait_analysis/full_models/results_TraitAnal_df_ESU_",reg,".csv"))
  
  traits_res_reg_e <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "expansion", term = Trait, estimate = expansion_coef, std.error = expansion_stderr, statistic = expansion_coef/expansion_stderr, p.value = expansion_p, varimp = 0.85) %>%
    select(region,model, term, estimate, std.error, statistic, p.value, varimp) %>% 
    filter(term %in% covariates)
  
  traits_res_reg_s <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "stability", term = Trait, estimate = stability_coef, std.error = stability_stderr, statistic = stability_coef/stability_stderr, p.value = stability_p, varimp = 0.85) %>%
    select(region, model, term, estimate, std.error, statistic, p.value, varimp)  %>% 
    filter(term %in% covariates)
  
  traits_res_reg_u <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "unfilling", term = Trait, estimate = unfilling_coef, std.error = unfilling_stderr, statistic = unfilling_coef/unfilling_stderr, p.value = unfilling_p, varimp = 0.85) %>%
    select(region, model, term, estimate, std.error, statistic, p.value, varimp) %>% 
    filter(term %in% covariates)
  
  
  
  # add results to main table
  traits_res_e <- rbind(traits_res_e, traits_res_reg_e)
  traits_res_s <- rbind(traits_res_s, traits_res_reg_s)
  traits_res_u <- rbind(traits_res_u, traits_res_reg_u)
  
} # end of for loop

rm(traits_res_reg_e, traits_res_reg_s, traits_res_reg_u, traits_res)


traits_res_e[traits_res_e$region == "pac" & traits_res_e$term == "mean_seedmass", "estimate"] <- -2
traits_res_e[traits_res_e$region == "aus" & traits_res_e$term == "mean_seedmass", "estimate"] <- -2

traits_res_s[traits_res_s$region == "sam" & traits_res_s$term == "mean_seedmass", "estimate"] <- -2
traits_res_s[traits_res_s$region == "afr" & traits_res_s$term == "lat_dist", "estimate"] <- -2
traits_res_s[traits_res_s$region == "eur" & traits_res_s$term == "lat_dist", "estimate"] <- -2
traits_res_s[traits_res_s$region == "nam" & traits_res_s$term == "lat_dist", "estimate"] <- -2

traits_res_u[traits_res_u$region == "eur" & traits_res_u$term == "mean_seedmass", "estimate"] <- -2
traits_res_u[traits_res_u$region == "sam" & traits_res_u$term == "lat_dist", "estimate"] <- -2
traits_res_u[traits_res_u$region == "atr" & traits_res_u$term == "lat_dist", "estimate"] <- -2
traits_res_u[traits_res_u$region == "pac" & traits_res_u$term == "lat_dist", "estimate"] <- -2
traits_res_u[traits_res_u$region == "eur" & traits_res_u$term == "lat_dist", "estimate"] <- -2

traits_res_e[traits_res_e$region == "eur" & traits_res_e$term == "mean_seedmass", "estimate"] <- 2.5
traits_res_e[traits_res_e$region == "sam" & traits_res_e$term == "mean_seedmass", "estimate"] <- 2.5
traits_res_e[traits_res_e$region == "eur" & traits_res_e$term == "lat_dist", "estimate"] <- 2.5
traits_res_e[traits_res_e$region == "pac" & traits_res_e$term == "lat_dist", "estimate"] <- 2.5
traits_res_e[traits_res_e$region == "ate" & traits_res_e$term == "lat_dist", "estimate"] <- 2.5
traits_res_e[traits_res_e$region == "nam" & traits_res_e$term == "lat_dist", "estimate"] <- 2.5
traits_res_e[traits_res_e$region == "atr" & traits_res_e$term == "lat_dist", "estimate"] <- 2.5

traits_res_s[traits_res_s$region == "eur" & traits_res_s$term == "mean_seedmass", "estimate"] <- 2.5
traits_res_s[traits_res_s$region == "aus" & traits_res_s$term == "mean_seedmass", "estimate"] <- 2.5
traits_res_s[traits_res_s$region == "sam" & traits_res_s$term == "lat_dist", "estimate"] <- 2.5

traits_res_u[traits_res_u$region == "sam" & traits_res_u$term == "mean_seedmass", "estimate"] <- 2.5



e_efs_mtx <- traits_res_e %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)


e_imp_mtx <- traits_res_e %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)


u_efs_mtx <- traits_res_u %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

u_imp_mtx <- traits_res_u %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

s_efs_mtx <- traits_res_s %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

s_imp_mtx <- traits_res_s %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)



rownames(e_efs_mtx) <- covariates
rownames(e_imp_mtx) <- covariates
rownames(u_efs_mtx) <- covariates
rownames(u_imp_mtx) <- covariates
rownames(s_efs_mtx) <- covariates
rownames(s_imp_mtx) <- covariates



# R² data  ----------------------------------------------------------------

regions <- c("afr", "ate", "atr", "aus", "eur", "nam", "pac", "sam")

r2_exp <- NULL
r2_sta <- NULL
r2_unf <- NULL

for (reg in regions) {
  
  df_res <- read.csv(paste0("results/trait_analysis/full_models/results_TraitAnal_df_ESU_",reg,".csv"))
  
  r2_exp <- c(r2_exp, round(df_res[df_res$Trait == "R2", "expansion_coef"], 2))
  r2_sta <- c(r2_sta, round(df_res[df_res$Trait == "R2", "stability_coef"], 2))
  r2_unf <- c(r2_unf, round(df_res[df_res$Trait == "R2", "unfilling_coef"], 2))
  
} # end of for loop over regions

r2_exp <- r2_exp %>% unlist()
r2_sta <- r2_sta %>% unlist()
r2_unf <- r2_unf %>% unlist()




col_lim <- c(-2, 2.5)

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


corrplot_mod(m_imp = e_imp_mtx,
             m_efs = e_efs_mtx,
             r2names = r2_exp,
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





corrplot_mod(m_imp = s_imp_mtx,
             m_efs = s_efs_mtx, 
             r2names = r2_sta,
             mar = c(2,2,2,2),
             # cl.pos = "n",
             cl.cex = 1.4,
             # cl.offset = 1,
             col.lim = col_lim, 
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)




corrplot_mod(m_imp = u_imp_mtx, 
             m_efs = u_efs_mtx,
             r2names = r2_unf,
             mar = c(2,2,2,2),
             # cl.pos = "n",
             cl.cex = 1.4,
             col.lim = col_lim, 
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)
