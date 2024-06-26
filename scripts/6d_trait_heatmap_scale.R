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
traits_res_a <- tibble()
traits_res_p <- tibble()

for (reg in regions) {
  
  print(reg)
  
  # load in results 
  traits_res <- read.csv(paste0("results/trait_analysis/main_analysis_scale/log_results_TraitAnal_df_ESU_",reg,".csv"))
  
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
  
  traits_res_reg_a <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "abandonment", term = Trait, estimate = abandonment_coef, std.error = abandonment_stderr, statistic = abandonment_coef/abandonment_stderr, p.value = abandonment_p, varimp = abandonment_varimp) %>%
    select(region, model, term, estimate, std.error, statistic, p.value, varimp) %>% 
    filter(term %in% covariates)
  
  traits_res_reg_p <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "pioneering", term = Trait, estimate = pioneering_coef, std.error = pioneering_stderr, statistic = pioneering_coef/pioneering_stderr, p.value = pioneering_p, varimp = pioneering_varimp) %>%
    select(region, model, term, estimate, std.error, statistic, p.value, varimp) %>% 
    filter(term %in% covariates)
  
  # add results to main table
  traits_res_e <- rbind(traits_res_e, traits_res_reg_e)
  traits_res_s <- rbind(traits_res_s, traits_res_reg_s)
  traits_res_u <- rbind(traits_res_u, traits_res_reg_u)
  traits_res_a <- rbind(traits_res_a, traits_res_reg_a)
  traits_res_p <- rbind(traits_res_p, traits_res_reg_p)
  
} # end of for loop

rm(traits_res_reg_e, traits_res_reg_s, traits_res_reg_u, traits_res_reg_a, traits_res_reg_p, traits_res)



#  r2 values --------------------------------------------------------------


r2_exp <- NULL
r2_sta <- NULL
r2_unf <- NULL
r2_aba <- NULL
r2_pio <- NULL

for (reg in regions) {
  
  df_res <- read.csv(paste0("results/trait_analysis/main_analysis_scale/log_results_TraitAnal_df_ESU_",reg,".csv"))
  
  r2_exp <- c(r2_exp, round(df_res[df_res$Trait == "R2", "expansion_coef"], 2))
  r2_sta <- c(r2_sta, round(df_res[df_res$Trait == "R2", "stability_coef"], 2))
  r2_unf <- c(r2_unf, round(df_res[df_res$Trait == "R2", "unfilling_coef"], 2))
  r2_aba <- c(r2_aba, round(df_res[df_res$Trait == "R2", "abandonment_coef"], 2))
  r2_pio <- c(r2_pio, round(df_res[df_res$Trait == "R2", "pioneering_coef"], 2))
  
} # end of for loop over regions

r2_exp <- r2_exp %>% unlist()
r2_sta <- r2_sta %>% unlist()
r2_unf <- r2_unf %>% unlist()
r2_aba <- r2_aba %>% unlist()
r2_pio <- r2_pio %>% unlist()


# prepare plot input ------------------------------------------------------

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

a_efs_mtx <- traits_res_a %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

a_imp_mtx <- traits_res_a %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

p_efs_mtx <- traits_res_p %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

p_imp_mtx <- traits_res_p %>% 
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
rownames(a_efs_mtx) <- covariates
rownames(a_imp_mtx) <- covariates
rownames(p_efs_mtx) <- covariates
rownames(p_imp_mtx) <- covariates


col_lim <- c(-1, 1)

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



#layout(matrix(c(1,2,3), ncol = 3))

# CairoPDF(file = paste0("plots/trait_analysis/unfilling_main.png"), width = 9, height = 8,
#          family = "Arial")

Cairo(file = paste0("plots/trait_analysis/unfilling_main.png"),  width = 640, height = 480, type = "png", pointsize = 12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")

# unfilling
corrplot_mod(m_imp = u_imp_mtx,
             m_efs = u_efs_mtx,
             addgrid.col = "grey",
             mar = c(0,0,0,0),
             col.lim = col_lim,
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.srt = 55,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()

Cairo(file = paste0("plots/trait_analysis/unfilling_stability.png"),  width = 640, height = 480, type = "png", pointsize = 12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")

# stability
corrplot_mod(m_imp = s_imp_mtx,
             m_efs = s_efs_mtx,
             addgrid.col = "grey",
             col.lim = col_lim,
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.srt = 55,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()

Cairo(file = paste0("plots/trait_analysis/expansion_main.png"),  width = 640, height = 480, type = "png", pointsize = 12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")
# expansion
corrplot_mod(m_imp = e_imp_mtx,
             m_efs = e_efs_mtx,
             addgrid.col = "grey",
             col.lim = col_lim,
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.srt = 55,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()


#layout.show(3)

Cairo(file = paste0("plots/trait_analysis/abandonent_main.png"),  width = 640, height = 480, type = "png", pointsize = 12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")
# abandonment
corrplot_mod(m_imp = a_imp_mtx,
             m_efs = a_efs_mtx,
             addgrid.col = "grey",
             # r2names = r2_exp,
             #mar = c(2,2,3,2),
             # cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             # tl.cex = 1.5,
             tl.srt = 55,
             # cl.offset = -1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()

Cairo(file = paste0("plots/trait_analysis/pioneering_main.png"),  width = 640, height = 480, type = "png", pointsize = 12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")

# pioneering
corrplot_mod(m_imp = p_imp_mtx,
             m_efs = p_efs_mtx,
             addgrid.col = "grey",
             #r2names = r2_pio,
             #mar = c(2,2,3,2),
             #cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             # tl.cex = 1.5,
             tl.srt = 55,
             cl.offset = -1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()





# var imp (traits, time, geo) ---------------------------------------------

traits <- c(0.305,0.151, 0, 0.47, 0.142, 0.146, 0.741, 0.311, 0.13, 0.29, 0, 0.298, 0.134, 0.035, 0.79, 0.326, 0.222, 0.350, 0.346, 0.052, 0.033, 0, 0.319)

time <- c(0.336, 0, 0.567, 0.34, 0.289, 0, 0.094, 0, 0.234, 0.112, 0.361, 0.267, 0.207, 0, 0, 0, 0, 0.317, 0, 0.129, 0.196, 0.182, 0.135)

geo <- c(0.358, 0.848, 0.433, 0.19, 0.569, 0.771, 0.164, 0.688, 0.636, 0.599, 0.639, 0.435, 0.659, 0.965, 0.21, 0.674, 0.777, 0.334, 0.654, 0.819, 0.77, 0.817, 0.546)

geo_time <- c(0.694, 0.848, 1, 0.53, 0.858, 0.771, 0.258, 0.688, 0.87, 0.711, 1, 0.702, 0.866, 0.965, 0.21, 0.674, 0.777, 0.651, 0.654, 0.948, 0.966, 1, 0.681)


mean(traits) # 0.2431
sd(traits) # 0.2133

mean(time)
sd(time)

mean(geo)
sd(geo)

mean(geo_time)
sd(geo_time)
