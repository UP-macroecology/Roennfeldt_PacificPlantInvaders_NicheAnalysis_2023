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


traits_res_p <- tibble()
traits_res_a <- tibble()

for (reg in regions) {
  
  print(reg)
  
  # load in results 
  traits_res <- read.csv(paste0("results/trait_analysis/main_analysis/results_TraitAnal_df_ESU_",reg,".csv"))
  
  traits_res_reg_p <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "pioneering", term = Trait, estimate = pioneering_coef, std.error = pioneering_stderr, statistic = pioneering_coef/pioneering_stderr, p.value = pioneering_p, varimp = pioneering_varimp) %>%
    select(region,model, term, estimate, std.error, statistic, p.value, varimp) %>% 
    filter(term %in% covariates)
  
  traits_res_reg_a <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "abandonment", term = Trait, estimate = abandonment_coef, std.error = abandonment_stderr, statistic = abandonment_coef/abandonment_stderr, p.value = abandonment_p, varimp = abandonment_varimp) %>%
    select(region, model, term, estimate, std.error, statistic, p.value, varimp)  %>% 
    filter(term %in% covariates)
  
  # add results to main table
  traits_res_p <- rbind(traits_res_p, traits_res_reg_p)
  traits_res_a <- rbind(traits_res_a, traits_res_reg_a)

  
} # end of for loop

rm(traits_res_reg_p, traits_res_reg_a, traits_res)

traits_res_p[traits_res_p$region == "sam" & traits_res_p$term == "lat_dist", "estimate"] <- -5.239
traits_res_p[traits_res_p$region == "aus" & traits_res_p$term == "mean_seedmass", "estimate"] <- -5.239
traits_res_p[traits_res_p$region == "atr" & traits_res_p$term == "lat_dist", "estimate"] <- 5.718
traits_res_a[traits_res_a$region == "afr" & traits_res_a$term == "lat_dist", "estimate"] <- 5.718
traits_res_a[traits_res_a$region == "aus" & traits_res_a$term == "lat_dist", "estimate"] <- 5.718


min_pio <- round(min(traits_res_p$estimate, na.rm = TRUE), 2)
max_pio <- round(max(traits_res_p$estimate, na.rm = TRUE), 2)
min_aba <- round(min(traits_res_a$estimate, na.rm = TRUE), 2)
max_aba <- round(max(traits_res_a$estimate, na.rm = TRUE), 2)


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

rownames(p_efs_mtx) <- covariates
rownames(p_imp_mtx) <- covariates
rownames(a_efs_mtx) <- covariates
rownames(a_imp_mtx) <- covariates


# prepare r² output -------------------------------------------------------


r2_pio <- NULL
r2_aba <- NULL

for (reg in regions) {
  
  df_res <- read.csv(paste0("results/trait_analysis/main_analysis/results_TraitAnal_df_ESU_",reg,".csv"))
  
  r2_pio <- c(r2_pio, round(df_res[df_res$Trait == "R2", "pioneering_coef"], 2))
  r2_aba <- c(r2_aba, round(df_res[df_res$Trait == "R2", "abandonment_coef"], 2))
  
} # end of for loop over regions

r2_pio <- r2_pio %>% unlist()
r2_aba <- r2_aba %>% unlist()


col_lim <- c(-5.24, 5.72)

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


corrplot_mod(m_imp = p_imp_mtx,
             m_efs = p_efs_mtx,
             r2names = r2_pio,
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

corrplot_mod(m_imp = a_imp_mtx,
             m_efs = a_efs_mtx,
             r2names = r2_aba,
             mar = c(2,2,2,2),
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

traits_res_p <- tibble()
traits_res_a <- tibble()

for (reg in regions) {
  
  print(reg)
  
  # load in results 
  traits_res <- read.csv(paste0("results/trait_analysis/full_models/results_TraitAnal_df_ESU_",reg,".csv"))
  
  traits_res_reg_p <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "pioneering", term = Trait, estimate = pioneering_coef, std.error = pioneering_stderr, statistic = pioneering_coef/pioneering_stderr, p.value = pioneering_p, varimp = 0.85) %>%
    select(region,model, term, estimate, std.error, statistic, p.value, varimp) %>% 
    filter(term %in% covariates)
  
  traits_res_reg_a <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "abandonment", term = Trait, estimate = abandonment_coef, std.error = abandonment_stderr, statistic = abandonment_coef/abandonment_stderr, p.value = abandonment_p, varimp = 0.85) %>%
    select(region, model, term, estimate, std.error, statistic, p.value, varimp)  %>% 
    filter(term %in% covariates)
  
  # add results to main table
  traits_res_p <- rbind(traits_res_p, traits_res_reg_p)
  traits_res_a <- rbind(traits_res_a, traits_res_reg_a)
  
  
} # end of for loop

rm(traits_res_reg_p, traits_res_reg_a, traits_res)


traits_res_p[traits_res_p$region == "sam" & traits_res_p$term == "lat_dist", "estimate"] <- -5.5
traits_res_p[traits_res_p$region == "sam" & traits_res_p$term == "mean_seedmass", "estimate"] <- -5.5
traits_res_p[traits_res_p$region == "aus" & traits_res_p$term == "mean_seedmass", "estimate"] <- -5.5
traits_res_p[traits_res_p$region == "eur" & traits_res_p$term == "mean_seedmass", "estimate"] <- -5.5
traits_res_a[traits_res_a$region == "eur" & traits_res_a$term == "mean_seedmass", "estimate"] <- -5.5
traits_res_a[traits_res_a$region == "sam" & traits_res_a$term == "mean_seedmass", "estimate"] <- -5.5


traits_res_a[traits_res_a$region == "afr" & traits_res_a$term == "lat_dist", "estimate"] <- 5.518
traits_res_a[traits_res_a$region == "aus" & traits_res_a$term == "lat_dist", "estimate"] <- 5.518
traits_res_p[traits_res_p$region == "ate" & traits_res_p$term == "lat_dist", "estimate"] <- 5.518
traits_res_p[traits_res_p$region == "atr" & traits_res_p$term == "lat_dist", "estimate"] <- 5.518
traits_res_p[traits_res_p$region == "nam" & traits_res_p$term == "lat_dist", "estimate"] <- 5.518


min_pio <- round(min(traits_res_p$estimate, na.rm = TRUE), 2)
max_pio <- round(max(traits_res_p$estimate, na.rm = TRUE), 2)
min_aba <- round(min(traits_res_a$estimate, na.rm = TRUE), 2)
max_aba <- round(max(traits_res_a$estimate, na.rm = TRUE), 2)


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

rownames(p_efs_mtx) <- covariates
rownames(p_imp_mtx) <- covariates
rownames(a_efs_mtx) <- covariates
rownames(a_imp_mtx) <- covariates


# prepare r² output -------------------------------------------------------


r2_pio <- NULL
r2_aba <- NULL

for (reg in regions) {
  
  df_res <- read.csv(paste0("results/trait_analysis/full_models/results_TraitAnal_df_ESU_",reg,".csv"))
  
  r2_pio <- c(r2_pio, round(df_res[df_res$Trait == "R2", "pioneering_coef"], 2))
  r2_aba <- c(r2_aba, round(df_res[df_res$Trait == "R2", "abandonment_coef"], 2))
  
} # end of for loop over regions

r2_pio <- r2_pio %>% unlist()
r2_aba <- r2_aba %>% unlist()


col_lim <- c(-5.85, 5.52)

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


corrplot_mod(m_imp = p_imp_mtx,
             m_efs = p_efs_mtx,
             r2names = r2_pio,
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

corrplot_mod(m_imp = a_imp_mtx,
             m_efs = a_efs_mtx,
             r2names = r2_aba,
             mar = c(2,2,2,2),
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

