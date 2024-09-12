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

# traits_res_oe <- tibble()
# traits_res_os <- tibble()
# traits_res_ou <- tibble()


for (reg in regions) {
  
  print(reg)
  
  # load in results 
  traits_res <- read.csv(paste0("results/trait_analysis/univariate/trait_analysis_results/results_TraitAnal_df_ESU_",reg,".csv"))
  
  traits_res_reg_e <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "expansion", term = Trait, estimate = lm_expansion_coef, varimp = 0.85) %>%
    select(region, model, term, estimate, varimp) %>% 
    filter(term %in% covariates)
  
  traits_res_reg_s <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "stability", term = Trait, estimate = lm_stability_coef, varimp = 0.85) %>%
    select(region, model, term, estimate, varimp) %>% 
    filter(term %in% covariates)
  
  traits_res_reg_u <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "unfilling", term = Trait, estimate = lm_unfilling_coef, varimp = 0.85) %>%
    select(region, model, term, estimate, varimp) %>% 
    filter(term %in% covariates)
  
  
  
  traits_res_reg_a <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "abandonment", term = Trait, estimate = lm_abandonment_coef, varimp = 0.85) %>%
    select(region, model, term, estimate, varimp) %>% 
    filter(term %in% covariates)
  
  traits_res_reg_p <- traits_res %>%
    as_tibble() %>% 
    mutate(region = reg, model = "unfilling", term = Trait, estimate = lm_pioneering_coef, varimp = 0.85) %>%
    select(region, model, term, estimate, varimp) %>% 
    filter(term %in% covariates)
  
  
  
  # traits_res_reg_oe <- traits_res %>%
  #   as_tibble() %>% 
  #   mutate(region = reg, model = "expansion", term = Trait, estimate = lm_orig_exp_coef, varimp = 0.85) %>%
  #   select(region, model, term, estimate, varimp) %>%  
  #   filter(term %in% covariates)
  # 
  # traits_res_reg_os <- traits_res %>%
  #   as_tibble() %>% 
  #   mutate(region = reg, model = "stability", term = Trait, estimate = lm_orig_stab_coef, varimp = 0.85) %>%
  #   select(region, model, term, estimate, varimp) %>% 
  #   filter(term %in% covariates)
  # 
  # traits_res_reg_ou <- traits_res %>%
  #   as_tibble() %>% 
  #   mutate(region = reg, model = "unfilling", term = Trait, estimate = lm_orig_unf_coef, varimp = 0.85) %>%
  #   select(region, model, term, estimate, varimp) %>% 
  #   filter(term %in% covariates)
  
  
  # add results to main table
  traits_res_e <- rbind(traits_res_e, traits_res_reg_e)
  traits_res_s <- rbind(traits_res_s, traits_res_reg_s)
  traits_res_u <- rbind(traits_res_u, traits_res_reg_u)
  
  traits_res_a <- rbind(traits_res_a, traits_res_reg_a)
  traits_res_p <- rbind(traits_res_p, traits_res_reg_p)
  
  # traits_res_oe <- rbind(traits_res_oe, traits_res_reg_oe)
  # traits_res_os <- rbind(traits_res_os, traits_res_reg_os)
  # traits_res_ou <- rbind(traits_res_ou, traits_res_reg_ou)
  
} # end of for loop

# rm(traits_res_reg_e, traits_res_reg_s, traits_res_reg_u, traits_res_reg_a, traits_res_reg_p, traits_res_reg_oe, traits_res_reg_os, traits_res_reg_ou, traits_res)
rm(traits_res_reg_e, traits_res_reg_s, traits_res_reg_u, traits_res_reg_a, traits_res_reg_p, traits_res)




# adjust min max ranges to make scale ranges more suitable for comparisons

# abandonment & pioneering
traits_res_a[traits_res_a$region == "eur" & traits_res_a$term == "mean_seedmass", "estimate"] <- -2
traits_res_a[traits_res_a$region == "sam" & traits_res_a$term == "lat_dist", "estimate"] <- -2
traits_res_a[traits_res_a$region == "atr" & traits_res_a$term == "years_since_intro", "estimate"] <- -2
traits_res_a[traits_res_a$region == "nam" & traits_res_a$term == "years_since_intro", "estimate"] <- -2
traits_res_a[traits_res_a$region == "nam" & traits_res_a$term == "niche_centroid_a_nat", "estimate"] <- -2

traits_res_a[traits_res_a$region == "pac" & traits_res_a$term == "lat_dist", "estimate"] <- 4
traits_res_a[traits_res_a$region == "nam" & traits_res_a$term == "lat_dist", "estimate"] <- 4
traits_res_a[traits_res_a$region == "afr" & traits_res_a$term == "lat_dist", "estimate"] <- 4

traits_res_p[traits_res_p$region == "atr" & traits_res_p$term == "lat_dist", "estimate"] <- 4
traits_res_p[traits_res_p$region == "eur" & traits_res_p$term == "mean_seedmass", "estimate"] <- 4
traits_res_p[traits_res_p$region == "pac" & traits_res_p$term == "lat_dist", "estimate"] <- 4
traits_res_p[traits_res_p$region == "nam" & traits_res_p$term == "lat_dist", "estimate"] <- 4


# ESU
traits_res_e[traits_res_e$region == "eur" & traits_res_e$term == "mean_seedmass", "estimate"] <- 5.055
traits_res_e[traits_res_e$region == "pac" & traits_res_e$term == "lat_dist", "estimate"] <- 5.055
traits_res_e[traits_res_e$region == "eur" & traits_res_e$term == "lat_dist", "estimate"] <- 5.055
traits_res_e[traits_res_e$region == "atr" & traits_res_e$term == "lat_dist", "estimate"] <- 5.055

traits_res_s[traits_res_s$region == "eur" & traits_res_s$term == "mean_seedmass", "estimate"] <- 5.055

traits_res_u[traits_res_u$region == "eur" & traits_res_u$term == "mean_seedmass", "estimate"] <- -2.5
traits_res_u[traits_res_u$region == "eur" & traits_res_u$term == "lat_dist", "estimate"] <- -2.5

traits_res_s[traits_res_s$region == "atr" & traits_res_s$term == "lat_dist", "estimate"] <- -2.5
traits_res_s[traits_res_s$region == "pac" & traits_res_s$term == "lat_dist", "estimate"] <- -2.5
traits_res_s[traits_res_s$region == "nam" & traits_res_s$term == "lat_dist", "estimate"] <- -2.5
traits_res_s[traits_res_s$region == "afr" & traits_res_s$term == "lat_dist", "estimate"] <- -2.5
traits_res_s[traits_res_s$region == "ate" & traits_res_s$term == "lat_dist", "estimate"] <- -2.5

# orig. ecospat output
traits_res_oe[traits_res_oe$region == "pac" & traits_res_oe$term == "lat_dist", "estimate"] <- 3.5
traits_res_oe[traits_res_oe$region == "atr" & traits_res_oe$term == "lat_dist", "estimate"] <- 3.5
traits_res_oe[traits_res_oe$region == "eur" & traits_res_oe$term == "lat_dist", "estimate"] <- 3.5
traits_res_oe[traits_res_oe$region == "sam" & traits_res_oe$term == "lat_dist", "estimate"] <- 3.5
traits_res_oe[traits_res_oe$region == "aus" & traits_res_oe$term == "mean_seedmass", "estimate"] <- 3.5

traits_res_ou[traits_res_ou$region == "atr" & traits_res_ou$term == "lat_dist", "estimate"] <- 3.5
traits_res_ou[traits_res_ou$region == "nam" & traits_res_ou$term == "lat_dist", "estimate"] <- 3.5
traits_res_ou[traits_res_ou$region == "ate" & traits_res_ou$term == "lat_dist", "estimate"] <- 3.5

traits_res_os[traits_res_os$region == "pac" & traits_res_os$term == "lat_dist", "estimate"] <- -2
traits_res_os[traits_res_os$region == "atr" & traits_res_os$term == "lat_dist", "estimate"] <- -2
traits_res_os[traits_res_os$region == "eur" & traits_res_os$term == "lat_dist", "estimate"] <- -2
traits_res_os[traits_res_os$region == "sam" & traits_res_os$term == "lat_dist", "estimate"] <- -2
traits_res_os[traits_res_os$region == "aus" & traits_res_os$term == "mean_seedmass", "estimate"] <- -2
traits_res_os[traits_res_os$region == "nam" & traits_res_os$term == "lat_dist", "estimate"] <- -2
traits_res_os[traits_res_os$region == "afr" & traits_res_os$term == "lat_dist", "estimate"] <- -2
traits_res_os[traits_res_os$region == "ate" & traits_res_os$term == "lat_dist", "estimate"] <- -2

traits_res_ou[traits_res_ou$region == "eur" & traits_res_ou$term == "mean_seedmass", "estimate"] <- -2
traits_res_ou[traits_res_ou$region == "eur" & traits_res_ou$term == "lat_dist", "estimate"] <- -2


# set limits for the colour scales
lim_ap <- c(-2, 4)
lim_esu <- c(-2.5, 5.055)
lim_orig <- c(-2, 3.5)


# prepare input matrices 

# AP
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

rownames(a_efs_mtx) <- covariates
rownames(a_imp_mtx) <- covariates
rownames(p_efs_mtx) <- covariates
rownames(p_imp_mtx) <- covariates


# ESU
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

rownames(e_efs_mtx) <- covariates
rownames(e_imp_mtx) <- covariates
rownames(s_efs_mtx) <- covariates
rownames(s_imp_mtx) <- covariates
rownames(u_efs_mtx) <- covariates
rownames(u_imp_mtx) <- covariates

# # orig
# oe_efs_mtx <- traits_res_oe %>% 
#   select(region, estimate, term) %>% 
#   pivot_wider(names_from = region, values_from = estimate) %>% 
#   select(!term) %>% 
#   as.matrix(rownames.force = TRUE)
# 
# oe_imp_mtx <- traits_res_oe %>% 
#   select(region, varimp, term) %>% 
#   pivot_wider(names_from = region, values_from = varimp) %>% 
#   select(!term) %>% 
#   as.matrix(rownames.force = TRUE)
# 
# 
# os_efs_mtx <- traits_res_os %>% 
#   select(region, estimate, term) %>% 
#   pivot_wider(names_from = region, values_from = estimate) %>% 
#   select(!term) %>% 
#   as.matrix(rownames.force = TRUE)
# 
# os_imp_mtx <- traits_res_os %>% 
#   select(region, varimp, term) %>% 
#   pivot_wider(names_from = region, values_from = varimp) %>% 
#   select(!term) %>% 
#   as.matrix(rownames.force = TRUE)
# 
# 
# ou_efs_mtx <- traits_res_ou %>% 
#   select(region, estimate, term) %>% 
#   pivot_wider(names_from = region, values_from = estimate) %>% 
#   select(!term) %>% 
#   as.matrix(rownames.force = TRUE)
# 
# ou_imp_mtx <- traits_res_ou %>% 
#   select(region, varimp, term) %>% 
#   pivot_wider(names_from = region, values_from = varimp) %>% 
#   select(!term) %>% 
#   as.matrix(rownames.force = TRUE)
# 
# rownames(oe_efs_mtx) <- covariates
# rownames(oe_imp_mtx) <- covariates
# rownames(os_efs_mtx) <- covariates
# rownames(os_imp_mtx) <- covariates
# rownames(ou_efs_mtx) <- covariates
# rownames(ou_imp_mtx) <- covariates

# set min max values to (-)1
a_efs_mtx[a_efs_mtx < -1] <- -1
a_efs_mtx[a_efs_mtx > 1] <- 1

u_efs_mtx[u_efs_mtx < -1] <- -1
u_efs_mtx[u_efs_mtx > 1] <- 1

s_efs_mtx[s_efs_mtx < -1] <- -1
s_efs_mtx[s_efs_mtx > 1] <- 1

e_efs_mtx[e_efs_mtx < -1] <- -1
e_efs_mtx[e_efs_mtx > 1] <- 1

p_efs_mtx[p_efs_mtx < -1] <- -1
p_efs_mtx[p_efs_mtx > 1] <- 1

col_names <- c("Africa", "temp. Asia", "trop. Asia", "Australasia", "Europe", "N. America", "Pacific Islands", "S. America")

row_names <- c("Plant height", 
               "Seed mass", 
               "Growth form", 
               "Life cycle",  
               "Residence time",
               "Native niche breadth", 
               "Native range size" ,  
               "Distance lat. centroids", 
               "Native niche centroid 1", 
               "Native niche centroid 2")

col_lim <- c(-1, 1)
txt_col <- "black"
na_col <- "white"
col <- rev(COL2('RdBu', 200))

# AP
corrplot_mod(m_imp = a_imp_mtx,
             m_efs = a_efs_mtx,
             mar = c(2,2,3,2),
             cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = NA,
             col_names = NA,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             cl.offset = -1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

corrplot_mod(m_imp = p_imp_mtx,
             m_efs = p_efs_mtx,
             mar = c(2,2,3,2),
             cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = NA,
             col_names = NA,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             cl.offset = -1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)
# ESU
corrplot_mod(m_imp = e_imp_mtx,
             m_efs = e_efs_mtx,
             mar = c(2,2,3,2),
             cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = NA,
             col_names = NA,
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
             mar = c(2,2,3,2),
             cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = NA,
             col_names = NA,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             cl.offset = -1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

corrplot_mod(m_imp = u_imp_mtx,
             m_efs = u_efs_mtx,
             mar = c(2,2,3,2),
             cl.cex = 1.4,
             col.lim = col_lim,
             col = col,
             row_names = NA,
             col_names = NA,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.cex = 1.5,
             tl.srt = 55,
             cl.offset = -1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)
