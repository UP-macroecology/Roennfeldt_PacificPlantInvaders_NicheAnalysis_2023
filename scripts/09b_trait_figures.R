#' ---------------------------
#
# Purpose of script: plotting the result of the trait analysis
# Author: Anna Rönnfeldt
# Date Created: ~ 2024-07
# Email: roennfeldt@uni-potsdam.de
#
# Notes: The script is split into three parts to plot the results of the different
# model versions -> (1) most parsimonious, (2) full and (3) univariate models. 
# The plot function is based on a modified version of the corrplot function.
#
#' ---------------------------

#library(Cairo)
library(corrplot)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)
library(tidyr)

select <- dplyr::select
source("scripts/functions.R")

regions <- c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr")

covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", 
                "years_since_intro", 
                "niche_breadth_nat",  "range_size_nat", "lat_dist",
                "niche_centroid_a_nat", "niche_centroid_b_nat")

# 1. main model --------------------------------------------------------------

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



##  r2 values --------------------------------------------------------------

# ended up adding them by hand to the plot (e.g. in ppt or inkscape) 

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


## prepare plot input ------------------------------------------------------

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
na_col <- "transparent"
bg_col <- "transparent"
grid_col <- "#737373"
col <- rev(COL2('RdBu', 200))

# col_names <- c("Africa (n = 124)",
#                "temp. Asia (n = 95)",
#                "trop. Asia (n = 78)",
#                "Australasia (n = 124)",
#                "Europe (n = 56)",
#                "N. America (n = 110)",
#                "Pacific Islands (n = 143)",
#                "S. America (n = 41)")

# col_names <- c("Pacific Islands (n = 143)",
#                "Africa (n = 124)",
#                "Australasia (n = 124)",
#                "Europe (n = 56)",
#                "N. America (n = 110)",
#                "S. America (n = 41)",
#                "temp. Asia (n = 95)",
#                "trop. Asia (n = 78)")
# 
# row_names <- c("Plant height", "Seed mass", "Growth form", "Life cycle",  "Residence time",
#                "Native niche breadth", "Native range size" ,  "Distance lat. centroids", 
#                "Native niche centroid 1", "Native niche centroid 2")


col_names <- NA
row_names <- NA

col_lim <- c(-1, 1)

txt_col <- "black"
na_col <- "transparent"
bg_col <- "transparent"
grid_col <- "#737373"
col <- rev(COL2('RdBu', 200))

w <- 480
h <- 480
dpi <- 300

file_type <- "svg"
path_plots <- "plots/manuscript/trait_analysis/"

# Cairo(file = paste0(path_plots,"unfilling_main.",file_type),  width = w, height = h, type = file_type, pointsize = 8, 
#       bg = bg_col, canvas = bg_col, units = "px", dpi = dpi)

svg(paste0(path_plots,"unfilling_main.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

# unfilling
corrplot_mod(m_imp = u_imp_mtx,
             m_efs = u_efs_mtx,
             addgrid.col = grid_col,
             bg = bg_col,
             mar = c(0,0,0,0),
             col.lim = col_lim,
             col = col,
             row_names = row_names,
             col_names = col_names,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.srt = 55,
             tl.cex = 1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()

# Cairo(file = paste0(path_plots,"stability_main.",file_type),  width = w, height = h, type = file_type, pointsize = 12, 
#       bg = bg_col, canvas = bg_col, units = "px", dpi = dpi)

svg(paste0(path_plots,"stability_main.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

# stability
corrplot_mod(m_imp = s_imp_mtx,
             m_efs = s_efs_mtx,
             addgrid.col = grid_col,
             bg = bg_col,
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

# Cairo(file = paste0(path_plots,"expansion_main.",file_type),  width = w, height = h, type = file_type, pointsize = 12, 
#       bg = bg_col, canvas = bg_col, units = "cm", dpi = dpi)

svg(paste0(path_plots,"expansion_main.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

# expansion
corrplot_mod(m_imp = e_imp_mtx,
             m_efs = e_efs_mtx,
             addgrid.col = grid_col,
             bg = bg_col,
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

svg(paste0(path_plots,"abandonment_main.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

# Cairo(file = paste0(path_plots,"abandonent_main.",file_type),  width = w, height = h, type = file_type, pointsize = 12, 
#       bg = bg_col, canvas = bg_col, units = "px", dpi = dpi)
# abandonment
corrplot_mod(m_imp = a_imp_mtx,
             m_efs = a_efs_mtx,
             addgrid.col = grid_col,
             bg = bg_col,
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

# Cairo(file = paste0(path_plots,"pioneering_main.",file_type),  width = w, height = h, type = file_type, pointsize = 12, 
#       bg = bg_col, canvas = bg_col, units = "px", dpi = "auto")

svg(paste0(path_plots,"pioneering_main.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

# pioneering
corrplot_mod(m_imp = p_imp_mtx,
             m_efs = p_efs_mtx,
             addgrid.col = grid_col,
             bg = bg_col,
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



# 2. full model -----------------------------------------------------------

traits_res_e <- tibble()
traits_res_s <- tibble()
traits_res_u <- tibble()
traits_res_a <- tibble()
traits_res_p <- tibble()

for (reg in regions) {
  
  print(reg)
  
  # load in results 
  traits_res <- read.csv(paste0("results/trait_analysis/full_models/results_TraitAnal_df_ESU_",reg,".csv"))
  
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


##  r2 values --------------------------------------------------------------


r2_exp <- NULL
r2_sta <- NULL
r2_unf <- NULL
r2_aba <- NULL
r2_pio <- NULL

for (reg in regions) {
  
  df_res <- read.csv(paste0("results/trait_analysis/full_models/results_TraitAnal_df_ESU_",reg,".csv"))
  
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

## prepare plot input ------------------------------------------------------

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

min(a_efs_mtx, na.rm = TRUE)
max(a_efs_mtx, na.rm = TRUE)

min(u_efs_mtx, na.rm = TRUE)
max(u_efs_mtx, na.rm = TRUE)

min(s_efs_mtx, na.rm = TRUE)
max(s_efs_mtx, na.rm = TRUE)

min(e_efs_mtx, na.rm = TRUE)
max(e_efs_mtx, na.rm = TRUE)

min(p_efs_mtx, na.rm = TRUE)
max(p_efs_mtx, na.rm = TRUE)


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


# set importance value to avoid varying circle sizes when displaying full model

imp_value <- 0.95

a_imp_mtx[] <- imp_value
u_imp_mtx[] <- imp_value
s_imp_mtx[] <- imp_value
e_imp_mtx[] <- imp_value
p_imp_mtx[] <- imp_value



# col_names <- c("Africa (n = 124)",
#                "temp. Asia (n = 95)",
#                "trop. Asia (n = 78)",
#                "Australasia (n = 124)",
#                "Europe (n = 56)",
#                "N. America (n = 110)",
#                "Pacific Islands (n = 143)",
#                "S. America (n = 41)")
# 
# 
# row_names <- c("Plant height", "Seed mass", "Growth form", "Life cycle",  "Residence time",
#                "Native niche breadth", "Native range size" ,  "Distance lat. centroids", 
#                "Native niche centroid 1", "Native niche centroid 2")




col_names <- NA
row_names <- NA

col_lim <- c(-1, 1)

txt_col <- "black"
na_col <- "transparent"
bg_col <- "transparent"
grid_col <- "#737373"
col <- rev(COL2('RdBu', 200))

w <- 28
h <- 28
dpi <- 300

file_type <- "svg"
path_plots <- "plots/manuscript/trait_analysis/"


svg(paste0(path_plots,"unfilling_full.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)



# unfilling
corrplot_mod(m_imp = u_imp_mtx,
             m_efs = u_efs_mtx,
             addgrid.col = grid_col,
             bg = bg_col,
             col.lim = col_lim,
             col = col,
             row_names = NA,
             col_names = NA,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.srt = 55,
             tl.cex = 1,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()


svg(paste0(path_plots,"expansion_full.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

# expansion
corrplot_mod(m_imp = e_imp_mtx,
             m_efs = e_efs_mtx,
             addgrid.col = grid_col,
             bg = bg_col,
             #mar = c(0,0,0,0),
             col.lim = col_lim,
             col = col,
             row_names = NA,
             col_names = NA,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.srt = 55,
             tl.cex = 0.5,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()


svg(paste0(path_plots,"stability_full.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

# stability
corrplot_mod(m_imp = s_imp_mtx,
             m_efs = s_efs_mtx,
             addgrid.col = grid_col,
             bg = bg_col,
             #mar = c(0,0,0,0),
             col.lim = col_lim,
             col = col,
             row_names = NA,
             col_names = NA,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.srt = 55,
             tl.cex = 0.5,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()


svg(paste0(path_plots,"abandonment_full.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

# abandonment
corrplot_mod(m_imp = a_imp_mtx,
             m_efs = a_efs_mtx,
             addgrid.col = grid_col,
             bg = bg_col,
             #mar = c(0,0,0,0),
             col.lim = col_lim,
             col = col,
             row_names = NA,
             col_names = NA,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.srt = 55,
             tl.cex = 0.5,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()


svg(paste0(path_plots,"pioneering_full.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

# pioneering
corrplot_mod(m_imp = p_imp_mtx,
             m_efs = p_efs_mtx,
             addgrid.col = grid_col,
             bg = bg_col,
             #mar = c(0,0,0,0),
             col.lim = col_lim,
             col = col,
             row_names = NA,
             col_names = NA,
             is.corr = FALSE,
             tl.col = txt_col,
             tl.srt = 55,
             tl.cex = 0.5,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()



# 3. univariate models ----------------------------------------------------

traits_res_e <- tibble()
traits_res_s <- tibble()
traits_res_u <- tibble()

traits_res_a <- tibble()
traits_res_p <- tibble()


for (reg in regions) {
  
  print(reg)
  
  # load in results 
  traits_res <- read.csv(paste0("results/trait_analysis/univariate/results_TraitAnal_df_ESU_",reg,".csv"))
  
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
  
  
  # add results to main table
  traits_res_e <- rbind(traits_res_e, traits_res_reg_e)
  traits_res_s <- rbind(traits_res_s, traits_res_reg_s)
  traits_res_u <- rbind(traits_res_u, traits_res_reg_u)
  
  traits_res_a <- rbind(traits_res_a, traits_res_reg_a)
  traits_res_p <- rbind(traits_res_p, traits_res_reg_p)

  
} # end of for loop

# rm(traits_res_reg_e, traits_res_reg_s, traits_res_reg_u, traits_res_reg_a, traits_res_reg_p, traits_res_reg_oe, traits_res_reg_os, traits_res_reg_ou, traits_res)
rm(traits_res_reg_e, traits_res_reg_s, traits_res_reg_u, traits_res_reg_a, traits_res_reg_p, traits_res)


## prepare plot input ------------------------------------------------------

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

# col_names <- c("Africa", "temp. Asia", "trop. Asia", "Australasia", "Europe", "N. America", "Pacific Islands", "S. America")
# 
# row_names <- c("Plant height", 
#                "Seed mass", 
#                "Growth form", 
#                "Life cycle",  
#                "Residence time",
#                "Native niche breadth", 
#                "Native range size" ,  
#                "Distance lat. centroids", 
#                "Native niche centroid 1", 
#                "Native niche centroid 2")

col_names <- NA
row_names <- NA

col_lim <- c(-1, 1)

txt_col <- "black"
na_col <- "transparent"
bg_col <- "transparent"
grid_col <- "#737373"
col <- rev(COL2('RdBu', 200))

w <- 28
h <- 28
dpi <- 300

file_type <- "svg"
path_plots <- "plots/manuscript/trait_analysis/"




svg(paste0(path_plots,"abandonment_uni.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

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

dev.off()


svg(paste0(path_plots,"pioneering_uni.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

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

dev.off()

# ESU

svg(paste0(path_plots,"expansion_uni.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

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

dev.off()


svg(paste0(path_plots,"stability_uni.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

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

dev.off()


svg(paste0(path_plots,"unfilling_uni.svg"), width = w, height = h, 
    pointsize = 10, bg = bg_col)

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

dev.off()

