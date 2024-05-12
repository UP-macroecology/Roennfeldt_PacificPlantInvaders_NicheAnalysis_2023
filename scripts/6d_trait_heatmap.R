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
covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", "range_size_nat", "dispersal",
                "niche_breadth_nat", "niche_centroid_a_nat", "niche_centroid_b_nat", "years_since_intro", "lat_dist")



# setting <- "mod_seed"
# setting <- "original"
setting <- "mod_seed_scale"

traits_res_e <- tibble()
traits_res_s <- tibble()
traits_res_u <- tibble()


for (reg in regions) {
  
  print(reg)
  
  # load in results 
  traits_res <- read.csv(paste0("results/trait_analysis/regional_trait_analysis_results/results_TraitAnal_df_ESU_",reg,".csv"))
  
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


# if (setting == "mod_seed") {
#   traits_res_u[traits_res_u$region == "eur" & traits_res_u$term == "mean_seedmass","estimate"] <- -4.962
#   traits_res_e[traits_res_e$region == "pac" & traits_res_e$term == "mean_seedmass","estimate"] <- -2.639
#   traits_res_s[traits_res_s$region == "sam" & traits_res_e$term == "eucl_dist","estimate"] <- 1.118
# }

if (setting == "mod_seed_scale") {
  traits_res_u[traits_res_u$region == "atr" & traits_res_u$term == "lat_dist","estimate"] <- -5.672
  traits_res_e[traits_res_e$region == "aus" & traits_res_e$term == "lat_dist","estimate"] <- -5.672
  traits_res_e[traits_res_e$region == "eur" & traits_res_e$term == "lat_dist","estimate"] <- 5.305
  traits_res_e[traits_res_e$region == "pac" & traits_res_e$term == "lat_dist","estimate"] <- 5.305
  traits_res_e[traits_res_e$region == "nam" & traits_res_e$term == "lat_dist","estimate"] <- 5.305
  traits_res_s[traits_res_s$region == "atr" & traits_res_s$term == "lat_dist","estimate"] <- 5.305
  traits_res_u[traits_res_u$region == "sam" & traits_res_u$term == "lat_dist","mean_seedmass"] <- 5.305
}


covariates <- unique(traits_res_e$term)

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

min_exp <- round(min(traits_res_e$estimate, na.rm = TRUE), 2)
max_exp <- round(max(traits_res_e$estimate, na.rm = TRUE), 2)
min_stb <- round(min(traits_res_s$estimate, na.rm = TRUE), 2)
max_stb <- round(max(traits_res_s$estimate, na.rm = TRUE), 2)
min_unf <- round(min(traits_res_u$estimate, na.rm = TRUE), 2)
max_unf <- round(max(traits_res_u$estimate, na.rm = TRUE), 2)



# R² data  ----------------------------------------------------------------

regions <- c("afr", "ate", "atr", "aus", "eur", "nam", "pac", "sam")

r2_exp <- NULL
r2_sta <- NULL
r2_unf <- NULL

for (reg in regions) {
  
  df_res <- read.csv(paste0("results/trait_analysis/regional_trait_analysis_results/results_TraitAnal_df_ESU_",reg,".csv"))
  
  r2_exp <- c(r2_exp, round(df_res[df_res$Trait == "R2", "expansion_coef"], 2))
  r2_sta <- c(r2_sta, round(df_res[df_res$Trait == "R2", "stability_coef"], 2))
  r2_unf <- c(r2_unf, round(df_res[df_res$Trait == "R2", "unfilling_coef"], 2))
  
} # end of for loop over regions

r2_exp <- r2_exp %>% unlist()
r2_sta <- r2_sta %>% unlist()
r2_unf <- r2_unf %>% unlist()


col_lim <- c(-5.672, 5.305) #col_lim <- c(-4.97, 3.23)
txt_col <- "black"
na_col <- "white"
col <- rev(COL2('RdBu', 200))

col_names <- c("Africa (n = 67)", 
               "temp. Asia (n = 49)", 
               "trop. Asia (n = 39)", 
               "Australasia (n = 64)", 
               "Europe (n = 26)", 
               "N. America (n = 57)", 
               "Pacific Islands (n = 73)",
               "S. America (n = 23)")


row_names <- c("Plant height", "Seed mass", "Growth form", "Life span", "Native range size","Dispersal", "Native niche breadth",
               "Native niche centroid 1", "Native niche centroid 2", "Residence time", "Distance lat. centroids")

Cairo(file = paste0("plots/trait_analysis/regional_expansion_",setting,".png"),  width = 640, height = 480, type = "png", pointsize = 12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")


if (setting == "mod_seed_scale") {
  
  corrplot_mod(m_imp = e_imp_mtx,
               m_efs = e_efs_mtx,
               r2names = r2_exp,
               mar = c(2,2,2,2),
               col.lim = col_lim,
               col = col,
               row_names = row_names,
               col_names = col_names,
               is.corr = FALSE,
               tl.col = txt_col,
               cl.offset = -1,
               na.label = "square",
               na.label.col = na_col,
               outline = TRUE)
  
} else {
  corrplot_mod(m_imp = e_imp_mtx,
               m_efs = e_efs_mtx,
               col.lim = c(min_exp, max_exp),
               col = col,
               row_names = row_names,
               col_names = col_names,
               is.corr = FALSE,
               tl.col = txt_col,
               na.label = "square",
               na.label.col = na_col,
               outline = TRUE)
}


dev.off()

Cairo(file = paste0("plots/trait_analysis/regional_stability_",setting,".png"),  width = 640, height = 480, type = "png", pointsize = 12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")


if (setting == "mod_seed_scale") { 
  
  corrplot_mod(m_imp = s_imp_mtx,
               m_efs = s_efs_mtx, 
               r2names = r2_sta,
               mar = c(2,2,2,2),
               cl.pos = "n",
               col.lim = col_lim, 
               col = col,
               row_names = row_names,
               col_names = col_names,
               is.corr = FALSE,
               tl.col = txt_col,
               na.label = "square",
               na.label.col = na_col,
               outline = TRUE)
} else {

  corrplot_mod(m_imp = s_imp_mtx,
               m_efs = s_efs_mtx, 
               col.lim = c(min_stb, max_stb),
               col = col,
               row_names = row_names,
               col_names = col_names,
               is.corr = FALSE,
               tl.col = txt_col,
               na.label = "square",
               na.label.col = na_col,
               outline = TRUE) 
  }

dev.off()



Cairo(file = paste0("plots/trait_analysis/regional_unfilling_",setting,".png"),  width = 640, height = 480, type = "png", pointsize = 12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")


if (setting == "mod_seed_scale") { 
  
  corrplot_mod(m_imp = u_imp_mtx, 
               m_efs = u_efs_mtx,
               r2names = r2_unf,
               mar = c(2,2,2,2),
               cl.pos = "n",
               col.lim = col_lim, 
               col = col,
               row_names = row_names,
               col_names = col_names,
               is.corr = FALSE,
               tl.col = txt_col,
               na.label = "square",
               na.label.col = na_col,
               outline = TRUE)
  } else {
  corrplot_mod(m_imp = u_imp_mtx, 
               m_efs = u_efs_mtx,
               col.lim = c(min_unf, max_unf), 
               col = col,
               row_names = row_names,
               col_names = col_names,
               is.corr = FALSE,
               tl.col = txt_col,
               na.label = "square",
               na.label.col = na_col,
               outline = TRUE)
    }

dev.off()


# correct for seedmass ----------------------------------------------------





# 
# 
# 
# 
# 
# # combined regions --------------------------------------------------------
# 
# rm(list = ls())
# 
# covariates <- c("mean_height", "growth_form", "lifecycle", "max_elev_range", "lon_centroid", "lat_centroid", "range_size_nat",
#                 "niche_breadth_nat", "niche_centroid_a_nat", "niche_centroid_b_nat", "years_since_intro", "eucl_dist", "CAI")
# 
# # load in results 
# traits_res <- read.csv(paste0("results/trait_analysis/combined_regions/results_TraitAnal_df_ESU.csv"))
# 
# traits_res_tb_e <- traits_res %>%
#   as_tibble() %>% 
#   mutate(model = "expansion", term = Trait, estimate = expansion_coef, std.error = expansion_stderr, statistic = expansion_coef/expansion_stderr, p.value = expansion_p, varimp = expansion_varimp) %>%
#   select(model, term, estimate, std.error, statistic, p.value, varimp) %>% 
#   filter(term %in% covariates)
# 
# traits_res_tb_s <- traits_res %>%
#   as_tibble() %>% 
#   mutate(model = "stability", term = Trait, estimate = stability_coef, std.error = stability_stderr, statistic = stability_coef/stability_stderr, p.value = stability_p, varimp = stability_varimp) %>%
#   select(model, term, estimate, std.error, statistic, p.value, varimp) %>% 
#   filter(term %in% covariates)
# 
# traits_res_tb_u <- traits_res %>%
#   as_tibble() %>% 
#   mutate(model = "unfilling", term = Trait, estimate = unfilling_coef, std.error = unfilling_stderr, statistic = unfilling_coef/unfilling_stderr, p.value = unfilling_p, varimp = unfilling_varimp) %>%
#   select(model, term, estimate, std.error, statistic, p.value, varimp) %>% 
#   filter(term %in% covariates)
# 
# # traits_res_tb_all <- rbind(traits_res_tb_a, traits_res_tb_e, traits_res_tb_p, traits_res_tb_s, traits_res_tb_u)
# traits_res_tb_all <- rbind(traits_res_tb_e, traits_res_tb_s, traits_res_tb_u)
# 
# 
# # windows(w = 4,h = 3)
# 
# (p <- dwplot(traits_res_tb_all, 
#              vline = geom_vline(xintercept = 0, colour = "grey80", linetype = 2),
#              # dot_args = list(aes(shape = model)),
#              dot_args = list(size = 1.9),
#              whisker_args = list(size = 1.8)) %>% 
#     relabel_predictors(c(mean_height = "Height", 
#                          mean_seedmass = "Seed mass",
#                          growth_form = "Growth form",
#                          lifecycle = "Life cycle",
#                          max_elev_range = "Elev. range",
#                          lon_centroid = "Lon. centroid",
#                          lat_centroid = "Lat. centroid",
#                          range_size = "Range size")) + 
#     theme_classic() +
#     theme(legend.position = "none",
#           panel.background = element_rect(fill = "transparent"),
#           plot.background = element_rect(fill = "transparent", color = NA),
#           text = element_text(size = 14),
#           axis.text = element_text(colour = "#1B3C59"),
#           #axis.text.y = element_text(angle = -35),
#           axis.line = element_line(linewidth = 0.5)) +
#     # ggtitle(paste0("ESU coefficient estimate (Polytomy age ",age,")")) +
#     xlab("Coefficient estimate") + 
#     ylab("") + 
#     scale_color_manual(values = c("unfilling" = "#FFE875", "stability" = "#A3DDEF","expansion" = "#87CF87"), name = "model"))
# 
# 
# 
# Cairo(file = "plots/trait_analysis/combined_regions.png",  width = 840, height = 680, type="png", pointsize=12, 
#       bg = "white", canvas = "white", units = "px", dpi = "auto")
# 
# dwplot(traits_res_tb_all, 
#              vline = geom_vline(xintercept = 0, colour = "grey80", linetype = 2),
#              # dot_args = list(aes(shape = model)),
#              dot_args = list(size = 1.9),
#              whisker_args = list(size = 1.8)) %>% 
#     relabel_predictors(c(mean_height = "Height", 
#                          mean_seedmass = "Seed mass",
#                          growth_form = "Growth form",
#                          lifecycle = "Life cycle",
#                          max_elev_range = "Elev. range",
#                          lon_centroid = "Lon. centroid",
#                          lat_centroid = "Lat. centroid",
#                          range_size = "Range size")) + 
#     theme_classic() +
#     theme(legend.position = "none",
#           panel.background = element_rect(fill = "transparent"),
#           plot.background = element_rect(fill = "transparent", color = NA),
#           text = element_text(size = 14),
#           axis.text = element_text(colour = "black"),
#           #axis.text.y = element_text(angle = -35),
#           axis.line = element_line(linewidth = 0.5)) +
#     # ggtitle(paste0("ESU coefficient estimate (Polytomy age ",age,")")) +
#     xlab("Coefficient estimate") + 
#     ylab("") + 
#     scale_color_manual(values = c("unfilling" = "#FFE875", "stability" = "#A3DDEF","expansion" = "#87CF87"), name = "model")
# 
# dev.off()
# 
# 
# ggsave(paste0("plots/results/trait_analysis/Coefficient_estimate_ESU_combined.pdf"), p,
#        bg = "transparent",
#        width = 6,
#        height = 5,
#        units = "cm")


# traits_res_e <- traits_res %>%
#   as_tibble() %>% 
#   mutate(model = "expansion", term = Trait, estimate = expansion_coef, std.error = expansion_stderr, statistic = expansion_coef/expansion_stderr, p.value = expansion_p, varimp = expansion_varimp) %>%
#   select(model, term, estimate, std.error, statistic, p.value, varimp) %>% 
#   filter(term %in% covariates)
# 
# traits_res_s <- traits_res %>%
#   as_tibble() %>% 
#   mutate(model = "stability", term = Trait, estimate = stability_coef, std.error = stability_stderr, statistic = stability_coef/stability_stderr, p.value = stability_p, varimp = stability_varimp) %>%
#   select(model, term, estimate, std.error, statistic, p.value, varimp) %>% 
#   filter(term %in% covariates)
# 
# traits_res_u <- traits_res %>%
#   as_tibble() %>% 
#   mutate(model = "unfilling", term = Trait, estimate = unfilling_coef, std.error = unfilling_stderr, statistic = unfilling_coef/unfilling_stderr, p.value = unfilling_p, varimp = unfilling_varimp) %>%
#   select(model, term, estimate, std.error, statistic, p.value, varimp) %>% 
#   filter(term %in% covariates)
# 
# 
# min_exp <- min(traits_res_e$estimate, na.rm = TRUE)
# max_exp <- max(traits_res_e$estimate, na.rm = TRUE)
# 
# min_stb <- min(traits_res_s$estimate, na.rm = TRUE)
# max_stb <- max(traits_res_s$estimate, na.rm = TRUE)
# 
# min_unf <- min(traits_res_u$estimate, na.rm = TRUE)
# max_unf <- max(traits_res_u$estimate, na.rm = TRUE)
# 
# 
# raits_e_wide <- traits_res_e %>% 
#   select(estimate, term) %>% 
#   pivot_wider(names_from = region, values_from = estimate) %>% 
#   select(!term)
# 
# exp_mtx <- as.matrix(traits_e_wide, rownames.force = TRUE)
# rownames(exp_mtx) <- covariates
# 
# 
# traits_s_wide <- traits_res_s %>% 
#   select(region, estimate, term) %>% 
#   pivot_wider(names_from = region, values_from = estimate) %>% 
#   select(!term)
# 
# stb_mtx <- as.matrix(traits_s_wide, rownames.force = TRUE)
# rownames(stb_mtx) <- covariates
# 
# 
# traits_u_wide <- traits_res_u %>% 
#   select(region, estimate, term) %>% 
#   pivot_wider(names_from = region, values_from = estimate) %>% 
#   select(!term)
# 
# unf_mtx <- as.matrix(traits_u_wide, rownames.force = TRUE)
# rownames(unf_mtx) <- covariates