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
covariates <- c("mean_height", "mean_seedmass", "growth_form", "lifecycle", "max_elev_range", "range_size_nat",
                "niche_breadth_nat", "niche_centroid_a_nat", "niche_centroid_b_nat", "years_since_intro", "eucl_dist")

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





# testing -----------------------------------------------------------------


setting <- "mod_seed"
# setting <- "original"


if (setting == "mod_seed") {
  traits_res_u[traits_res_u$region == "eur" & traits_res_u$term == "mean_seedmass","estimate"] <- -4.962
  traits_res_e[traits_res_e$region == "pac" & traits_res_e$term == "mean_seedmass","estimate"] <- -3.420
  traits_res_s[traits_res_s$region == "sam" & traits_res_s$term == "mean_seedmass","estimate"] <- -2.221
  traits_res_s[traits_res_s$region == "eur" & traits_res_s$term == "mean_seedmass","estimate"] <- 4.046
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

min_exp <- min(traits_res_e$estimate, na.rm = TRUE)
max_exp <- max(traits_res_e$estimate, na.rm = TRUE)
min_stb <- min(traits_res_s$estimate, na.rm = TRUE)
max_stb <- max(traits_res_s$estimate, na.rm = TRUE)
min_unf <- min(traits_res_u$estimate, na.rm = TRUE)
max_unf <- max(traits_res_u$estimate, na.rm = TRUE)

# windows()
# 

# 
# corrplot(e_efs_mtx, 
#          col.lim = c(min_exp, max_exp), 
#          is.corr = FALSE,
#          tl.col = txt_col,
#          na.label = "square",
#          na.label.col = na_col,
#          outline = TRUE)
# 
# corrplot_mod(m_imp = e_imp_mtx, 
#              m_efs = e_efs_mtx,
#              col.lim = c(min_exp, max_exp), 
#              is.corr = FALSE,
#              tl.col = txt_col,
#              na.label = "square",
#              na.label.col = na_col,
#              outline = TRUE)
# 
# 
# corrplot(s_efs_mtx, 
#          col.lim = c(min_stb, max_stb), 
#          is.corr = FALSE,
#          tl.col = txt_col,
#          na.label = "square",
#          na.label.col = na_col,
#          outline = TRUE)
# 
# corrplot_mod(m_imp = s_imp_mtx,
#              m_efs = s_efs_mtx, 
#              col.lim = c(min_stb, max_stb), 
#              is.corr = FALSE,
#              tl.col = txt_col,
#              na.label = "square",
#              na.label.col = na_col,
#              outline = TRUE)
# 
# corrplot(u_efs_mtx, 
#          col.lim = c(min_unf, max_unf), 
#          is.corr = FALSE,
#          tl.col = txt_col,
#          na.label = "square",
#          na.label.col = na_col,
#          outline = TRUE)
# 
# corrplot_mod(m_imp = u_imp_mtx, 
#              m_efs = u_efs_mtx,
#              col.lim = c(min_unf, max_unf), 
#              is.corr = FALSE,
#              tl.col = txt_col,
#              na.label = "square",
#              na.label.col = na_col,
#              outline = TRUE)

txt_col <- "black"
na_col <- "white"

Cairo(file = "plots/trait_analysis/regional_expansion_seedmod.png",  width = 640, height = 480, type="png", pointsize=12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")

corrplot_mod(m_imp = e_imp_mtx,
             m_efs = e_efs_mtx,
             col.lim = c(min_exp, max_exp),
             is.corr = FALSE,
             tl.col = txt_col,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()

Cairo(file = "plots/trait_analysis/regional_stability_seedmod.png",  width = 640, height = 480, type="png", pointsize=12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")


corrplot_mod(m_imp = s_imp_mtx,
             m_efs = s_efs_mtx, 
             col.lim = c(min_stb, max_stb), 
             is.corr = FALSE,
             tl.col = txt_col,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()

Cairo(file = "plots/trait_analysis/regional_unfilling_seedmod.png",  width = 640, height = 480, type = "png", pointsize = 12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")

corrplot_mod(m_imp = u_imp_mtx, 
             m_efs = u_efs_mtx,
             col.lim = c(min_unf, max_unf), 
             is.corr = FALSE,
             tl.col = txt_col,
             na.label = "square",
             na.label.col = na_col,
             outline = TRUE)

dev.off()




# correct for seedmass ----------------------------------------------------










# combined regions --------------------------------------------------------

rm(list = ls())

covariates <- c("mean_height", "growth_form", "lifecycle", "max_elev_range", "lon_centroid", "lat_centroid", "range_size_nat",
                "niche_breadth_nat", "niche_centroid_a_nat", "niche_centroid_b_nat", "years_since_intro", "eucl_dist", "CAI")

# load in results 
traits_res <- read.csv(paste0("results/trait_analysis/combined_regions/results_TraitAnal_df_ESU.csv"))

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


# windows(w = 4,h = 3)

(p <- dwplot(traits_res_tb_all, 
             vline = geom_vline(xintercept = 0, colour = "grey80", linetype = 2),
             # dot_args = list(aes(shape = model)),
             dot_args = list(size = 1.9),
             whisker_args = list(size = 1.8)) %>% 
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
          text = element_text(size = 14),
          axis.text = element_text(colour = "#1B3C59"),
          #axis.text.y = element_text(angle = -35),
          axis.line = element_line(linewidth = 0.5)) +
    # ggtitle(paste0("ESU coefficient estimate (Polytomy age ",age,")")) +
    xlab("Coefficient estimate") + 
    ylab("") + 
    scale_color_manual(values = c("unfilling" = "#FFE875", "stability" = "#A3DDEF","expansion" = "#87CF87"), name = "model"))



Cairo(file = "plots/trait_analysis/combined_regions.png",  width = 840, height = 680, type="png", pointsize=12, 
      bg = "white", canvas = "white", units = "px", dpi = "auto")

dwplot(traits_res_tb_all, 
             vline = geom_vline(xintercept = 0, colour = "grey80", linetype = 2),
             # dot_args = list(aes(shape = model)),
             dot_args = list(size = 1.9),
             whisker_args = list(size = 1.8)) %>% 
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
          text = element_text(size = 14),
          axis.text = element_text(colour = "black"),
          #axis.text.y = element_text(angle = -35),
          axis.line = element_line(linewidth = 0.5)) +
    # ggtitle(paste0("ESU coefficient estimate (Polytomy age ",age,")")) +
    xlab("Coefficient estimate") + 
    ylab("") + 
    scale_color_manual(values = c("unfilling" = "#FFE875", "stability" = "#A3DDEF","expansion" = "#87CF87"), name = "model")

dev.off()


ggsave(paste0("plots/results/trait_analysis/Coefficient_estimate_ESU_combined.pdf"), p,
       bg = "transparent",
       width = 6,
       height = 5,
       units = "cm")


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