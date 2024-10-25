#' ---------------------------
#
# Purpose of script: creating figures to visualise the results of the niche comparison
# Author: Anna Rönnfeldt
# Date Created: 2024-10-25
# Email: roennfeldt@uni-potsdam.de
#
# Notes: the script contains code for multiple different figure versions (e.g., for the publication or for printed posters)
#
#' ---------------------------



library(dplyr)
library(ggplot2)
library(maps)
library(tidyr) # for %>% gather()
library(terra)
library(viridis)

rm(list = ls())

# load data ---------------------------------------------------------------

load("results/ecospat/master_results_AC.RData")
load("results/ecospat/rel_niche_dynamics_results_AC.RData")
load("results/ecospat/niche_dynamics_results_AC.RData")
load("results/ecospat/niche_overlap_results_AC.RData")
# load("results/ecospat/niche_ses_results_AC.RData")
# load("results/ecospat/percentages_niche_conservatism.RData")


# # unique pacific species
# spp_pac <- unique(subset(results_overlap_AC, region == "pac")$species)
# 
# 
rel_niche_dynamics_AC <- rel_niche_dynamics_AC %>%
  mutate(across(!c(species,percentage, total), as.factor)) %>%
  mutate(metric = factor(metric, levels = c("abandonment", "unfilling", "stability", "expansion", "pioneering"))) %>% 
  mutate(percentage = percentage *100)

results_dynamics_AC <- results_dynamics_AC %>%
  filter(inter_method == "inter") %>%
  mutate(across(!c(species, value), as.factor)) %>%
  mutate(metric = factor(dyn_metric, levels = c("unfilling", "stability", "expansion")))

# results_ses_AC <- results_ses_AC %>%
#   filter(sim_metric == "z.D")
#   

col_regular <- c("lightsalmon3","lightgoldenrod1","lightblue2","darkseagreen3",  "thistle4")
col_poster <- c("#C76967","#FFE875","#A3DDEF","#87CF87",  "#927290")

label_regional <- c("Pacific Islands\n n = 317", "Africa\n n = 214", "Australasia\n n = 219", "Europe\n n = 90", "N. America\n n = 204",  "S. America\n n = 219", "temp. Asia\n n = 179", "trop. Asia\n n = 151")


# 1. overlap --------------------------------------------------------------

ggplot(results_overlap_AC, aes(x = region, y = schoeners_D)) +
  geom_boxplot(fill = "grey") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = c("Pacific Islands\n n = 317", "Africa\n n = 214", "Australasia\n n = 219", "Europe\n n = 90", "N. America\n n = 204", "South America\n n = 219", "temp. Asia\n n = 179", "trop. Asia\n n = 151")) +
  scale_y_continuous(name = "Niche overlap D\n", limits = c(0,1)) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank())





# 2. niche dynamics ------------------------------------------------------


# relative dynamics - per region
(p1 <- ggplot(rel_niche_dynamics_AC, aes(x = region, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5, colour = "black", , linewidth = 0.4, outlier.size = 0.5) +
  labs(x = "Non-native region", y = "Niche changes (%)\n") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  scale_fill_manual(name = "Niche changes", 
                    labels = c("Abandonment", "Unfilling", "Stability", "Expansion", "Pioneering"),
                    values = col_regular) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank(),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
        # panel.background = element_rect(fill = "transparent"),
        # plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "top",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,-10,-10,-10),
        legend.key.size = unit(0.4, "cm"),
        # legend.key.spacing = unit(0.25, "cm"),
        axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 11)))

ggsave("plots/results/all_dynamics.jpg", p1, 
       width = 16,
       height = 10,
       units = "cm")


png("plots/niche_dynamics/regional_dynamics_all.png",width = 1450, height = 947)
print(p1)
dev.off()

# relative dynamics
p2 <- ggplot(rel_niche_dynamics_AC, aes(x = metric, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5) +
  labs(x = NULL, y = "Niche dynamics (%)\n") +
  theme_bw(base_size = 20) +
  scale_fill_manual(name = "Niche dynamics", 
                    values = col_regular) +
  scale_x_discrete(limits = c("abandonment", "unfilling", "stability", "expansion", "pioneering"),
                   labels = c("Abandonment", "Unfilling", "Stability", "Expansion", "Pioneering")) +
  theme(panel.grid.major = element_blank(),
        plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA,),
        legend.position = "none")

png("plots/niche_dynamics/dynamics_all.png",width = 1050, height = 947)
print(p2)
dev.off()

# relative dynamics - Prague poster -> WHITE
p <- ggplot(rel_niche_dynamics_AC, aes(x = region, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5,
               colour = "#F2F2F2") +
  labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  scale_fill_manual(name = "Niche dynamics", 
                    values = col_poster) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA,),
        legend.position = "none",
        axis.title = element_text(colour = "#F2F2F2"),
        axis.text = element_text(colour = "#F2F2F2"),
        axis.line = element_line(colour = "#F2F2F2"),
        axis.ticks = element_line(colour = "#F2F2F2"))

# relative dynamics - Prague poster -> BLACK
(p <- ggplot(rel_niche_dynamics_AC, aes(x = region, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5) +
  labs(x = "Non-native region", y = "Niche dynamics (%)") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  scale_fill_manual(name = "Niche dynamics", 
                    values = col_poster) +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"), 
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA,),
        legend.position = "none",
        axis.text = element_text(colour = "#1B3C59")))

ggsave("plots/results/Niche_dynamics_poster.jpg", p, 
       bg = "transparent",
       width = 35,
       height = 17,
       units = "cm")


results_ecospat <- master_results_AC %>%
  select(c(species, region, expansion, stability, unfilling)) %>% 
  pivot_longer(!c(species, region), names_to = "dyn_metric", values_to = "percentage") %>%
  mutate(across(!c(species, percentage), as.factor)) %>%
  mutate(dyn_metric = factor(dyn_metric, levels = c("unfilling", "stability", "expansion")))
  
  
  
# original ecospat output
# ggplot(results_ecospat, aes(x = region, y = percentage, fill = dyn_metric)) +
#   geom_boxplot(fatten = 1.5) +
#   labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
#   scale_x_discrete(name = "\nNon-native region",
#                    limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
#                    labels = c("Pacific Islands\n n = 317", "Africa\n n = 214", "Australasia\n n = 219", "Europe\n n = 96", "N. America\n n = 204", "South America\n n = 219", "temp. Asia\n n = 179", "trop. Asia\n n = 151")) +
#   scale_fill_manual(name = "Niche dynamics", 
#                     labels = c("Unfilling", "Stability", "Expansion"),
#                     values = c("lightgoldenrod1", "lightblue2", "darkseagreen3")) +
#   theme_bw(base_size = 20) +
#   theme(panel.grid.major = element_blank(),
#         legend.position = "top",
#         plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"))



df_es_original <- subset(results_ecospat, dyn_metric == "expansion" | dyn_metric == "stability")

p3 <- ggplot(df_es_original, aes(x = region, y = percentage, fill = dyn_metric)) +
  geom_boxplot(fatten = 1.5) +
  labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  scale_fill_manual(name = "Niche dynamics", 
                    labels = c("Stability", "Expansion"),
                    values = c("lightblue2", "darkseagreen3")) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank(),
        legend.position = "top",
        plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"))

png("plots/niche_dynamics/regional_dynamics_ecospat_ES.png",width = 1050, height = 947)
print(p3)
dev.off()


df_es_original <- subset(results_ecospat, dyn_metric == "unfilling")

p4 <- ggplot(df_es_original, aes(x = region, y = percentage, fill = dyn_metric)) +
  geom_boxplot(fatten = 1.5) +
  labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  scale_fill_manual(name = "Niche dynamics", 
                    labels = "Unfilling",
                    values = "lightgoldenrod1") +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"))

png("plots/niche_dynamics/regional_dynamics_ecospat_U.png",width = 850, height = 947)
print(p4)
dev.off()


# Niche conservatism ------------------------------------------------------

# getting an idea for the regional trends:
pac <- subset(master_results_AC, region == "pac")
nrow(pac)
table(pac$similarity)

afr <- subset(master_results_AC, region == "afr")
nrow(afr)
table(afr$similarity)

aus <- subset(master_results_AC, region == "aus")
nrow(aus)
table(aus$similarity) 

eur <- subset(master_results_AC, region == "eur")
nrow(eur)
table(eur$similarity) 

nam <- subset(master_results_AC, region == "nam")
nrow(nam)
table(nam$similarity)

sam <- subset(master_results_AC, region == "sam")
nrow(sam)
table(sam$similarity)

ate <- subset(master_results_AC, region == "ate")
nrow(ate)
table(ate$similarity)

atr <- subset(master_results_AC, region == "atr")
nrow(atr)
table(atr$similarity)


# very basic bar plot to explore results:

df_con <- data.frame(similarity = c("Conservatism", "Neither", "Switching"),
               freq = c(719, 874, 0))

(p <- ggplot(df_con, aes(x = similarity, y = freq)) +
  labs(x = NULL, y = NULL) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Niche \nconservatism", "Neither", "Niche \nswitching")) +
  ylim(0,950) +
  theme_bw(base_size = 20) +
  theme(axis.line = element_line(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA,),
        axis.text = element_text(colour = "#1B3C59")))

ggsave("plots/results/Niche_conservatism_bar.png", p, 
       bg = "transparent",
       width = 13,
       height = 18,
       units = "cm")


# stand. ESU --------------------------------------------------------------

# TODO: move this seciton to the results_overview script and save the file there to load it back in here
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

#t <- subset(stand_ESU, region == "pac")

#summary(t)
# t <- master_results_AC %>% filter(species %in% c("Agave sisalana", "Spathodea campanulata"))
# 
# t2 <- t

# plot ESU niche dynamcis

col_ESU <- c("#FFEC8B","#B2DFEE","#9FD39F")
col_ESU_poster <- c("#FFE875","#A3DDEF","#87CF87")
# col_poster <- c("#FFE875","#A3DDEF","#87CF87")

# relative dynamics 
(p <- ggplot(stand_ESU, aes(x = region, y = percentage, fill = metric)) +
    geom_boxplot(fatten = 1.5, colour = "black", , linewidth = 0.4, outlier.size = 0.5) +
    labs(x = "Non-native region", y = "Niche changes (%)") +
    scale_x_discrete(name = "Non-native region",
                     limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                     labels = label_regional) +
    scale_fill_manual(name = "Niche changes", 
                      labels = c("Unfilling", "Stability", "Expansion"),
                      values = col_ESU) +
    theme_bw(base_size = 20) +
    theme(panel.grid.major = element_blank(),
          plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
          # panel.background = element_rect(fill = "transparent"),
          # plot.background = element_rect(fill = "transparent", color = NA),
          legend.position = "top",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 11),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,-10,-10,-10),
          axis.text = element_text(size = 9, color = "black"),
          axis.title = element_text(size = 11)))
          # axis.text = element_text(colour = "#1B3C59")))

ggsave("plots/results/ESU_dynamics.jpg", p, 
       width = 16,
       height = 11,
       units = "cm")


(p_poster <- ggplot(stand_ESU, aes(x = region, y = percentage, fill = metric)) +
    geom_boxplot(fatten = 1.5, colour = "black", , linewidth = 0.7, outlier.size = 0.5) +
    labs(x = "Non-native region", y = "Niche changes (%)") +
    scale_x_discrete(name = "Non-native region",
                     limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                     labels = label_regional) +
    scale_fill_manual(name = "Niche changes", 
                      labels = c("Unfilling", "Stability", "Expansion"),
                      values = col_ESU_poster) +
    theme_bw(base_size = 22) +
    theme(panel.grid.major = element_blank(),
          plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
          legend.position = "null",
          legend.text = element_text(size = 10, color = "#1B3C59"),
          legend.title = element_text(size = 11, color = "#1B3C59"),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0,-10,-10,-10),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA),
          axis.text = element_text(size = 24, color = "#1B3C59"),
          # axis.title = element_text(size = 11, color = "#1B3C59"),
          axis.title = element_blank()))
# axis.text = element_text(cNULL# axis.text = element_text(colour = "#1B3C59")))


ggsave("plots/results/ESU_dynamics_poster.png", p_poster, 
       bg = "transparent",
       width = 45,
       height = 18,
       units = "cm")
# 
# png("plots/niche_dynamics/regional_dynamics_ESU.png",width = 1450, height = 947)
# print(p)
# dev.off()
# 
# png("plots/niche_dynamics/dynamics_ESU.png",width = 1050, height = 947)
# print(p2)
# dev.off()







# barplot similarity results  ---------------------------------------------

rm(list = ls())

perc_group <- function(x) {
  
  if(x == 0) {g <- 0}
  if(x > 0  & x <= 25) {g <- 1}
  if(x > 25 & x <= 50) {g <- 2}
  if(x > 50 & x <= 75) {g <- 3}
  if(x > 75 & x <= 100) {g <- 4}
  # if(x == 100) {g <- 5}
  
  return(g)
}

load("results/ecospat/master_results_AC.RData")

df_con <- master_results_AC %>% 
  group_by(species, similarity) %>% 
  tally() %>% 
  pivot_wider(names_from=similarity, values_from=n) %>% 
  mutate(across(everything(), .fns=~replace_na(., 0))) %>% 
  mutate(perc_conservatism = NA) %>% 
  mutate(perc_neither = NA) %>% 
  mutate(perc_con_group = NA) %>% 
  mutate(perc_nei_group = NA) %>% 
  mutate(perc_swi_group = 5)


rm(master_results_AC)

for (r in 1:nrow(df_con)) {
  
  c <- as.numeric(df_con[r,"conservatism"])
  n <- as.numeric(df_con[r,"neither"])
  
  # total number of introduction events
  t <- c + n
  
  
  perc_con <- round(100/t*c, 2)
  perc_nei <- round(100/t*n, 2)
  
  
  g_con <- perc_group(perc_con)
  g_nei <- perc_group(perc_nei)
  
  
  # calculate and add percentage values to df
  df_con[r,"perc_conservatism"] <- perc_con
  df_con[r,"perc_neither"] <- perc_nei
  df_con[r,"perc_con_group"] <- g_con
  df_con[r,"perc_nei_group"] <- g_nei
  
} # end of for loop over df_con


rm(list = setdiff(ls(), "df_con"))


df_plot <- df_con %>% 
  select(species, perc_con_group, perc_nei_group, perc_swi_group) %>% 
  rename("conservatism" = "perc_con_group",
         "non_significant"= "perc_nei_group",
         "switching" = "perc_swi_group") %>% 
  pivot_longer(cols = c(conservatism, non_significant, switching), names_to = "observation", values_to = "perc_group") %>% 
  dplyr::filter(!perc_group == 0) %>% 
  mutate(perc_group = factor(perc_group, levels = c(1,2,3,4,5))) %>% 
  mutate(perc_group = replace(perc_group, perc_group == 5, NA)) %>% 
  mutate(perc_group = factor(perc_group, levels = c(1,2,3,4)))




# legend_labels <- c("0 < X <= 25", "25 < X <= 50", "50 < X <= 75", "75 < X <= 100")

# legend_labels <- c(expression("0 < x" <= "25"), expression("25 < x" <= "50"), expression("50 < x" <= "75"), expression("75 < x" < "100"), expression("x = 100"))
legend_labels <- c(expression("" <= "25%"), expression("" <= "50%"), expression("" <= "75%"), expression("" <= "100%"))


col_gradient <- c("#E0C9AA", "#D4B387", "#725546", "#594236")

col_gradient <- c("#C1C9D1", "#8B9BA9", "#5E7080", "#3E4A54")

(p <- ggplot(df_plot, aes(observation, group = perc_group)) +
    geom_bar(aes(fill = perc_group), position = "dodge") +
    xlab(NULL) +
    ylab("Number of species") +
    scale_fill_manual(name = "Regions with\nthis pattern:", 
                      labels = legend_labels,
                      values = col_gradient,
                      na.value = "transparent") +
    # scale_fill_viridis_d(option = "G", begin = 0.9, end = 0.2, alpha = 1, na.value = "transparent",
    #                      labels = legend_labels, name = "Consistency across\nregions:") +
    scale_y_continuous(n.breaks = 8, limits = c(0,116), expand = c(0.01, 0)) +
    scale_x_discrete(labels = c("Niche\n conservatism", "n.s.", "Niche\n switching")) +
    # guides(fill = guide_legend(title = "Consistency across regions"), theme(legend.text = element_text(size = 8))) +
    theme_bw() +
    theme(legend.title = element_text(size = 8),
          legend.position = "right",
          legend.spacing.x = unit(0.1, 'cm'),
          legend.key.size = unit(0.5,"line"),
          legend.text = element_text(size = 8),
          legend.margin = margin(0,0,0,5),
          legend.box.margin = margin(0.5,-0.5,-10,-10),
          axis.text = element_text(size = 8, color = "black"),
          panel.grid.major = element_blank()))

ggsave("plots/results/Similarity_percentage_100.jpg", p,
       bg = "transparent",
       width = 8,
       height = 6,
       units = "cm")

(p_poster <- ggplot(df_plot, aes(observation, group = perc_group)) +
    geom_bar(aes(fill = perc_group), position = "dodge", color = "#494949", linewidth = 0.2, width = 0.9) +
    xlab(NULL) +
    ylab("Number of species") +
    scale_fill_manual(labels = legend_labels,
                      # name = "Consistency across regions:", 
                      values = col_gradient,
                      na.value = "transparent") +
    scale_y_continuous(n.breaks = 8, limits = c(0,116), expand = c(0.01, 0)) +
    scale_x_discrete(labels = c("Niche\n conservatism", "n.s.", "Niche\n switching")) +
    theme_bw() +
    theme(legend.title = element_blank(),
          # legend.title = element_text(size = 10, color = "#1B3C59"),
          legend.position = "top",
          legend.spacing.x = unit(0.2, 'cm'),
          legend.key.size = unit(0.5,"line"),
          legend.text = element_text(size = 10, color = "#1B3C59"),
          legend.margin = margin(0,0,3,5),
          legend.box.margin = margin(0.5,-0.5,-10,-10),
          axis.text = element_text(size = 10, color = "#1B3C59"),
          axis.title = element_text(size = 11, color = "#1B3C59"),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.key = element_rect(fill = "transparent", color = NA)))


ggsave("plots/results/Similarity_poster.png", p_poster,
       bg = "transparent",
       width = 9.5,
       height = 12,
       units = "cm")





# original:
# c("#6F5449", "#EAB464", "#919AA1")



#  regions per species ----------------------------------------------------

df_reg <- master_results_AC %>% 
  group_by(species, region) %>% 
  tally() %>% 
  pivot_wider(names_from=region, values_from=n) %>% 
  mutate(across(everything(), .fns=~replace_na(., 0))) 

df_reg$n_regions <- rowSums(df_reg[,2:9])

# mean and median:
summary(df_reg$n_regions)
# SD
sd(df_reg$n_regions)




# linerange plots ---------------------------------------------------------

## count version similarity results -------------------------------------------
library(dplyr)
library(dotwhisker)
library(ggplot2)
library(tidyr)


rm(list = ls())

load("results/ecospat/master_results_AC.RData")

ID <- c(1:317)

df_con <- master_results_AC %>% 
  group_by(species, similarity) %>% 
  tally() %>% 
  pivot_wider(names_from=similarity, values_from=n) %>% 
  mutate(across(everything(), .fns=~replace_na(., 0))) %>% 
  mutate(total = conservatism + neither) %>%
  mutate(conservatism = conservatism * (-1)) %>% 
  mutate(perc_con = (round(100/total*abs(conservatism), 2))*(-1)) %>% 
  mutate(perc_neither = round(100/total*neither, 2)) 

df_con <- data.frame(species_ID = ID, df_con)


# # species with neither = 0 and then ordered by conservatism
# set_1 <- df_con %>%
#   filter(neither == 0) %>% 
#   arrange(-conservatism)
# 
# # species with neither > 0, but mode conservatism, ordered by conservatism
# set_2 <- df_con %>%
#   mutate(conservatism = conservatism * (-1)) %>% 
#   filter(neither > 0 & conservatism > neither) %>% 
#   mutate(conservatism = conservatism * (-1)) %>% 
#   arrange(-conservatism) 
# 
# # species with conservatism == neither
# set_3 <- df_con %>%
#   mutate(conservatism = conservatism * (-1)) %>% 
#   filter(conservatism == neither) %>% 
#   mutate(conservatism = conservatism * (-1)) %>% 
#   arrange(-conservatism) 
# 
# # species with conservatism >0, but mode neither, ordered by neither
# set_4 <- df_con %>%
#   mutate(conservatism = conservatism * (-1)) %>% 
#   filter(conservatism > 0 & conservatism < neither) %>% 
#   arrange(-neither) %>% 
#   mutate(conservatism = conservatism * (-1))
# 
# # species with conservatism = 0, ordered by neither
# set_5 <- df_con %>%
#   mutate(conservatism = conservatism * (-1)) %>% 
#   filter(conservatism == 0) %>% 
#   arrange(-neither) %>% 
#   mutate(conservatism = conservatism * (-1))
# 
# 
# df_plot <- rbind(set_5, set_4, set_3, set_2, set_1)
# 
# df_plot %>%
#   ggplot(aes(y = factor(species_ID, levels = unique(species_ID)))) +
#   geom_linerange(aes(xmin = conservatism, xmax = neither)) +
#   geom_vline(xintercept = 0, col = "black") +
#   geom_vline(xintercept = c(-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7), col = "lightgrey") +
#   scale_x_continuous(breaks = c(-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7)) +
#   # position = "top") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# # theme(panel.grid.major = element_blank(),
# #       panel.grid.minor = element_blank(),
# #       axis.ticks.y = element_blank(),
# #       axis.text.y = element_blank())


## linerange percentage version -------------------------------------------------

# species with neither = 0 and then ordered by conservatism
set_1 <- df_con %>%
  filter(perc_neither == 0) %>% 
  arrange(-perc_con) %>% 
  mutate(set = as.factor(1))


# species with neither > 0, but mode conservatism, ordered by conservatism
set_2 <- df_con %>%
  mutate(perc_con = perc_con * (-1)) %>% 
  filter(perc_neither > 0 & perc_con > perc_neither) %>% 
  mutate(perc_con = perc_con * (-1)) %>% 
  arrange(-perc_con) %>% 
  mutate(set = as.factor(2))

# species with conservatism == neither
set_3 <- df_con %>%
  mutate(perc_con = perc_con * (-1)) %>% 
  filter(perc_con == perc_neither) %>% 
  mutate(perc_con = perc_con * (-1)) %>% 
  arrange(-perc_con) %>% 
  mutate(set = as.factor(3))

# species with conservatism >0, but mode neither, ordered by neither
set_4 <- df_con %>%
  mutate(perc_con = perc_con * (-1)) %>% 
  filter(perc_con > 0 & perc_con < perc_neither) %>% 
  arrange(-perc_neither) %>% 
  mutate(perc_con = perc_con * (-1)) %>% 
  mutate(set = as.factor(4))

# species with conservatism = 0, ordered by neither
set_5 <- df_con %>%
  mutate(perc_con = perc_con * (-1)) %>% 
  filter(perc_con == 0) %>% 
  arrange(-perc_neither) %>% 
  mutate(perc_con = perc_con * (-1)) %>% 
  mutate(set = as.factor(5))


df_plot <- rbind(set_5, set_4, set_3, set_2, set_1)

col_perc <- c("#80A8C9","#506F89","#101819","#809E51", "#C6C981")
# col_perc <- c("#BFC270","#809E51","#101819","#506F89", "#80A8C9")


# plot main figure structure 
# (everything else (labels etc.) will be added in another software))
df_plot %>%
  ggplot(aes(y = factor(species_ID, levels = unique(species_ID)))) +
  geom_linerange(aes(xmin = perc_con, xmax = perc_neither, colour = set)) +
  scale_color_manual(values = col_perc) +
  # scale_color_manual(values = col_perc) +
  geom_segment(x = -100, y = 1, xend = 100, yend = 1, color = "black") + 
  geom_segment(x = -100, y = 317, xend = 100, yend = 317, color = "black") + 
  # geom_hline(yintercept = c(1, 317), col = "black") +
  geom_vline(xintercept = c(-75,-50,-25,25,50,75), col = "darkgrey", linetype = 3) +
  geom_vline(xintercept = 0, col = "black") +
  # geom_vline(xintercept = c(-100,100), col = "black") +
  scale_x_continuous(limits = c(-100,100),
                     labels = c(100,75,50,25,25,50,75,100),
                     # breaks = c(-87.5,-62.5,-37.5,-12.5,12.5,37.5,62.5,87.5),
                     breaks = c(-100,-75,-50,-25,25,50,75,100),
                     position = "top") +
  ylab(NULL) +
  # position = "top") +
  theme_void() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "null")



