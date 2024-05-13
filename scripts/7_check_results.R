# Author: Anna Rönnfeldt
# Date: 2023/11/16
# Purpose: get an overview over the current results for the niche comparison

# preamble ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr) # for %>% gather()

rm(list = ls())


# load data ---------------------------------------------------------------


# load("results/ecospat/master_results.RData")
# load("results/ecospat/rel_niche_dynamics_results.RData")
# load("results/ecospat/niche_dynamics_results.RData")
# load("results/ecospat/niche_overlap_results.RData")
# load("results/ecospat/niche_ses_results.RData")


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
  mutate(metric = factor(metric, levels = c("abandonment", "unfilling", "stability", "expansion", "pioneering"))) 

results_dynamics_AC <- results_dynamics_AC %>%
  filter(inter_method == "inter") %>%
  mutate(across(!c(species, value), as.factor)) %>%
  mutate(metric = factor(dyn_metric, levels = c("unfilling", "stability", "expansion")))

# results_ses_AC <- results_ses_AC %>%
#   filter(sim_metric == "z.D")
#   

col_regular <- c("lightsalmon3","lightgoldenrod1","lightblue2","darkseagreen3",  "thistle4")
col_poster <- c("#C76967","#FFE875","#A3DDEF","#87CF87",  "#927290")

label_regional <- c("Pacific Islands\n n = 317", "Africa\n n = 214", "Australasia\n n = 219", "Europe\n n = 90", "N. America\n n = 204",  "South America\n n = 219", "temp. Asia\n n = 179", "trop. Asia\n n = 151")


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
p1 <- ggplot(rel_niche_dynamics_AC, aes(x = region, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5) +
  labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  scale_fill_manual(name = "Niche dynamics", 
                    labels = c("Abandonment", "Unfilling", "Stability", "Expansion", "Pioneering"),
                    values = col_regular) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank(),
        plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "top")

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

ggsave("plots/results/Niche_dynamics.png", p, 
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

# 3. niche conservatism per region -------------------------------------------

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



# 4. ses ---------------------------------------------------------------------

ses_conservatism <- subset(results_ses_AC, sim_setting == "conservatism")


ggplot(results_ses_AC, aes(x = region, y = value, fill = sim_setting)) +
  geom_boxplot() +
  labs(x = "Non-native region", y = "SES") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank(),
        legend.box.spacing = unit(3, "pt"),
        plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"))



# 5. Niche conservatism ---------------------------------------------------

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


# 6. stand. ESU --------------------------------------------------------------

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
  mutate(metric = factor(metric, levels = c("unfilling", "stability", "expansion")))


t <- subset(stand_ESU, region == "pac")

summary(t)
# t <- master_results_AC %>% filter(species %in% c("Agave sisalana", "Spathodea campanulata"))
# 
# t2 <- t

# plot ESU niche dynamcis

col_ESU <- c("lightgoldenrod1","lightblue2","darkseagreen3")
# col_poster <- c("#FFE875","#A3DDEF","#87CF87")

# relative dynamics - Prague poster -> BLACK
(p <- ggplot(stand_ESU, aes(x = region, y = percentage, fill = metric)) +
    geom_boxplot(fatten = 1.5) +
    labs(x = "Non-native region", y = "Niche dynamics (%)") +
    scale_x_discrete(name = "\nNon-native region",
                     limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                     labels = label_regional) +
    scale_fill_manual(name = "Niche dynamics", 
                      labels = c("Unfilling", "Stability", "Expansion"),
                      values = col_ESU) +
    theme_bw(base_size = 20) +
    theme(panel.grid.major = element_blank(),
          plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.position = "top"))
          # axis.text = element_text(colour = "#1B3C59")))


p2 <- ggplot(stand_ESU, aes(x = metric, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5) +
  labs(x = NULL, y = "Niche dynamics (%)\n") +
  theme_bw(base_size = 20) +
  scale_fill_manual(name = "Niche dynamics", 
                    values = col_ESU) +
  scale_x_discrete(limits = c("unfilling", "stability", "expansion"),
                   labels = c("Unfilling", "Stability", "Expansion")) +
  theme(panel.grid.major = element_blank(),
        plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none")


png("plots/niche_dynamics/regional_dynamics_ESU.png",width = 1450, height = 947)
print(p)
dev.off()

png("plots/niche_dynamics/dynamics_ESU.png",width = 1050, height = 947)
print(p2)
dev.off()

