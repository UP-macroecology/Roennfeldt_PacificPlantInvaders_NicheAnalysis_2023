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
load("results/ecospat/niche_ses_results_AC.RData")
# load("results/ecospat/percentages_niche_conservatism.RData")


# unique pacific species
spp_pac <- unique(subset(results_overlap_AC, region == "pac")$species)


rel_niche_dynamics_AC <- rel_niche_dynamics_AC %>%
  mutate(across(!c(species,percentage, total), as.factor)) %>%
  mutate(metric = factor(metric, levels = c("abandonment", "unfilling", "stability", "expansion", "pioneering")))

results_dynamics_AC <- results_dynamics_AC %>%
  filter(inter_method == "inter") %>%
  mutate(across(!c(species, value), as.factor)) %>%
  mutate(metric = factor(dyn_metric, levels = c("unfilling", "stability", "expansion")))

results_ses_AC <- results_ses_AC %>%
  filter(sim_metric == "z.D")
  



# 1. overlap --------------------------------------------------------------

ggplot(results_overlap_AC, aes(x = region, y = schoeners_D)) +
  geom_boxplot() +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = c("Pacific Islands", "Africa", "Australasia", "Europe", "North America", "South America", "temperate Asia", "tropical Asia")) +
  scale_y_continuous(name = "Niche overlap D\n", limits = c(0,1)) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank())





# 2. niche dynamics ------------------------------------------------------

# colour schemes

col_regular <- c("lightsalmon3","lightgoldenrod1","lightblue2","darkseagreen3",  "thistle4")
col_poster <- c("#C76967","#FFE875","#A3DDEF","#87CF87",  "#927290")


# relative dynamics
ggplot(rel_niche_dynamics_AC, aes(x = region, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5) +
  labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = c("Pacific Islands\n n = 328", "Africa\n n = 223", "Australasia\n n = 227", "Europe\n n = 96", "N. America\n n = 209", "S. America\n n = 227", "temp. Asia\n n = 183", "trop. Asia\n n = 153")) +
  scale_fill_manual(name = "Niche dynamics", 
                    values = col_regular) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank(),
        plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA,),
        legend.position = "none")





# relative dynamics - Prague poster -> WHITE
p <- ggplot(rel_niche_dynamics_AC, aes(x = region, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5,
               colour = "#F2F2F2") +
  labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = c("Pacific Islands\n n = 328", "Africa\n n = 223", "Australasia\n n = 227", "Europe\n n = 96", "N. America\n n = 209", "S. America\n n = 227", "temp. Asia\n n = 183", "trop. Asia\n n = 153")) +
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
                   labels = c("Pacific Islands\n n = 328", "Africa\n n = 223", "Australasia\n n = 227", "Europe\n n = 96", "N. America\n n = 209", "S. America\n n = 227", "temp. Asia\n n = 183", "trop. Asia\n n = 153")) +
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


# original ecospat output
ggplot(results_dynamics_AC, aes(x = region, y = value, fill = dyn_metric)) +
  geom_boxplot(fatten = 1.5) +
  labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = c("Pacific Islands", "Africa", "Australasia", "Europe", "North America", "South America", "temperate Asia", "tropical Asia")) +
  scale_fill_manual(name = "Niche dynamics", 
                    values = c("darkseagreen3", "lightblue2","lightgoldenrod1")) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank(),
        legend.box.spacing = unit(3, "pt"),
        plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"))


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
                   labels = c("Pacific Islands", "Africa", "Australasia", "Europe", "North America", "South America", "temperate Asia", "tropical Asia")) +
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
  pivot_longer(!c(species, region), names_to = "metric", values_to = "percentage")

# plot ESU niche dynamcis

col_poster <- c("#FFE875","#A3DDEF","#87CF87")

# relative dynamics - Prague poster -> BLACK
(p <- ggplot(stand_ESU, aes(x = region, y = percentage, fill = metric)) +
    geom_boxplot(fatten = 1.5) +
    labs(x = "Non-native region", y = "Niche dynamics (%)") +
    scale_x_discrete(name = "\nNon-native region",
                     limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                     labels = c("Pacific Islands\n n = 317", "Africa\n n = 212", "Australasia\n n = 217", "Europe\n n = 83", "N. America\n n = 200", "S. America\n n = 225", "temp. Asia\n n = 173", "trop. Asia\n n = 148")) +
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

