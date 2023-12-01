# Author: Anna Rönnfeldt
# Date: 2023/11/16
# Purpose: get an overview over the current results for the niche comparison

# preamble ----------------------------------------------------------------

library(dplyr)
library(ggplot2)

rm(list = ls())


load("results/master_results_new.RData")
load("results/rel_niche_dynamics_overview_new.RData")
load("results/niche_dynamics_overview_new.RData")
load("results/niche_overlap_overview_new.RData")
load("results/niche_ses_overview_new.RData")


# unique pacific species
spp_pac <- unique(subset(results_overlap, region == "pac")$species)


rel_niche_dynamics <- rel_niche_dynamics %>%
  mutate(across(!c(species,percentage, total), as.factor)) %>%
  mutate(metric = factor(metric, levels = c("abandonment", "unfilling", "stability", "expansion", "pioneering")))

results_dynamics <- results_dynamics %>%
  filter(inter_method == "inter") %>%
  mutate(across(!c(species, value), as.factor)) %>%
  mutate(metric = factor(dyn_metric, levels = c("unfilling", "stability", "expansion")))

results_ses <- results_ses %>%
  filter(sim_metric == "z.D")
  



# 1. overlap --------------------------------------------------------------

ggplot(results_overlap, aes(x = region, y = schoeners_D)) +
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
ggplot(rel_niche_dynamics, aes(x = region, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5) +
  labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = c("Pacific Islands", "Africa", "Australasia", "Europe", "North America", "South America", "temperate Asia", "tropical Asia")) +
  scale_fill_manual(name = "Niche dynamics", 
                    values = col_regular) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank(),
        plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA,),
        legend.position = "none")





# relative dynamics - Prague poster
p <- ggplot(rel_niche_dynamics, aes(x = region, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5,
               colour = "#F2F2F2") +
  labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = c("Pacific Islands", "Africa", "Australasia", "Europe", "N. America", "S. America", "temperate Asia", "tropical Asia")) +
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


ggsave("plots/results/Niche_dynamics.pdf", p, 
       bg = "transparent",
       width = 35,
       height = 17,
       units = "cm")


# original ecospat output
ggplot(results_dynamics, aes(x = region, y = value, fill = dyn_metric)) +
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

pac <- subset(master_results, region == "pac")
nrow(pac)
table(pac$similarity)

afr <- subset(master_results, region == "afr")
nrow(afr)
table(afr$similarity)

aus <- subset(master_results, region == "aus")
nrow(aus)
table(aus$similarity) 

eur <- subset(master_results, region == "eur")
nrow(eur)
table(eur$similarity) 

nam <- subset(master_results, region == "nam")
nrow(nam)
table(nam$similarity)

sam <- subset(master_results, region == "sam")
nrow(sam)
table(sam$similarity)

ate <- subset(master_results, region == "ate")
nrow(ate)
table(ate$similarity)

atr <- subset(master_results, region == "atr")
nrow(atr)
table(atr$similarity)



# 4. ses ---------------------------------------------------------------------

ses_conservatism <- subset(results_ses, sim_setting == "conservatism")


ggplot(results_ses, aes(x = region, y = value, fill = sim_setting)) +
  geom_boxplot() +
  labs(x = "Non-native region", y = "SES") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = c("Pacific Islands", "Africa", "Australasia", "Europe", "North America", "South America", "temperate Asia", "tropical Asia")) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major = element_blank(),
        legend.box.spacing = unit(3, "pt"),
        plot.margin = unit(c(0.4,0.2,0.4,0.3), "cm"))

