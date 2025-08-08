
#' ---------------------------
#
# Purpose of script: Compare the outcome of the niche analysis based on buffer size
# Author: Anna Rönnfeldt
# Date Created: 2025-06-18
# Email: roennfeldt@uni-potsdam.de
#
# Notes:
#
#' ---------------------------


library(dplyr)
library(ggpattern)
library(ggplot2)
library(patchwork)
library(tidyr)

path_plot <- "plots/manuscript/inkscape/"

# required data -----------------------------------------------------------

load("data/buffer_sensitivity/results/stand_ESU.RData") # 50 m
load("results/ecospat/stand_ESU.RData") # 200 m

load("data/buffer_sensitivity/results/master_results.RData")
load("results/ecospat/master_results_AC.RData")

# subset 200 dfs ----------------------------------------------------------

# subset to only contain niche pairs from 50 m analysis
stand_ESU_50 <- stand_ESU_50 %>% 
  mutate(species_region = paste(species, region, sep = "_")) %>% 
  mutate(buffer = "50")

# add species_region columns to both dfs to make filtering easier

stand_ESU_200 <- stand_ESU %>% 
  mutate(species_region = paste(species, region, sep = "_")) %>% 
  filter(species_region %in% stand_ESU_50$species_region) %>% 
  mutate(buffer = "200")



master_results_50 <- master_results %>% 
  mutate(species_region = paste(species, region, sep = "_")) %>% 
  mutate(buffer = "50")

master_results_200 <- master_results_AC %>% 
  mutate(species_region = paste(species, region, sep = "_")) %>% 
  filter(species_region %in% master_results_50$species_region) %>% 
  mutate(buffer = "200")

rm(master_results, master_results_AC)

# stand ESU -----------------------------------------------------------------


label_regional <- c("Pacific Islands\n n = 49", "Africa\n n = 34", 
                    "Australasia\n n = 37", "Europe\n n = 18", "N. America\n n = 33",
                    "S. America\n n = 33", "temp. Asia\n n = 30", "trop. Asia\n n = 26")




col_ESU <- c("#FFEC8B","#B2DFEE","#9FD39F")


# relative dynamics 
(p_50 <- ggplot(stand_ESU_50, aes(x = region, y = percentage, fill = metric)) +
    geom_boxplot(fatten = 1, colour = "black", linewidth = 0.4, outlier.size = 0.5) +
    labs(title = "50 km buffer", x = "Non-native region", y = "Niche dynamics (%)") +
    scale_x_discrete(name = "Non-native region",
                     limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                     labels = label_regional) +
    scale_fill_manual(name = NULL, 
                      labels = c("Unfilling", "Stability", "Expansion"),
                      values = col_ESU) +
    theme_bw(base_size = 20) +
    theme(panel.grid.major.x = element_blank(),
          plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
          # panel.background = element_rect(fill = "transparent"),
          # plot.background = element_rect(fill = "transparent", color = NA),
          legend.position = "none",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 11),
          legend.margin = margin(0,0,0,0),
          legend.box.margin = margin(0,-10,-10,-10),
          axis.text = element_text(size = 9, color = "black"),
          axis.title = element_text(size = 11)))
# axis.text = element_text(colour = "#1B3C59")))

(p_200 <- ggplot(stand_ESU_200, aes(x = region, y = percentage, fill = metric)) +
    geom_boxplot(fatten = 1, colour = "black", linewidth = 0.4, outlier.size = 0.5) +
    labs(title = "200 km buffer", x = "Non-native region", y = "Niche dynamics (%)") +
    scale_x_discrete(name = "Non-native region",
                     limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                     labels = label_regional) +
    scale_fill_manual(name = NULL, 
                      labels = c("Unfilling", "Stability", "Expansion"),
                      values = col_ESU) +
    theme_bw(base_size = 20) +
    theme(panel.grid.major.x = element_blank(),
          plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
          # panel.background = element_rect(fill = "transparent"),
          # plot.background = element_rect(fill = "transparent", color = NA),
          legend.position = "none",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 11),
          legend.margin = margin(0,0,0,0),
          legend.box.margin = margin(0,-10,-10,-10),
          axis.text = element_text(size = 9, color = "black"),
          axis.title = element_text(size = 11)))


(p_ESU <- p_50 + p_200)




# similarity test ---------------------------------------------------------

ID <- c(1:25)

df_con_50 <- master_results_50 %>% 
  group_by(species, similarity) %>% 
  tally() %>% 
  pivot_wider(names_from = similarity, values_from=n) %>% 
  mutate(across(everything(), .fns =~replace_na(., 0))) %>% 
  rename(conservatism_50 = conservatism,
         neither_50 = neither) %>% 
  mutate(total_50 = conservatism_50 + neither_50) %>%
  # mutate(conservatism_50 = conservatism_50 * (-1)) %>% 
  mutate(perc_con_50 = (round(100/total_50*abs(conservatism_50), 2))) %>% 
  mutate(perc_neither_50 = round(100/total_50*neither_50, 2)) 
 


df_con_200 <- master_results_200 %>% 
  group_by(species, similarity) %>% 
  tally() %>% 
  pivot_wider(names_from = similarity, values_from = n) %>% 
  mutate(across(everything(), .fns =~replace_na(., 0))) %>% 
  rename(conservatism_200 = conservatism,
         neither_200 = neither) %>% 
  mutate(total_200 = conservatism_200 + neither_200) %>%
  # mutate(conservatism_200 = conservatism_200 * (-1)) %>% 
  mutate(perc_con_200 = (round(100/total_200*abs(conservatism_200), 2))) %>% 
  mutate(perc_neither_200 = round(100/total_200*neither_200, 2)) 




df_con_both <- df_con_50 %>% 
  left_join(df_con_200, by = "species") %>% 
  mutate(dif_con = conservatism_50 - conservatism_200)


table(df_con_both$dif_con)


df_con_200 <- data.frame(species_ID = ID, df_con_200)

df_con_50 <- data.frame(species_ID = ID, df_con_50)

# species with neither = 0 and then ordered by conservatism
set_1 <- df_con_50 %>%
  filter(perc_neither == 0) %>%
  arrange(-perc_con) %>%
  mutate(set = as.factor(1))

# species with neither > 0, but mode conservatism, ordered by conservatism
set_2 <- df_con_50 %>%
  mutate(perc_con = perc_con * (-1)) %>%
  filter(perc_neither > 0 & perc_con > perc_neither) %>%
  mutate(perc_con = perc_con * (-1)) %>%
  arrange(-perc_con) %>%
  mutate(set = as.factor(2))

# species with conservatism == neither
set_3 <- df_con_50 %>%
  mutate(perc_con = perc_con * (-1)) %>%
  filter(perc_con == perc_neither) %>%
  mutate(perc_con = perc_con * (-1)) %>%
  arrange(-perc_con) %>%
  mutate(set = as.factor(3))

# species with conservatism >0, but mode neither, ordered by neither
set_4 <- df_con_50 %>%
  mutate(perc_con = perc_con * (-1)) %>%
  filter(perc_con > 0 & perc_con < perc_neither) %>%
  arrange(-perc_neither) %>%
  mutate(perc_con = perc_con * (-1)) %>%
  mutate(set = as.factor(4))

# species with conservatism = 0, ordered by neither
set_5 <- df_con_50 %>%
  mutate(perc_con = perc_con * (-1)) %>%
  filter(perc_con == 0) %>%
  arrange(-perc_neither) %>%
  mutate(perc_con = perc_con * (-1)) %>%
  mutate(set = as.factor(5))


df_plot <- rbind(set_5, set_4, set_3, set_2, set_1)

col_perc <- c("#80A8C9","#506F89","#101819","#809E51", "#C6C981")


df_plot_con <- df_plot %>%
  mutate(perc_con = perc_con * (-1))


(p_linerange_50 <- df_plot_con %>% 
    ggplot(aes(y = factor(species_ID, levels = unique(species_ID)))) +
    geom_linerange(aes(xmin = 0, xmax = perc_con, colour = set)) +
    scale_color_manual(values = col_perc) +
    geom_vline(xintercept = c(25,50,75,100), col = "darkgrey", linetype = 3) +
    geom_segment(x = 0, y = 1, xend = 100, yend = 1, color = "black") + 
    #geom_segment(x = 0, y = 317, xend = 100, yend = 317, color = "black") + 
    geom_vline(xintercept = 0, col = "black") +
    scale_x_continuous(limits = c(-5,100),
                       labels = c(0,25,50,75,100),
                       # breaks = c(-87.5,-62.5,-37.5,-12.5,12.5,37.5,62.5,87.5),
                       breaks = c(0,24,50,75,100),
                       position = "bottom") +
    ylab(NULL) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "null"))




df_con_200 <- master_results_200 %>% 
  group_by(species, similarity) %>% 
  tally() %>% 
  pivot_wider(names_from = similarity, values_from=n) %>% 
  mutate(across(everything(), .fns =~replace_na(., 0))) %>% 
  mutate(total = conservatism + neither) %>%
  mutate(conservatism = conservatism * (-1)) %>% 
  mutate(perc_con = (round(100/total*abs(conservatism), 2))*(-1)) %>% 
  mutate(perc_neither = round(100/total*neither, 2)) 

df_con_200 <- data.frame(species_ID = ID, df_con_200)

# species with neither = 0 and then ordered by conservatism
set_1 <- df_con_200 %>%
  filter(perc_neither == 0) %>% 
  arrange(-perc_con) %>% 
  mutate(set = as.factor(1))

# species with neither > 0, but mode conservatism, ordered by conservatism
set_2 <- df_con_200 %>%
  mutate(perc_con = perc_con * (-1)) %>% 
  filter(perc_neither > 0 & perc_con > perc_neither) %>% 
  mutate(perc_con = perc_con * (-1)) %>% 
  arrange(-perc_con) %>% 
  mutate(set = as.factor(2))

# species with conservatism == neither
set_3 <- df_con_200 %>%
  mutate(perc_con = perc_con * (-1)) %>% 
  filter(perc_con == perc_neither) %>% 
  mutate(perc_con = perc_con * (-1)) %>% 
  arrange(-perc_con) %>% 
  mutate(set = as.factor(3))

# species with conservatism >0, but mode neither, ordered by neither
set_4 <- df_con_200 %>%
  mutate(perc_con = perc_con * (-1)) %>% 
  filter(perc_con > 0 & perc_con < perc_neither) %>% 
  arrange(-perc_neither) %>% 
  mutate(perc_con = perc_con * (-1)) %>% 
  mutate(set = as.factor(4))

# species with conservatism = 0, ordered by neither
set_5 <- df_con_200 %>%
  mutate(perc_con = perc_con * (-1)) %>% 
  filter(perc_con == 0) %>% 
  arrange(-perc_neither) %>% 
  mutate(perc_con = perc_con * (-1)) %>% 
  mutate(set = as.factor(5))


df_plot <- rbind(set_5, set_4, set_3, set_2, set_1)

col_perc <- c("#80A8C9","#506F89","#101819","#809E51", "#C6C981")


df_plot_con <- df_plot %>%
  mutate(perc_con = perc_con * (-1))

(p_linerange_200 <- df_plot_con %>% 
    ggplot(aes(y = factor(species_ID, levels = unique(species_ID)))) +
    geom_linerange(aes(xmin = 0, xmax = perc_con, colour = set)) +
    scale_color_manual(values = col_perc) +
    geom_vline(xintercept = c(25,50,75,100), col = "darkgrey", linetype = 3) +
    geom_segment(x = 0, y = 1, xend = 100, yend = 1, color = "black") + 
    #geom_segment(x = 0, y = 317, xend = 100, yend = 317, color = "black") + 
    geom_vline(xintercept = 0, col = "black") +
    scale_x_continuous(limits = c(-5,100),
                       labels = c(0,25,50,75,100),
                       # breaks = c(-87.5,-62.5,-37.5,-12.5,12.5,37.5,62.5,87.5),
                       breaks = c(0,24,50,75,100),
                       position = "bottom") +
    ylab(NULL) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "null"))

p_linerange <- p_linerange_50 + p_linerange_200



# niche conservatism per region -------------------------------------------

df_reg_50 <- master_results_50 %>% 
  group_by(region, similarity) %>% 
  tally() %>% 
  pivot_wider(names_from=similarity, values_from = n) %>% 
  mutate(total = conservatism + neither) %>% 
  mutate(perc_con = round(100/total*abs(conservatism), 2)) %>% 
  mutate(region = factor(region, levels = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"))) %>% 
  mutate(buffer = "50 km")

df_reg_200 <- master_results_200 %>% 
  group_by(region, similarity) %>% 
  tally() %>% 
  pivot_wider(names_from=similarity, values_from = n) %>% 
  mutate(total = conservatism + neither) %>% 
  mutate(perc_con = round(100/total*abs(conservatism), 2)) %>% 
  mutate(region = factor(region, levels = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"))) %>% 
  mutate(buffer = "200 km")


df_reg_both <- rbind(df_reg_50, df_reg_200)



label_regional <- c("Pacific Islands\n n = 49", "Africa\n n = 34", 
                    "Australasia\n n = 37", "Europe\n n = 18", "N. America\n n = 33",
                    "S. America\n n = 33", "temp. Asia\n n = 30", "trop. Asia\n n = 26")





p_barplot <- df_reg_both %>% 
  ggplot() +
  geom_bar_pattern(aes(x = region, y = perc_con, fill = buffer, pattern = buffer),
           position = position_dodge(preserve = "single"),
           stat = "identity",
           color = "black", 
           pattern_fill = "grey33",
           pattern_angle = 50,
           pattern_density = 0.1,
           pattern_spacing = 0.02,
           pattern_key_scale_factor = 0.6)  +
  scale_pattern_manual(values = c("200 km" = "stripe", "50 km" = "none")) +
  scale_fill_manual("Buffer", values=c('grey50','lightgrey')) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  scale_y_continuous(name = "Non-naitve species\n with niche conservatism (%)", 
                     limits = c(0,100), expand = c(0.005, 0)) +
  scale_x_discrete(name = "Non-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  # theme() +
  theme_bw()
  
ggsave(paste0(path_plot,"buffer_niche_conservatism.svg"), p_barplot, 
       width = 17,
       height = 9,
       units = "cm")



# individual niche metrics ------------------------------------------------

df_ESU <- rbind(stand_ESU_200, stand_ESU_50)


col_ESU <- c("#FFEC8B","#B2DFEE","#9FD39F")

# version with patterns 




p_u <- ggplot(df_ESU[df_ESU$metric == "unfilling",], aes(x = region, y = percentage, fill = buffer, pattern = buffer)) +
  geom_boxplot(fatten = 1, colour = "black", linewidth = 0.4, outlier.size = 0.5) +
  scale_fill_manual(values = c("#FFE253", "#FFF5C5")) +
  geom_boxplot_pattern(position = position_dodge(preserve = "single"),
                       color = "black", 
                       pattern_fill = "grey33",
                       pattern_angle = 50,
                       pattern_density = 0.1,
                       pattern_spacing = 0.02,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c("200" = "stripe", "50" = "none")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  labs(x = "Non-native region", y = "Niche dynamics (%)", pattern = "Buffer (km)") +
  scale_x_discrete(name = "Non-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  scale_y_continuous(limits = c(0,100)) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major.x = element_blank(),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
        # panel.background = element_rect(fill = "transparent"),
        # plot.background = element_rect(fill = "transparent", color = NA),
        #legend.position = "none",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,-10,-10,-10),
        axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 11)) 


p_e <- ggplot(df_ESU[df_ESU$metric == "expansion",], aes(x = region, y = percentage, fill = buffer, pattern = buffer)) +
  geom_boxplot(fatten = 1, colour = "black", linewidth = 0.4, outlier.size = 0.5) +
  scale_fill_manual(values = c("#59B359", "#BAE0BA")) +
  geom_boxplot_pattern(position = position_dodge(preserve = "single"),
                       color = "black", 
                       pattern_fill = "grey33",
                       pattern_angle = 50,
                       pattern_density = 0.1,
                       pattern_spacing = 0.02,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c("200" = "stripe", "50" = "none")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  labs(x = "Non-native region", y = "Niche dynamics (%)", pattern = "Buffer (km)") +
  scale_x_discrete(name = "Non-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  scale_y_continuous(limits = c(0,100)) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major.x = element_blank(),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
        # panel.background = element_rect(fill = "transparent"),
        # plot.background = element_rect(fill = "transparent", color = NA),
        #legend.position = "none",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,-10,-10,-10),
        axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 11)) 


p_s <- ggplot(df_ESU[df_ESU$metric == "stability",], aes(x = region, y = percentage, fill = buffer, pattern = buffer)) +
  geom_boxplot(fatten = 1, colour = "black", linewidth = 0.4, outlier.size = 0.5) +
  scale_fill_manual(values = c("#72C3E0", "#E6F5FA")) +
  geom_boxplot_pattern(position = position_dodge(preserve = "single"),
                       color = "black", 
                       pattern_fill = "grey33",
                       pattern_angle = 50,
                       pattern_density = 0.1,
                       pattern_spacing = 0.02,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c("200" = "stripe", "50" = "none")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none"))) +
  labs(x = "Non-native region", y = "Niche dynamics (%)", pattern = "Buffer (km)") +
  scale_x_discrete(name = "Non-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  scale_y_continuous(limits = c(0,100)) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major.x = element_blank(),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
        # panel.background = element_rect(fill = "transparent"),
        # plot.background = element_rect(fill = "transparent", color = NA),
        #legend.position = "none",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,-10,-10,-10),
        axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 11)) 


ggplot(df_ESU[df_ESU$metric == "stability",], aes(x = region, y = percentage, fill = buffer)) +
  geom_boxplot(fatten = 1, colour = "black", linewidth = 0.4, outlier.size = 0.5) +
  labs(x = "Non-native region", y = "Niche dynamics (%)") +
  scale_x_discrete(name = "Non-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  scale_fill_manual(name = "Buffer", 
                    labels = c("200 km", "50 km"),
                    values = c("#72C3E0", "#E6F5FA")) +
  scale_y_continuous(limits = c(0,100)) +
  theme_bw(base_size = 20) +
  theme(panel.grid.major.x = element_blank(),
        plot.margin = unit(c(0.2,0.1,0.2,0.2), "cm"),
        # panel.background = element_rect(fill = "transparent"),
        # plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = "none",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,-10,-10,-10),
        axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 11))

ggsave(paste0(path_plot,"buffer_sensitivity_unf.svg"), p_u, 
       width = 17,
       height = 9,
       units = "cm")

ggsave(paste0(path_plot,"buffer_sensitivity_exp.svg"), p_e, 
       width = 17,
       height = 9,
       units = "cm")

ggsave(paste0(path_plot,"buffer_sensitivity_stab.svg"), p_s, 
       width = 17,
       height = 9,
       units = "cm")


# stats -------------------------------------------------------------------

combined_results <- rbind(stand_ESU_200, stand_ESU_50) %>% 
  pivot_wider(names_from = "metric",
              values_from = "percentage")


# pacific
reg <- "atr"
df <- subset(combined_results, region == reg)

kruskal.test(df$stability ~ df$buffer) 
kruskal.test(df$unfilling ~ df$buffer) 
kruskal.test(df$expansion ~ df$buffer) 
