# Author: Anna Rönnfeldt
# Date: 2023/11/16
# Last update: 


# preamble ----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(maps)
# library(prettymapr)
library(tidyr) # for %>% gather()
# library(tidyterra)
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
p1 <- ggplot(rel_niche_dynamics_AC, aes(x = region, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5, colour = "black", , linewidth = 0.4, outlier.size = 0.5) +
  labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  scale_fill_manual(name = "Niche dynamics", 
                    labels = c("Abandonment", "Unfilling", "Stability", "Expansion", "Pioneering"),
                    values = col_regular) +
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
        axis.title = element_text(size = 11))

ggsave("plots/results/all_dynamics.png", p1, 
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


# 4. Niche conservatism ---------------------------------------------------

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


#t <- subset(stand_ESU, region == "pac")

#summary(t)
# t <- master_results_AC %>% filter(species %in% c("Agave sisalana", "Spathodea campanulata"))
# 
# t2 <- t

# plot ESU niche dynamcis

col_ESU <- c("lightgoldenrod1","lightblue2","darkseagreen3")


col_ESU_bright <- c("#FFE875","#A3DDEF","#87CF87")
# col_poster <- c("#FFE875","#A3DDEF","#87CF87")

# relative dynamics - Prague poster -> BLACK
(p <- ggplot(stand_ESU, aes(x = region, y = percentage, fill = metric)) +
    geom_boxplot(fatten = 1.5, colour = "black", , linewidth = 0.4, outlier.size = 0.5) +
    labs(x = "Non-native region", y = "Niche dynamics (%)") +
    scale_x_discrete(name = "Non-native region",
                     limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                     labels = label_regional) +
    scale_fill_manual(name = "Niche dynamics", 
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






ggsave("plots/results/ESU_dynamics.png", p, 
       width = 16,
       height = 11,
       units = "cm")
# 
# png("plots/niche_dynamics/regional_dynamics_ESU.png",width = 1450, height = 947)
# print(p)
# dev.off()
# 
# png("plots/niche_dynamics/dynamics_ESU.png",width = 1050, height = 947)
# print(p2)
# dev.off()



# map ---------------------------------------------------------------------



load("data/valen/island_lon_lat.RData")
pac <- vect("data/spatial_data/map_shapefiles/pacific_map.shp")
afr <- vect("data/spatial_data/map_shapefiles/africa_map.shp")
ate <- vect("data/spatial_data/map_shapefiles/asia_temperate_map.shp")
atr <- vect("data/spatial_data/map_shapefiles/asia_tropical_map.shp")
aus <- vect("data/spatial_data/map_shapefiles/australia_map.shp")
eur <- vect("data/spatial_data/map_shapefiles/europe_map.shp")
nam <- vect("data/spatial_data/map_shapefiles/northern_america_map.shp")
sam <- vect("data/spatial_data/map_shapefiles/southern_america_map.shp")


# save plot for the pacific centred world map
map_pacific <- map("world2", fill = TRUE, col = "lightgrey")
map_pacific_data <- map_data(map_pacific)

ggplot(island_lon_lat, aes(x = new_lon, y = lat)) +
  geom_polygon(data = map_pacific_data, 
               aes(x = long, y = lat, group = group)) +
  geom_point(size = 5, alpha = 0.8) + 
  geom_spatvector(data = sam, aes(x = lon, y = lat)) 

ggplot(island_lon_lat, aes(x = new_lon, y = lat))

windows()
maps::map("world2", fill = TRUE, col = "grey100", ylim = c(-65,85), xlim = c(10,350))
addnortharrow(pos = "bottomleft", scale = 1, padin = c(0.4, 0.2))
map.axes()
plot(pac, col = "#E090A5", add = TRUE)
plot(afr, col = "plum4", add = TRUE)
plot(ate, col = "#CFDEE7", add = TRUE)
plot(atr, col = "#BFB48F", add = TRUE)
plot(aus, col = "#2D728F", add = TRUE)
plot(eur, col = "#FED766", add = TRUE)
plot(nam, col = "#904E55", add = TRUE)
plot(sam, col = "#83BCA9", add = TRUE)
graphics::points(island_lon_lat[,c("new_lon", "lat")],  pch = 23, bg = "#E090A5", lwd = 2, cex = 3) 




# barplot similarity results  ---------------------------------------------

rm(list = ls())

perc_group <- function(x) {
  
  if(x == 0) {g <- 0}
  if(x > 0  & x <= 25) {g <- 1}
  if(x > 25 & x <= 50) {g <- 2}
  if(x > 50 & x <= 75) {g <- 3}
  if(x > 75 & x < 100) {g <- 4}
  if(x == 100) {g <- 5}
  
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
  mutate(perc_swi_group = 6)


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
  mutate(perc_group = factor(perc_group, levels = c(1,2,3,4,5,6))) %>% 
  mutate(perc_group = replace(perc_group, perc_group == 6, NA)) %>% 
  mutate(perc_group = factor(perc_group, levels = c(1,2,3,4,5)))




# legend_labels <- c("0 < X <= 25", "25 < X <= 50", "50 < X <= 75", "75 < X <= 100")

legend_labels <- c(expression("0 < x" <= "25"), expression("25 < x" <= "50"), expression("50 < x" <= "75"), expression("75 < x" < "100"), expression("x = 100"))

(p <- ggplot(df_plot, aes(observation, group = perc_group)) +
    geom_bar(aes(fill = perc_group), position = "dodge") +
    xlab(NULL) +
    ylab("Number of species") +
    scale_fill_viridis_d(option = "G", begin = 0.9, end = 0.2, alpha = 1, na.value = "transparent",
                         labels = legend_labels ) +
    scale_y_continuous(n.breaks = 8, limits = c(0,116), expand = c(0.01, 0)) +
    scale_x_discrete(labels = c("Conservatism", "n.s.", "Switching")) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = "right",
          legend.spacing.x = unit(0.1, 'cm'),
          legend.key.size = unit(0.5,"line"),
          legend.text = element_text(size = 8),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0.5,-0.5,-10,-10),
          axis.text = element_text(size = 8, color = "black"),
          panel.grid.major = element_blank()))


ggsave("plots/results/Similarity_percentage_100.png", p,
       bg = "transparent",
       width = 8,
       height = 6,
       units = "cm")

(p_E <- ggplot(df_plot, aes(observation, group = perc_group)) +
    geom_bar(aes(fill = perc_group), position = "dodge") +
    xlab(NULL) +
    ylab("Number of species") +
    scale_fill_viridis_d(option = "E", begin = 0.9, end = 0.2, alpha = 0.9, na.value = "transparent",
                         labels = legend_labels ) +
    scale_y_continuous(n.breaks = 8, limits = c(0,116), expand = c(0.01, 0)) +
    scale_x_discrete(labels = c("Conservatism", "n.s.", "Switching")) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = "right",
          legend.spacing.x = unit(0.1, 'cm'),
          legend.key.size = unit(0.5,"line"),
          legend.text = element_text(size = 8),
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(0.5,-0.5,-10,-10),
          axis.text = element_text(size = 8, color = "black"),
          panel.grid.major = element_blank()))


ggsave("plots/results/Similarity_percentage_beige_100.png", p_E,
       bg = "transparent",
       width = 8,
       height = 6,
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
