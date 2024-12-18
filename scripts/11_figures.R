#' ---------------------------
#
# Purpose of script: creating figures to visualise the results of the niche comparison
# Author: Anna Rönnfeldt
# Date Created: 2023-10-25
# Email: roennfeldt@uni-potsdam.de
#
# Notes: the script contains code for multiple different figure versions (e.g., for the publication or for printed posters)
#
#' ---------------------------



library(dplyr)
library(dotwhisker) # for line range plot
library(fmsb) # for radar plot
library(ggplot2)
library(maps)
library(networkD3) # for sankey plot
library(tidyr) # for %>% gather()
library(terra)
library(viridis)


# load data ---------------------------------------------------------------

load("results/ecospat/master_results_AC.RData")
load("results/ecospat/rel_niche_dynamics_results_AC.RData")
load("results/ecospat/niche_dynamics_results_AC.RData")
load("results/ecospat/niche_overlap_results_AC.RData")
# load("results/ecospat/niche_ses_results_AC.RData")
# load("results/ecospat/percentages_niche_conservatism.RData")
load("results/ecospat/stand_ESU.RData")
load("data/input_TA_unstand.RData")
load("data/climate_zones/spp_zones_all.RData")


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


label_regional <- c("Pacific Islands\n n = 317", "Africa\n n = 214", "Australasia\n n = 219", "Europe\n n = 90", "N. America\n n = 204",  "S. America\n n = 219", "temp. Asia\n n = 179", "trop. Asia\n n = 151")



# Niche dynamics ------------------------------------------------------


# relative dynamics - per region
(p1 <- ggplot(rel_niche_dynamics_AC, aes(x = region, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5, colour = "black", , linewidth = 0.4, outlier.size = 0.5) +
  labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
  scale_x_discrete(name = "\nNon-native region",
                   limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                   labels = label_regional) +
  scale_fill_manual(name = NULL, 
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
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,-10,-10,-10),
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

# ggsave("plots/results/Niche_conservatism_bar.png", p, 
#        bg = "transparent",
#        width = 13,
#        height = 18,
#        units = "cm")


# stand. ESU --------------------------------------------------------------

# plot ESU niche dynamics

col_ESU <- c("#FFEC8B","#B2DFEE","#9FD39F")

# col_poster <- c("#FFE875","#A3DDEF","#87CF87")

# relative dynamics 
(p <- ggplot(stand_ESU, aes(x = region, y = percentage, fill = metric)) +
    geom_boxplot(fatten = 1.5, colour = "black", , linewidth = 0.4, outlier.size = 0.5) +
    labs(x = "Non-native region", y = "Niche dynamics (%)") +
    scale_x_discrete(name = "Non-native region",
                     limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                     labels = label_regional) +
    scale_fill_manual(name = NULL, 
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



# barplot similarity results  ---------------------------------------------

perc_group <- function(x) {
  
  if (x == 0) {g <- 0}
  if (x > 0  & x <= 25) {g <- 1}
  if (x > 25 & x <= 50) {g <- 2}
  if (x > 50 & x <= 75) {g <- 3}
  if (x > 75 & x <= 100) {g <- 4}
  # if(x == 100) {g <- 5}
  
  return(g)
}


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


legend_labels <- c(expression("" <= "25%"), expression("" <= "50%"), expression("" <= "75%"), expression("" <= "100%"))
col_gradient <- c("#C1C9D1", "#8B9BA9", "#5E7080", "#3E4A54") # alternative: c("#E0C9AA", "#D4B387", "#725546", "#594236")

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







# original:
# c("#6F5449", "#EAB464", "#919AA1")




# linerange plots ---------------------------------------------------------


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



## panel B: conservatism per region -----------------------------------

# create a barplot that shows the percentage of species that conserved 
# their niche in each region

# create df
df_reg <- master_results_AC %>% 
  group_by(region, similarity) %>% 
  tally() %>% 
  pivot_wider(names_from=similarity, values_from = n) %>% 
  mutate(total = conservatism + neither) %>% 
  mutate(perc_con = round(100/total*abs(conservatism), 2)) %>% 
  mutate(region = factor(region, levels = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"))) 

# label_regional <- c("Pacific Islands\n n = 317", "Africa\n n = 214", 
#                     "Australasia\n n = 219", "Europe\n n = 90", 
#                     "N. America\n n = 204",  "S. America\n n = 219",
#                     "temp. Asia\n n = 179", "trop. Asia\n n = 151")
# 
# label_regional <- c("Pac. Islands\n n = 317", "Africa\n n = 214", 
#                     "Austral.\n n = 219", "Europe\n n = 90", 
#                     "N. America\n n = 204",  "S. America\n n = 219",
#                     "temp. Asia\n n = 179", "trop. Asia\n n = 151")

label_regional <- c("Pac\n n=317", "Afr\n n=214", 
                    "Aus\n n=219", "Eur\n n=90", 
                    "Nam\n n=204", "Sam\n n=219",
                    "Ate\n n=179", "Atr\n n=151")

(p_barplot <- df_reg %>% 
    ggplot(aes(x = region, y = perc_con)) +
    geom_col(colour = "grey", fill = "grey", width = 0.8) +
    scale_y_continuous(name = "Non-naitve species\n with niche conservatism (%)", 
                       limits = c(0,100), expand = c(0.005, 0)) +
    scale_x_discrete(name = "Non-native region",
                     limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                     labels = label_regional) +
    theme_classic() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_text(size = 7, colour = "grey22"),
          axis.text = element_text(size = 7, colour = "grey22")))

ggsave("plots/manuscript/inkscape/regional_barplot.svg", p_barplot,
       bg = "transparent",
       device = "svg",
       width = 9.5,
       height = 4.2,
       units = "cm")

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


df_plot_con <- df_plot %>%
  mutate(perc_con = perc_con * (-1))


(p_linerange <- df_plot_con %>% 
    ggplot(aes(y = factor(species_ID, levels = unique(species_ID)))) +
    geom_linerange(aes(xmin = 0, xmax = perc_con, colour = set)) +
    scale_color_manual(values = col_perc) +
    # geom_vline(xintercept = c(25,50,75,100), col = "darkgrey", linetype = 3) +
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


ggsave("plots/manuscript/inkscape/linerange_empty.svg", p_linerange,
       device = "svg",
       # bg = "white",
       width = 16,
       height = 14,
       units = "cm")



# Koeppen climate related plots -------------------------------------------

## mean trait values -------------------------------------------------------

# determine mean trait values for species native to certain climate regions 
# these will then be plotted in a radar plot

# growth form
input_TA[input_TA == "herb"] <- 1
input_TA[input_TA == "shrub"] <- 2
input_TA[input_TA == "tree"] <- 3

# life-cycle
input_TA[input_TA == "annual"] <- 1
input_TA[input_TA == "biennial"] <- 2
input_TA[input_TA == "perennial"] <- 3

# shift centroid values so that they start at 0
min_centroid_a <- min(input_TA$niche_centroid_a_nat)
min_centroid_b <- min(input_TA$niche_centroid_b_nat)

input_TA <- input_TA %>% 
  mutate(niche_centroid_a_nat = niche_centroid_a_nat + abs(min(niche_centroid_a_nat))) %>% 
  mutate(niche_centroid_b_nat = niche_centroid_b_nat + abs(min(niche_centroid_b_nat)))

species_A <- spp_zones_all %>% 
  filter(freq_climate == "A") %>% 
  pull(species)

species_AC <- spp_zones_all %>% 
  filter(freq_climate == "AC") %>% 
  pull(species)

species_C <- spp_zones_all %>% 
  filter(freq_climate == "C") %>% 
  pull(species)

species_all <- spp_zones_all$species


traits_A <- input_TA %>% 
  filter(species %in% species_A) %>% 
  select(mean_height, mean_seedmass, growth_form, lifecycle, range_size_nat, niche_breadth_nat, niche_centroid_a_nat, niche_centroid_b_nat, years_since_intro) %>% 
  mutate_if(is.character, as.numeric) 

traits_AC <- input_TA %>% 
  filter(species %in% species_AC) %>% 
  select(mean_height, mean_seedmass, growth_form, lifecycle, range_size_nat, niche_breadth_nat, niche_centroid_a_nat, niche_centroid_b_nat, years_since_intro) %>% 
  mutate_if(is.character, as.numeric) 

traits_C <- input_TA %>% 
  filter(species %in% species_C) %>% 
  select(mean_height, mean_seedmass, growth_form, lifecycle, range_size_nat, niche_breadth_nat, niche_centroid_a_nat, niche_centroid_b_nat, years_since_intro) %>% 
  mutate_if(is.character, as.numeric) 


mean_A <- apply(traits_A, 2, mean)
mean_AC <- apply(traits_AC, 2, mean)
mean_C <- apply(traits_C, 2, mean)

mean_values <- as.data.frame(rbind(mean_A, mean_AC, mean_C), row.names = FALSE) 
row.names(mean_values) <- c("zone A", "zone AC", " zone C")
names(mean_values) <- c("mean height", "mean seedmass", "growth form", "life cycle\nshort    long", 
                        "range size", "niche breadth", "niche centroid\nwarm    cold", "niche centroid\n wet    dry", "years since introduction")

max_values <- apply(mean_values, 2, max)


radar_input <- rbind(max_values,
                     rep(0,9),
                     mean_values)


## radar plot --------------------------------------------------------------

zones_TA <- spp_zones_all %>% 
  filter(species %in% unique(input_TA$species))

table(zones_TA$freq_climate)



# pnt_col = c("#7180AC", "#BE8A60", "#8E3E48")
# pnt_col = c("#317787", "#ED9A6E", "#804D57")

# same col scale as in the sankey diagram:
# pnt_col = c("#69b3a2", "steelblue","darkblue")
pnt_col = c("darkblue", "steelblue","#69b3a2") # doesn't show up in correct order

# windows()
#radarchart(radar_input)
# par(mar = c(1.5, 1.5, 1.5, 1.5))
# par(mar = c(0.2, 0.2, 0.2, 0.2))
par(mar = c(0, 0, 0, 0))


# png(file = "plots/manuscript/radar.png", width = 680, height = 680, units = "px", pointsize = 12)
# radarchartcirc(radar_input, axistype = 0, seg = 1,
#                pty = 19,
#                #custom polygon
#                pcol = pnt_col, plwd = 2, plty = 1,
#                #custom the grid
#                cglcol = "darkgrey", cglty = 1, cglwd = 0.8,
#                #custom labels
#                vlcex = 0.5 )
# dev.off()

svg(file = "plots/manuscript/inkscape/radar.svg", width = 18, height = 18)
radarchartcirc(radar_input, axistype = 0, seg = 1,
               pty = 19,
               #custom polygon
               pcol = pnt_col, plwd = 2, plty = 1,
               #custom the grid
               cglcol = "darkgrey", cglty = 1, cglwd = 0.8,
               #custom labels
               vlcex = 0.5 )
dev.off()



# sankey all regions ------------------------------------------------------

species_flow <- master_results_AC %>% 
  select(species, region) %>% 
  left_join(select(spp_zones_all, species, freq_climate), by = "species") 

flow_count <- dplyr::count(species_flow, region, freq_climate)


flow_count$region <- as.character(flow_count$region )
flow_count[flow_count == "pac"] <- "Pacific Islands"
flow_count[flow_count == "nam"] <- "North America"
flow_count[flow_count == "sam"] <- "South America"
flow_count[flow_count == "afr"] <- "Africa"
flow_count[flow_count == "aus"] <- "Australasia"
flow_count[flow_count == "ate"] <- "temp. Asia"
flow_count[flow_count == "atr"] <- "trop. Asia"
flow_count[flow_count == "eur"] <- "Europe"

flow_count <- flow_count %>% 
  mutate(region = factor(region, levels = c("Pacific Islands", "Africa", "trop. Asia", "temp. Asia", "Australasia", "North America", "South America", "Europe")))

nodes <- data.frame(name=c(as.character(flow_count$freq_climate), as.character(flow_count$region)) %>% unique())
nodes$group <- as.factor(c("a", "ab","abc", "ac", "b","bc", "c", "cd", "ce", rep("target_group", 8)))
nodes$names_null <- c("","","","","","","","","","","","","","","","","") #add empty cell values so that no labels are plotted
flow_count$IDsource=match(flow_count$freq_climate, nodes$name)-1
flow_count$IDtarget=match(flow_count$region, nodes$name)-1

# define colour palette so that boxes on the left have the same colour as the corresponding links and that all boxes on the right have the same grey colour
# first indicate the link groups (here upper case labels), then the node groups (lower case),
# then provide the colours for the label boxes and links, and lastly, the colour for the boxes on the right
# note: the order in which the colours will be plotted is not intuitive. Play around until desired outcome is reached
col_all <- 'd3.scaleOrdinal() .domain(["A", "AB","ABC", "AC", "B","BC", "C", "CD", "CE", "a", "ab","abc", "ac", "b","bc", "c", "cd", "ce","target_group"])
.range(["#121288", "#D6C594","#74AE62", "#588EBB", "#BE7A34", "#CEC44B", "#78BAAB", "#D85B66", "#902D59",
"#121288", "#D6C594","#74AE62", "#588EBB", "#BE7A34", "#CEC44B", "#78BAAB", "#D85B66", "#902D59",
"lightgrey"])'


# # with labels
# sankeyNetwork(Links = flow_count, Nodes = nodes,
#               Source = "IDsource", Target = "IDtarget",
#               Value = "n", NodeID = "name",
#               nodeWidth = 40, fontSize = 13, nodePadding = 5,
#               sinksRight = FALSE, LinkGroup = "freq_climate",
#               colourScale = col_all, NodeGroup = "group",
#               fontFamily = "Arial", margin = 0)



# no labels
sankeyNetwork(Links = flow_count, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "n", NodeID = "names_null",
              nodeWidth = 30, fontSize = 13, nodePadding = 12,
              sinksRight = FALSE, LinkGroup = "freq_climate",
              colourScale = col_all, NodeGroup = "group",
              fontFamily = "Arial", margin = 0)







# other figures: ----------------------------------------------------------

# results based on species native to the three most common main climates:

df_clim <- master_results_AC %>% 
  left_join(select(spp_zones_all, species, freq_climate), by = "species") %>% 
  filter(freq_climate %in% c("A", "AC", "C")) %>% 
  group_by(freq_climate, similarity) %>% 
  tally() %>% 
  pivot_wider(names_from=similarity, values_from = n) %>% 
  mutate(total = conservatism + neither) %>% 
  mutate(perc_con = round(100/total*abs(conservatism), 2))

df_clim %>% 
  ggplot(aes(x = freq_climate, y = perc_con)) +
  # geom_col(colour = "grey", fill = "grey", width = 0.8) +
  geom_col(colour = "grey", fill = c("darkblue", "steelblue","#69b3a2"), width = 0.8) +
  xlab("Main climate classification of native range") +
  ylab("Percentage of species\n with niche conservatism") +
  scale_y_continuous(limits = c(0,100), expand = c(0.005, 0)) +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


load("results/ecospat/rel_niche_dynamics_results_AC.RData")

df_clim_rel  <- rel_niche_dynamics_AC %>%
  mutate(across(!c(species,percentage, total), as.factor)) %>%
  mutate(metric = factor(metric, levels = c("abandonment", "unfilling", "stability", "expansion", "pioneering"))) %>% 
  mutate(percentage = percentage *100) %>% 
  left_join(select(spp_zones_all, species, freq_climate), by = "species") %>% 
  filter(freq_climate %in% c("A", "AC", "C"))

col_regular <- c("lightsalmon3","lightgoldenrod1","lightblue2","darkseagreen3",  "thistle4")

ggplot(df_clim_rel, aes(x = freq_climate, y = percentage, fill = metric)) +
  geom_boxplot(fatten = 1.5, colour = "black", , linewidth = 0.4, outlier.size = 0.5) +
  labs(x = "Main climate classification of native range", y = "Niche dynamics (%)\n") +
  scale_fill_manual(name = NULL, 
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
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,-10,-10,-10),
        legend.key.size = unit(0.4, "cm"),
        # legend.key.spacing = unit(0.25, "cm"),
        axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 11))


# relative dynamics - per region
(p1 <- ggplot(t2, aes(x = freq_climate, y = percentage, fill = metric)) +
    geom_boxplot(fatten = 1.5, colour = "black", , linewidth = 0.4, outlier.size = 0.5) +
    labs(x = "Non-native region", y = "Niche dynamics (%)\n") +
    scale_x_discrete(name = "\nNon-native region",
                     limits = c("pac", "afr", "aus", "eur", "nam", "sam", "ate", "atr"),
                     labels = label_regional) +
    scale_fill_manual(name = NULL, 
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
          legend.margin = margin(0,0,0,0),
          legend.box.margin = margin(0,-10,-10,-10),
          legend.key.size = unit(0.4, "cm"),
          # legend.key.spacing = unit(0.25, "cm"),
          axis.text = element_text(size = 9, color = "black"),
          axis.title = element_text(size = 11)))

