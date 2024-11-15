#' ---------------------------
#
# Purpose of script: Identify to which Köppen-Geiger climate zones each 
# occurrence point belogs to
# Author: Anna Rönnfeldt
# Date Created: 2024-11-04
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "doParallel", "foreach", "kgc" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)


# library(dplyr)
# library(fmsb)
# library(kgc)
# library(tidyr)
# library(networkD3)

# cluster version ---------------------------------------------------------

path_occ_data <- "/import/ecoc9z/data-zurell/roennfeldt/C1/regional_occs/criterion_1/native/"

load("/import/ecoc9z/data-zurell/roennfeldt/C1/input/spp_suitable_AC.RData")


# df_occ_zones <- as.data.frame(matrix(nrow = 0, ncol = 5))
# names(df_occ_zones) <- c("species", "occ_id", "lon", "lat", "climate_zone")

no_cores <- 20
cl <- makeCluster(no_cores)
registerDoParallel(cl)


df_occ_zones <- foreach(spp_index = 1:length(spp_suitable_AC), .packages = c("dplyr", "kgc"), .combine = "rbind", .verbose = TRUE) %dopar% # can be easily modded to be done on the cluster
  try({
    print(spp_suitable_AC[spp_index])
    
    # load native occurrences
    load(paste0(path_occ_data,"nat_occs",spp_suitable_AC[spp_index],".RData")) # object name: nat
    
    spp_occs <- nat %>%
      select(occ_id, lon, lat) %>%
      mutate(rndCoord.lon = RoundCoordinates(lon)) %>%
      mutate(rndCoord.lat = RoundCoordinates(lat))
    
    spp_occ_zones <- data.frame(spp_occs, ClimateZ = LookupCZ(spp_occs)) %>%
      select(occ_id, lon, lat, ClimateZ) %>%
      # dplyr::rename(climate_zone = ClimateZ) %>%
      mutate(species = spp_suitable_AC[spp_index], .before = occ_id)
    
    names(spp_occ_zones)[names(spp_occ_zones) == 'ClimateZ'] <- 'climate_zone'
    
    # df_occ_zones <- rbind(df_occ_zones, spp_occ_zones)
    
    spp_occ_zones
    
  }) # end of foreach loop over species

save(df_occ_zones, file = "/import/ecoc9z/data-zurell/roennfeldt/C1/output/df_occ_climate_zones.RData")
stopCluster(cl)

rm(list = ls())
gc()



# local -------------------------------------------------------------------

library(dplyr)
library(fmsb)
library(tidyr)
library(networkD3)

load("data/df_occ_climate_zones.RData")
load("data/spp_suitable_AC.RData")

df_zones <- df_occ_zones %>% 
  select(!.before) %>% 
  select(species, occ_id, lon, lat, climate_zone)

df_zones[df_zones$climate_zone == "Climate Zone info missing","climate_zone"] <- NA

a_index <- which(grepl(pattern = "^A", df_zones$climate_zone))
b_index <- which(grepl(pattern = "^B", df_zones$climate_zone))
c_index <- which(grepl(pattern = "^C", df_zones$climate_zone))
d_index <- which(grepl(pattern = "^D", df_zones$climate_zone))
e_index <- which(grepl(pattern = "^E", df_zones$climate_zone))

df_zones[a_index, "main_climate"] <- "A"
df_zones[b_index, "main_climate"] <- "B"
df_zones[c_index, "main_climate"] <- "C"
df_zones[d_index, "main_climate"] <- "D"
df_zones[e_index, "main_climate"] <- "E"

#define custom function to add columns to data frame if they do not exist
add_cols <- function(df, cols) {
  add <- cols[!cols %in% names(df)]
  if (length(add) !=0) df[add] <- NA
  return(df)
}

zones <- c("A", "B", "C", "D", "E")

spp_zones_all <- as.data.frame(matrix(ncol = 7, nrow = 0))
names(spp_zones_all) <- c("species", "A", "B", "C", "D", "E", "main_climate")

for (spp in spp_suitable_AC) {
  
  print(spp)
  
  spp_zones <- as.data.frame(table(subset(df_zones, species == spp)$main_climate)) %>% 
    pivot_wider(names_from = Var1, values_from = Freq) %>%
    mutate(species = spp) %>% 
    add_cols(c("A", "B", "C", "D", "E")) %>% 
    select(species, A, B, C, D, E) %>% 
    mutate(total_occs = sum(A,B,C,D,E, na.rm = TRUE)) %>% 
    mutate(A = round(100/total_occs*A)) %>% 
    mutate(B = round(100/total_occs*B)) %>% 
    mutate(C = round(100/total_occs*C)) %>% 
    mutate(D = round(100/total_occs*D)) %>% 
    mutate(E = round(100/total_occs*E)) %>% 
    replace(is.na(.), 0) 
  
  spp_zones_all <- rbind(spp_zones_all, spp_zones)
  }
  
spp_zones_all$freq_climate <- NA

for (spp in spp_suitable_AC) { 
  
  print(spp)
  
  spp_label <- NULL
  
  for (zone in zones) {
    print(zone)
    if (spp_zones_all[spp_zones_all$species == spp, zone] >= 30) {
      spp_label <- c(spp_label,zone)
    } # end of if condition
  } # end of for loop over zones
  
  spp_label <- paste(spp_label, collapse = "")
  
  spp_zones_all[spp_zones_all$species == spp,]$freq_climate <- spp_label
} # end of for loop over species


table(spp_zones_all$freq_climate)

save(spp_zones_all, file = "data/climate_zones/spp_zones_all.RData")

# figures -----------------------------------------------------------------

# create_radar_input <- function(df){
#   
#   max_values <- apply(df, 2, min)
#   min_values <- apply(df, 2, max)
#   mean_values <- apply(df, 2, mean)
#   
#   df_radar <- rbind(max_values, min_values, mean_values)
#   rownames(df_radar) <- NULL
#   
#   return(as.data.frame(df_radar))
# } # end of function definition

# mean trait values -------------------------------------------------------
load("data/input_TA_unstand.RData")
load("data/climate_zones/spp_zones_all.RData")

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


traits_A <- input_TA %>% 
  filter(species %in% species_A) %>% 
  select(mean_height, mean_seedmass, growth_form, lifecycle, range_size_nat, niche_breadth_nat, niche_centroid_a_nat, niche_centroid_b_nat) %>% 
  mutate_if(is.character, as.numeric) 

traits_AC <- input_TA %>% 
  filter(species %in% species_AC) %>% 
  select(mean_height, mean_seedmass, growth_form, lifecycle, range_size_nat, niche_breadth_nat, niche_centroid_a_nat, niche_centroid_b_nat) %>% 
  mutate_if(is.character, as.numeric) 

traits_C <- input_TA %>% 
  filter(species %in% species_C) %>% 
  select(mean_height, mean_seedmass, growth_form, lifecycle, range_size_nat, niche_breadth_nat, niche_centroid_a_nat, niche_centroid_b_nat) %>% 
  mutate_if(is.character, as.numeric) 


mean_A <- apply(traits_A, 2, mean)
mean_AC <- apply(traits_AC, 2, mean)
mean_C <- apply(traits_C, 2, mean)

mean_values <- as.data.frame(rbind(mean_A, mean_AC, mean_C), row.names = FALSE) 
row.names(mean_values) <- c("zone A", "zone AC", " zone C")
names(mean_values) <- c("mean height", "mean seedmass", "growth form", "life cycle\nshort    long", 
                        "range size", "niche breadth", "niche centroid\nwarm    cold", "niche centroid\n wet    dry")

max_values <- apply(mean_values, 2, max)


radar_input <- rbind(max_values,
                     rep(0,8),
                     mean_values)


## radar plot --------------------------------------------------------------



pnt_col = c("#7180AC", "#BE8A60", "#8E3E48")
pnt_col = c("#317787", "#ED9A6E", "#804D57")


windows()
#radarchart(radar_input)
par(mar = c(1.5, 1.5, 1.5, 1.5))
radarchartcirc(radar_input, axistype = 0, seg = 1,
           pty = 19,
           #custom polygon
           pcol = pnt_col, plwd = 2, plty = 1,
           #custom the grid
           cglcol = "darkgrey", cglty = 1, cglwd = 0.8,
           #custom labels
           vlcex = 0.8 )


legend(x = -1.5, y = -1, horiz = TRUE,legend = rownames(radar_input[-c(1,2),]),
       bty = "n", # no box drawn
       pch = 19, col = pnt_col,
       cex = 0.8)

# add a legend
# legend(x=1.5, y=1, legend = rownames(radar_input[-c(1,2),]), 
#        bty = "n", pch=20 , col=pnt_col , text.col = "grey", 
#        cex=1.2, pt.cex=2)
set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(20,5) , rep(0,5) , data)

# plot with default options:
radarchart(data)


# sankey diagram ----------------------------------------------------------

load("results/ecospat/master_results_AC.RData")
load("data/climate_zones/spp_zones_all.RData")

species_flow <- master_results_AC %>% 
  select(species, region) %>% 
  left_join(select(spp_zones_all, species, freq_climate), by = "species") %>% 
  filter(freq_climate %in% c("A", "AC", "C")) %>% 
  count(vars = c("region", "freq_climate")) %>%  
  mutate(group = as.factor(freq_climate)) 


species_flow$region <- as.character(species_flow$region )
species_flow[species_flow == "pac"] <- "Pacific Islands"
species_flow[species_flow == "nam"] <- "North America"
species_flow[species_flow == "sam"] <- "South America"
species_flow[species_flow == "afr"] <- "Arica"
species_flow[species_flow == "aus"] <- "Australasia"
species_flow[species_flow == "ate"] <- "temp. Asia"
species_flow[species_flow == "atr"] <- "trop. Asia"
species_flow[species_flow == "eur"] <- "Europe"

species_flow <- species_flow %>% 
  mutate(metric = factor(region, levels = c("Pacific Islands", "Africa", "trop. Asia", "temp. Asia", "Australasia", "North America", "South America", "Europe")))

nodes <- data.frame(name=c(as.character(species_flow$freq_climate), as.character(species_flow$region)) %>% unique())
nodes$group <- as.factor(c("a", "ac", "c", rep("target_group", 8)))
species_flow$IDsource=match(species_flow$freq_climate, nodes$name)-1
species_flow$IDtarget=match(species_flow$region, nodes$name)-1

# col_sankey <- 'd3.scaleOrdinal() .domain(["A", "AC", "C", "a", "ac", "c","target_group"]) .range(["#69b3a2", "steelblue","darkblue","#69b3a2", "steelblue","darkblue", "grey"])'
col_sankey <- 'd3.scaleOrdinal() .domain(["A", "AC", "C", "a", "ac", "c","target_group"]) .range(["#C4594B", "#FEB248","#69b3a2","#C4594B", "#FEB248","#69b3a2", "grey"])'


sankeyNetwork(Links = species_flow, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "freq", NodeID = "name",
              nodeWidth = 40, fontSize = 13, nodePadding = 5,
              sinksRight = FALSE, LinkGroup = "group",
              colourScale = col_sankey, NodeGroup = "group")



# chord diagram -----------------------------------------------------------
# 
# library(tidyverse)
# library(viridis)
# library(patchwork)
# library(hrbrthemes)
# library(circlize)
# library(chorddiag)  #devtools::install_github("mattflor/chorddiag")
# # Load dataset from github
# data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)
# 
# # short names
# colnames(data) <- c("Africa", "East Asia", "Europe", "Latin Ame.",   "North Ame.",   "Oceania", "South Asia", "South East Asia", "Soviet Union", "West.Asia")
# rownames(data) <- colnames(data)
# 
# # I need a long format
# data_long <- data %>%
#   rownames_to_column %>%
#   gather(key = 'key', value = 'value', -rowname)
# 
# # parameters
# circos.clear()
# circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
# par(mar = rep(0, 4))
# 
# # color palette
# mycolor <- viridis(10, alpha = 1, begin = 0, end = 1, option = "D")
# mycolor <- mycolor[sample(1:10)]
# 
# # Base plot
# chordDiagram(
#   x = data_long, 
#   grid.col = mycolor,
#   transparency = 0.25,
#   directional = 1,
#   direction.type = c("arrows", "diffHeight"), 
#   diffHeight  = -0.04,
#   annotationTrack = "grid", 
#   annotationTrackHeight = c(0.05, 0.1),
#   link.arr.type = "big.arrow", 
#   link.sort = TRUE, 
#   link.largest.ontop = TRUE)
# 
# 
# t <- master_results_AC %>% 
#   select(species, region) %>% 
#   left_join(select(spp_zones_all, species, freq_climate), by = "species") %>% 
#   filter(freq_climate %in% c("A", "AC", "C")) 
# 
# mycolor <- viridis(10, alpha = 1, begin = 0, end = 1, option = "D")
# 
# chordDiagram(
#   x = t, 
#   # grid.col = mycolor,
#   transparency = 0.25,
#   directional = 1,
#   direction.type = c("arrows", "diffHeight"), 
#   diffHeight  = -0.04,
#   annotationTrack = "grid", 
#   annotationTrackHeight = c(0.05, 0.1),
#   link.arr.type = "big.arrow", 
#   link.sort = TRUE, 
#   link.largest.ontop = TRUE,
#   small.gap = 0.5)
