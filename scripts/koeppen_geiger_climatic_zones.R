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
# library(kgc)
# library(tidyr)

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

create_radar_input <- function(df){
  
  max_values <- apply(df, 2, min)
  min_values <- apply(df, 2, max)
  mean_values <- apply(df, 2, mean)
  
  df_radar <- rbind(max_values, min_values, mean_values)
  rownames(df_radar) <- NULL
  
  return(as.data.frame(df_radar))
} # end of function definition

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
names(mean_values) <- c("mean height", "mean seedmass", "growth form", "life cycle\nshort to long", 
                        "range size", "niche breadth", "niche centroid\nwarm to cold", "niche centroid\n wet dry")

max_values <- apply(mean_values, 2, max)


radar_input <- rbind(max_values,
                     rep(0,8),
                     mean_values)

radarchart(radar_input)

