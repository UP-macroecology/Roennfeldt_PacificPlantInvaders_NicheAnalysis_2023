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
#library(tidyr)
# 
# LookupCZ()
# 
# data <- data.frame(Site = c("GC","UFS","NEG"),
#                    Longitude = c(-15.42,10.98,34.78),
#                    Latitude = c(27.82,47.42,30.86))
# data <- data.frame(data,
#                    rndCoord.lon = RoundCoordinates(data$Longitude),
#                    rndCoord.lat = RoundCoordinates(data$Latitude))
# data <- data.frame(data, ClimateZ = LookupCZ(data))
# 
# c_zones <- climatezones


# loop over species

# path_occ_data <- "X:/roennfeldt/Projects/PhD_C1/data/regional_occs/criterion_1/native/"
# # load native occs
# 
# load("data/spp_suitable_AC.RData")
# 
# df_occ_zones <- as.data.frame(matrix(nrow = 0, ncol = 5))
# names(df_occ_zones) <- c("species", "occ_id", "lon", "lat", "climate_zone")
# 
# counter <- 0
# 
# for (spp in spp_suitable_AC) {
#   
#   counter <- counter + 1
#   print(counter)
#   # load native occurrences
#   load(paste0(path_occ_data,"nat_occs",spp,".RData")) # object name: nat
#   
#   spp_occs <- nat %>% 
#     select(occ_id, lon, lat) %>% 
#     mutate(rndCoord.lon = RoundCoordinates(lon)) %>% 
#     mutate(rndCoord.lat = RoundCoordinates(lat))
#   
#   spp_occ_zones <- data.frame(spp_occs, ClimateZ = LookupCZ(spp_occs)) %>% 
#     select(occ_id, lon, lat, ClimateZ) %>% 
#     mutate(species = spp, .before = occ_id) %>% 
#     rename(climate_zone = ClimateZ)
#   
#   df_occ_zones <- rbind(df_occ_zones, spp_occ_zones)
# 
#   } # end of for loop over species



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

# load("data/df_occ_climate_zones.RData")

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


for (spp in spp_suitable_AC) {
  
  print(spp)
  
  zones <- as.data.frame(table(subset(df_zones, species == spp)$main_climate)) 
  
  max_zone <- as.character(zones[which.max(zones$Freq),"Var1"])
  
  spp_zones <- zones %>% 
    pivot_wider(names_from = Var1, values_from = Freq) %>%
    mutate(species = spp, .before = A) %>% 
    mutate(main_climate = max_zone)
}


