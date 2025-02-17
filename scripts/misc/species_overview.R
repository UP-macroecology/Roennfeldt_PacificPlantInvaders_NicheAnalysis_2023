#' ---------------------------
#
# Purpose of script:creating an intermediate overview over the species data halfway thorugh the project
# Author: Anna Rönnfeldt
# Email: roennfeldt@uni-potsdam.de
#
# Notes:
#
#' ---------------------------


library(dplyr)
library(miscTools) # for insertRow

rm(list = ls())


load("data/occurrence_data/occ_cleaned_slim.RData")
spp_slim <- data.frame(occ_cleaned_slim = unique(occ_cleaned_slim$species)) 

load("data/status_assignment/GloNAF_status.RData")
load("data/status_assignment/GIFT_names.RData")

spp_glonaf <- glonaf_status_info %>%
  select(species_orig, standardized_name) %>%
  distinct(species_orig,standardized_name) %>%
  arrange(species_orig, .by_group = TRUE) %>%
  distinct(species_orig, .keep_all = TRUE)

spp_glonaf <- unique(spp_glonaf[c("species_orig","standardized_name")])
length(unique(spp_glonaf$standardized_name))


GIFT_names <- unique(GIFT_names[c("searched_name", "GIFT_genus", "GIFT_species_ep")])


# species pre-selection --------------------------------------------------------

load("data/occurrence_data/regional_occs/criterion_1/occ_count_crit_1.RData")
load("data/occurrence_data/regional_occs/criterion_2/occ_count_crit_2.RData")

occ_count_crit_1 <- occ_count_crit_1 %>% 
  arrange(species) %>%
  distinct(species, .keep_all = TRUE)
occ_count_crit_2 <- occ_count_crit_2 %>%
  arrange(species) %>%
  distinct(species, .keep_all = TRUE)

apply(occ_count_crit_1[,-1], 2, FUN = function(x) sum(x >= 20, na.rm = TRUE))
apply(occ_count_crit_2[,-1], 2, FUN = function(x) sum(x >= 20, na.rm = TRUE))

suitable_1 <- occ_count_crit_1[,-1]
suitable_1[suitable_1 < 20] <- 0
suitable_1[suitable_1 >= 20] <- 1
suitable_1$species <- occ_count_crit_1$species
suitable_1 <- suitable_1 %>% relocate(species)
suitable_1$mainland_regions <- rowSums(suitable_1[,4:10])
spp_suitable_1 <- suitable_1[!(suitable_1$native_occs == 0 | suitable_1$pacific_occs == 0 | suitable_1$mainland_regions == 0),]
colSums(spp_suitable_1[,-1], na.rm = TRUE)


suitable_2 <- occ_count_crit_2[,-1]
suitable_2[suitable_2 < 20] <- 0
suitable_2[suitable_2 >= 20] <- 1
suitable_2$species <- occ_count_crit_2$species
suitable_2 <- suitable_2 %>% relocate(species) 
suitable_2$mainland_regions <- rowSums(suitable_2[,4:10])
spp_suitable_2 <- suitable_2[!(suitable_2$native_occs == 0 | suitable_2$pacific_occs == 0 | suitable_2$mainland_regions == 0),]
colSums(spp_suitable_2[,-1], na.rm = TRUE)


spp_1 <- spp_suitable_1$species
spp_2 <- spp_suitable_2$species


# join everything together ------------------------------------------------
species_overview <- species_names %>%
  left_join(spp_slim, by = c("species_orig" = "occ_cleaned_slim"), keep = TRUE) %>%
  left_join(spp_glonaf, by = "species_orig") %>%
  rename("glonaf_name" = "standardized_name") %>%
  left_join(GIFT_names, by = c("species_changed" = "searched_name")) %>%
  mutate(criterion_1 = case_when(species_orig %in% spp_1 | species_changed %in% spp_1 | species_no_x %in% spp_1 ~ "included")) %>%
  mutate(criterion_2 = case_when(species_orig %in% spp_2 | species_changed %in% spp_2 | species_no_x %in% spp_2 ~ "included"))


save(species_overview, file = "data/species_overview.RData")

