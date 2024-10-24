#' ---------------------------
#
# Purpose of script: merging geographic and species functional trait to prepare 
# the input data for the trait analysis
# Author: Anna Rönnfeldt
# Date Created: ~ 2024-02
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------

library(dplyr)
library(stringr)
library(tidyr) # to use unite()
library(ggplot2)

# preamble ----------------------------------------------------------------

path_transfer <- "T:/Holle_Roennfeldt/"
# path_transfer <- "Y:/AG26/Transfer/Holle_Roennfeldt/"

logit <- function(x) {x = ifelse(x < 0.0001,0.0001,ifelse(x > 0.9999,.9999,x));log(x/(1 - x))}

# required data
load("data/spp_suitable_AC.RData") # species list 
load("results/ecospat/master_results_AC.RData") # results niche comparison


load(paste0(path_transfer, "trait_data_processed/species_pacific_traits_GIFT.RData")) # GIFT trait data


# geographic traits
load("data/trait_analysis/native_niche_breadth_centroid.RData")
load("data/trait_analysis/native_range_size.RData")
load("data/trait_analysis/year_first_intro_Seebens.RData")
load("data/trait_analysis/lat_dist.RData")

# response data -----------------------------------------------------------

# subset results overview for species in the final spp selection 
df_results <- master_results_AC %>%
  filter(species %in% spp_suitable_AC) %>%
  dplyr::select(species, region, rel_expansion, rel_stability, rel_unfilling, rel_abandonment, rel_pioneering, expansion, unfilling, stability) %>% # select which columns to keep
 rename("orig_expansion" = "expansion",
        "orig_unfilling" = "unfilling",
        "orig_stability" = "stability") %>% 
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(expansion = rel_expansion / total_esu) %>%
  mutate(stability = rel_stability / total_esu) %>%
  mutate(unfilling = rel_unfilling / total_esu) 


# time since introduction -------------------------------------------------

year_first_intro_Seebens[year_first_intro_Seebens == "NI"] <- NA

year_first_intro_Seebens <- year_first_intro_Seebens %>%
  dplyr::select(!pac_region) %>%
  pivot_longer(cols = !species,
               names_to = "region",
               values_to = "years_since_intro") 
  

year_first_intro_Seebens$years_since_intro <- as.numeric(year_first_intro_Seebens$years_since_intro)


# niche breadth -----------------------------------------------------------
# 
# df_niche_breadth_centroid <- data.frame(matrix(nrow = 0, ncol = 6))
# colnames(df_niche_breadth_centroid) <- c("species", "niche_breadth_zcor",
#                                          "niche_centroid1_global", "niche_centroid2_global",
#                                          "niche_centroid1_native", "niche_centroid2_native")
# 
# 
# spp_breadth <- list.files("data/trait_analysis/niche_breadth_centroid/", pattern = "niche_breadth_centroid_twice_") %>%
#   str_remove(".RData") %>%
#   str_split(pattern = "_") %>%
#   map(~ .x[[5]]) %>%
#   simplify()
# 
# for (spp in spp_breadth) {
# 
#   load(paste0("data/trait_analysis/niche_breadth_centroid/niche_breadth_centroid_twice_",spp,".RData")) # object name: nbc_spec_df
# 
#   df_niche_breadth_centroid <- rbind(df_niche_breadth_centroid, nbc_spec_df)
# } # end of loop over spp_suitable_AC



# species traits ----------------------------------------------------------

spec_traits <- species_pacific_traits_GIFT %>%
  rename("species" = "species_orig") %>% 
  dplyr::select(species, mean_height_GIFT, mean_seedmass_GIFT, growth_form_GIFT, lifecycle_GIFT) %>% 
  rename_with(stringr::str_replace,
              pattern = "_GIFT", replacement = "",
              matches("_GIFT")) %>%
  filter(species %in% spp_suitable_AC)



# merge data --------------------------------------------------------------

# prepare the input data for the trait analysis (TA)
input_TA <- df_results %>% left_join(spec_traits, by = "species") %>%
  unite(species_region, c("species", "region"), remove = FALSE, sep = " ") %>%
  left_join(dplyr::select(native_range_df, species, range_both), by = "species") %>% # native range size
  rename("range_size_nat" = "range_both") %>% 
  left_join(select(df_native_niche, species, niche_breadth_zcor, niche_centroid1_global, niche_centroid2_global), by = "species") %>%  # native niche breadth and centroid
  rename("niche_breadth_nat" = "niche_breadth_zcor",
         "niche_centroid_a_nat" = "niche_centroid1_global",
         "niche_centroid_b_nat" = "niche_centroid2_global") %>% 
  left_join(year_first_intro_Seebens, by = c("species", "region")) %>% # years since first introduction
  left_join(df_lat_distance, by = c("species", "region")) %>% # nat & intr range centroid + euclidean distance
  na.omit()

save(input_TA, file = "shiny/NicheDyn_exploration/data/input_TA_unstand.RData")


# standardise data --------------------------------------------------------

# growth form
input_TA[input_TA == "herb"] <- 1
input_TA[input_TA == "shrub"] <- 2
input_TA[input_TA == "tree"] <- 3

# life-cycle
input_TA[input_TA == "annual"] <- 1
input_TA[input_TA == "biennial"] <- 2
input_TA[input_TA == "perennial"] <- 3


# make sure all trait columns are numeric
# then standardise
input_TA <- input_TA %>% 
  mutate(across(!c(species_region, species, region), as.numeric)) %>%
  mutate(mean_height = scale(mean_height)) %>% 
  mutate(mean_seedmass = scale(log(mean_seedmass + 0.00001))) %>% 
  mutate(growth_form = scale(growth_form)) %>% 
  mutate(lifecycle = scale(lifecycle)) %>% 
  mutate(range_size_nat = scale(range_size_nat)) %>% 
  mutate(years_since_intro = scale(log(years_since_intro))) %>% 
  mutate(niche_breadth_nat = scale(niche_breadth_nat)) %>% 
  mutate(niche_centroid_a_nat = scale(niche_centroid_a_nat)) %>% 
  mutate(niche_centroid_b_nat = scale(niche_centroid_b_nat)) %>% 
  mutate(lat_dist = scale(lat_dist)) %>%
  mutate(unfilling = logit(unfilling)) %>% 
  mutate(expansion = logit(expansion)) %>% 
  mutate(stability = logit(stability)) %>% 
  mutate(rel_unfilling = logit(rel_unfilling)) %>% 
  mutate(rel_expansion = logit(rel_expansion)) %>% 
  mutate(rel_stability = logit(rel_stability)) %>% 
  mutate(rel_abandonment = logit(rel_abandonment)) %>% 
  mutate(rel_pioneering = logit(rel_pioneering)) %>% 
  mutate(orig_expansion = logit(orig_expansion)) %>% 
  mutate(orig_stability = logit(orig_stability)) %>% 
  mutate(orig_unfilling = logit(orig_unfilling)) %>% 
  na.omit()

save(input_TA, file = "data/trait_analysis/input_TA_scale_log.RData")
