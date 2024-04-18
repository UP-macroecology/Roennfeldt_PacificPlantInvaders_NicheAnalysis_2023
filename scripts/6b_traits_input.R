library(dplyr)
library(stringr)
library(tidyr) # to use unite()


# preamble ----------------------------------------------------------------

rm(list = ls())

path_transfer <- "T:/Holle_Roennfeldt/"

logit <- function(x) {x = ifelse(x < 0.0001,0.0001,ifelse(x > 0.9999,.9999,x));log(x/(1 - x))}

# required data
load("data/spp_suitable_AC.RData") # species list 
load("results/ecospat/master_results_AC.RData") # results niche comparison


load(paste0(path_transfer, "trait_data_processed/species_pacific_traits_GIFT.RData")) # GIFT trait data
load(paste0(path_transfer, "trait_data_processed/species_pacific_traits_TRY.RData")) # TRY data

# geographic traits
load("data/trait_analysis/df_fragmentation_metrics.RData")
load("data/trait_analysis/native_niche_breadth_centroid.RData")
load("data/trait_analysis/native_range_size.RData")
load("data/trait_analysis/introduced_range_size.RData")
load("data/trait_analysis/year_first_intro_Seebens.RData")
load("data/trait_analysis/eucl_dist.RData")

# response data -----------------------------------------------------------

# subset results overview for species in the final spp selection 
df_results <- master_results_AC %>%
  filter(species %in% spp_suitable_AC) %>%
  dplyr::select(species, region, overlap, rel_expansion, rel_stability, rel_unfilling, rel_abandonment, rel_pioneering) %>% # select which columns to keep
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(expansion = rel_expansion / total_esu) %>%
  mutate(stability = rel_stability / total_esu) %>%
  mutate(unfilling = rel_unfilling / total_esu) %>%
  na.omit()



# fragmentation -----------------------------------------------------------

df_fragmentation[df_fragmentation == "EUROPE"] <- "eur"
df_fragmentation[df_fragmentation == "AFRICA"] <- "afr"
df_fragmentation[df_fragmentation == "ASIA-TEMPERATE"] <- "ate"
df_fragmentation[df_fragmentation == "ASIA-TROPICAL"] <- "atr"
df_fragmentation[df_fragmentation == "AUSTRALASIA"] <- "aus"
df_fragmentation[df_fragmentation == "NORTHERN AMERICA"] <- "nam"
df_fragmentation[df_fragmentation == "SOUTHERN AMERICA"] <- "sam"
df_fragmentation[df_fragmentation == "PACIFIC"] <- "pac"

# spread df to wide format
df_frag_wide <- spread(df_fragmentation, metric_name, metric_value) %>%
  dplyr::select(!c("CORE", "CLUMPY"))


# time since introduction -------------------------------------------------

year_first_intro_Seebens <- year_first_intro_Seebens %>%
  dplyr::select(!pac_region) %>%
  pivot_longer(cols = !species,
               names_to = "region",
               values_to = "years_since_intro") 

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
  dplyr::select(species, mean_height_GIFT, mean_seedmass_GIFT, growth_form_GIFT, lifecycle_GIFT, max_elev_range_GIFT) %>% 
  rename_with(stringr::str_replace,
              pattern = "_GIFT", replacement = "",
              matches("_GIFT")) %>%
  filter(species %in% spp_suitable_AC) %>% 
  na.omit() # only keep species with trait information for all traits


# merge data --------------------------------------------------------------

# prepare the input data for the trait analysis (TA)
input_TA <- df_results %>% left_join(spec_traits, by = "species") %>%
  unite(species_region, c("species", "region"), remove = FALSE, sep = " ") %>%
  left_join(df_frag_wide, by = "region") %>% # add fragmentation metric (requires region name)
  left_join(dplyr::select(native_range_df, species, range_both), by = "species") %>% # native range size
  rename("range_size_nat" = "range_both") %>% 
  left_join(dplyr::select(intro_range_df, species, region, range_both), by = c("species", "region")) %>% # intro. range size
  rename("range_size_intr" = "range_both") %>% 
  left_join(select(df_native_niche, species, niche_breadth_zcor, niche_centroid1_global, niche_centroid2_global), by = "species") %>%  # native niche breadth and centroid
  rename("niche_breadth_nat" = "niche_breadth_zcor",
         "niche_centroid_a_nat" = "niche_centroid1_global",
         "niche_centroid_b_nat" = "niche_centroid2_global") %>% 
  left_join(year_first_intro_Seebens, by = c("species", "region")) %>% # years since first introduction
  left_join(eucl_dist, by = c("species", "region")) %>% # nat & intr range centroid + euclidean distance
  dplyr::select(!AI) %>%  
  na.omit()


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
input_TA <- input_TA %>%
  mutate(across(!c(species_region, species, region), as.numeric)) %>%
  na.omit()


# Standardise traits
input_TA$mean_height <- scale(as.numeric(input_TA$mean_height))
input_TA$mean_seedmass <- scale(as.numeric(input_TA$mean_seedmass))
input_TA$max_elev_range <- scale(as.numeric(input_TA$max_elev_range))
input_TA$CAI <- scale(as.numeric(input_TA$CAI))
input_TA$range_size_nat <- scale(as.numeric(input_TA$range_size_nat))
input_TA$range_size_intr <- scale(as.numeric(input_TA$range_size_intr))
input_TA$years_since_intro <- scale(as.numeric(input_TA$years_since_intro))
input_TA$lon_intr <- input_TA$lon_intr/180
input_TA$lat_intr <- input_TA$lat_intr/90
input_TA$lon_nat <- input_TA$lon_nat/180
input_TA$lat_nat <- input_TA$lat_nat/90
input_TA$eucl_dist <- scale(as.numeric(input_TA$eucl_dist))
input_TA$niche_breadth_nat <- scale(as.numeric(input_TA$niche_breadth_nat))
input_TA$niche_centroid_a_nat <- scale(as.numeric(input_TA$niche_centroid_a_nat))
input_TA$niche_centroid_b_nat <- scale(as.numeric(input_TA$niche_centroid_b_nat))




# Logit-transform response variables
input_TA$unfilling <- logit(input_TA$unfilling)
input_TA$stability <- logit(input_TA$stability)
input_TA$expansion <- logit(input_TA$expansion)

spp_traits <- unique(input_TA$species)

save(input_TA, file = "data/trait_analysis/input_TA.RData")
