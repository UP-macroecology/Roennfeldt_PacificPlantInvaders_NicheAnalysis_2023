
library(dplyr)
library(stringr)
library(tidyr) # to use unite()


# preamble ----------------------------------------------------------------

rm(list = ls())

path_transfer <- "T:/Holle_Roennfeldt/"

logit <- function(x) {x = ifelse(x < 0.0001,0.0001,ifelse(x > 0.9999,.9999,x));log(x/(1 - x))}

# load data ---------------------------------------------------------------

# species list 
load("data/spp_suitable_AC.RData")

# results 
load("results/ecospat/master_results.RData")

# trait data
load(paste0(path_transfer, "trait_data_processed/species_pacific_traits_GIFT.RData")) 
load(paste0(path_transfer, "trait_data_processed/species_pacific_traits_TRY.RData")) 

# geographic traits
load("data/trait_analysis/df_fragmentation_metrics.RData")
load("data/trait_analysis/native_centroid.RData")
load("data/trait_analysis/native_range_size.RData")
load("data/trait_analysis/year_first_intro_Seebens.RData")


# response data -----------------------------------------------------------

# subset results overview for species in the final spp selection 

df_res <- master_results %>%
  filter(species %in% spp_suitable_AC) %>%
  select(species, region, overlap, rel_expansion, rel_stability, rel_unfilling, rel_abandonment, rel_pioneering) %>% # select which columns to keep
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(expansion = rel_expansion / total_esu) %>%
  mutate(stability = rel_stability / total_esu) %>%
  mutate(unfilling = rel_unfilling / total_esu) %>%
  na.omit()


# trait data --------------------------------------------------------------

#' ---------------------------

# prepare fragmentation data
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
  select(!c("CORE", "CLUMPY"))

#' ---------------------------

# prepare time since introduction data

year_first_intro_Seebens <- year_first_intro_Seebens %>%
  select(!pac_region) %>%
  pivot_longer(cols = !species,
               names_to = "region",
               values_to = "years_since_intro") 
  
#' ---------------------------

# prepare niche breadth data
df_niche_breadth_centroid <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(df_niche_breadth_centroid) <- c("species", "niche_breadth_zcor", "niche_centroid1_global", "niche_centroid2_global", "niche_centroid1_native", "niche_centroid2_native")


spp_breadth <- list.files("data/trait_analysis/niche_breadth_centroid/", pattern = "niche_breadth_centroid_twice_") %>%
  str_remove(".RData") %>%
  str_split(pattern = "_") %>%
  map(~ .x[[5]]) %>%
  simplify()

for (spp in spp_breadth) {
  
  load(paste0("data/trait_analysis/niche_breadth_centroid/niche_breadth_centroid_twice_",spp,".RData")) # object name: nbc_spec_df
  
  df_niche_breadth_centroid <- rbind(df_niche_breadth_centroid, nbc_spec_df)
} # end of loop over spp_suitable_AC

#' ---------------------------

# prepare df with specie traits and georgaphic characteristics
# df_traits_myco <- cbind(species_pacific_traits_GIFT, species_pacific_traits_TRY$mycorrhiza_try_crit80_TRY) %>% # add the mycrrhiza (80) column from TRY to the GIFT df
#   rename("mycorrhiza_try_crit80_TRY" = "species_pacific_traits_TRY$mycorrhiza_try_crit80_TRY", "species" = "species_orig") %>%
#   mutate(mycorrhiza_both = case_when(!is.na(mycorrhiza_GIFT) ~ mycorrhiza_GIFT, # combine trait information from both sources in one columns
#                                      is.na(mycorrhiza_GIFT) ~ mycorrhiza_try_crit80_TRY)) %>%
#   select(!c(mycorrhiza_GIFT, mycorrhiza_try_crit80_TRY, work_species, min_height_GIFT)) %>%
#   #make sure that numeric columns are really numeric
#   mutate_at(c("mean_height_GIFT", "max_height_GIFT", 
#               "mean_seedmass_GIFT", "max_seedmass_GIFT", "min_seedmass_GIFT",
#               "max_elev_range_GIFT"), as.numeric) %>%
#   select(species, mean_height_GIFT, mean_seedmass_GIFT, growth_form_GIFT, lifecycle_GIFT, max_elev_range_GIFT, mycorrhiza_both) %>%
#   rename_with(stringr::str_replace, 
#               pattern = "_GIFT", replacement = "", 
#               matches("_GIFT")) %>%
#   filter(species %in% spp_suitable_AC) %>%
#   # add geographic "traits"
#   left_join(select(native_centroid_df, species, lat_centroid, lon_centroid), by = "species") %>%
#   left_join(select(native_range_df, species, range_both), by = "species") %>%
#   left_join(df_niche_breadth_centroid, by = "species") %>%
#   select(!c(niche_centroid1_native, niche_centroid2_native)) %>%
#   rename("range_size" = "range_both") %>%
#   na.omit()
  
# including mycorrhizal association as trait would reduce sample size too much. Exclude the trait: 

df_traits <- cbind(species_pacific_traits_GIFT, species_pacific_traits_TRY$mycorrhiza_try_crit80_TRY) %>% # add the mycrrhiza (80) column from TRY to the GIFT df
  rename("mycorrhiza_try_crit80_TRY" = "species_pacific_traits_TRY$mycorrhiza_try_crit80_TRY", "species" = "species_orig") %>%
  mutate(mycorrhiza_both = case_when(!is.na(mycorrhiza_GIFT) ~ mycorrhiza_GIFT, # combine trait information from both sources in one columns
                                     is.na(mycorrhiza_GIFT) ~ mycorrhiza_try_crit80_TRY)) %>%
  select(!c(mycorrhiza_GIFT, mycorrhiza_try_crit80_TRY, work_species, min_height_GIFT)) %>%
  #make sure that numeric columns are really numeric
  mutate_at(c("mean_height_GIFT", "max_height_GIFT", 
              "mean_seedmass_GIFT", "max_seedmass_GIFT", "min_seedmass_GIFT",
              "max_elev_range_GIFT"), as.numeric) %>%
  select(species, mean_height_GIFT, mean_seedmass_GIFT, growth_form_GIFT, lifecycle_GIFT, max_elev_range_GIFT) %>%
  rename_with(stringr::str_replace, 
              pattern = "_GIFT", replacement = "", 
              matches("_GIFT")) %>%
  filter(species %in% spp_suitable_AC) %>%
  # add geographic "traits"
  left_join(select(native_centroid_df, species, lat_centroid, lon_centroid), by = "species") %>%
  left_join(select(native_range_df, species, range_both), by = "species") %>%
  left_join(df_niche_breadth_centroid, by = "species") %>%
  select(!c(niche_centroid1_native, niche_centroid2_native)) %>%
  rename("range_size" = "range_both") %>%
  na.omit()


# input data for trait analysis -------------------------------------------

# merge data and prepare column with "species_region" name
df_res_traits <- df_res %>% left_join(df_traits, by = "species") %>%
  unite(species_region, c("species", "region"), remove = FALSE, sep = " ") %>%
  left_join(df_frag_wide, by = "region") %>% # add fragmentation metric (requires region name)
  left_join(year_first_intro_Seebens, by = c("species", "region")) %>%
  na.omit()

# standardise data



# growth form
df_res_traits[df_res_traits == "herb"] <- 1
df_res_traits[df_res_traits == "shrub"] <- 2
df_res_traits[df_res_traits == "tree"] <- 3

# life-cycle
df_res_traits[df_res_traits == "annual"] <- 1
df_res_traits[df_res_traits == "biennial"] <- 2
df_res_traits[df_res_traits == "perennial"] <- 3


# make sure all trait columns are numeric
# df_res_traits <- df_res_traits %>%
#   mutate(across(!c(species_region, species, region), as.numeric)) %>%
#   na.omit()



# standardise trait data --------------------------------------------------

# Standardise traits
df_res_traits$mean_height <- scale(as.numeric(df_res_traits$mean_height))
df_res_traits$mean_seedmass <- scale(as.numeric(df_res_traits$mean_seedmass))
df_res_traits$max_elev_range <- scale(as.numeric(df_res_traits$max_elev_range))
df_res_traits$range_size <- scale(as.numeric(df_res_traits$range_size))
df_res_traits$niche_breadth_zcor <- scale(as.numeric(df_res_traits$niche_breadth_zcor))
df_res_traits$niche_centroid1_global <- scale(as.numeric(df_res_traits$niche_centroid1_global))
df_res_traits$niche_centroid2_global <- scale(as.numeric(df_res_traits$niche_centroid2_global))
df_res_traits$AI <- scale(as.numeric(df_res_traits$AI))
df_res_traits$CAI <- scale(as.numeric(df_res_traits$CAI))
df_res_traits$lat_centroid <- df_res_traits$lat_centroid/90
df_res_traits$lon_centroid <- df_res_traits$lon_centroid/180
df_res_traits$years_since_intro <- scale(as.numeric(df_res_traits$years_since_intro))


# Logit-transform response variables
df_res_traits$unfilling <- logit(df_res_traits$unfilling)
df_res_traits$stability <- logit(df_res_traits$stability)
df_res_traits$expansion <- logit(df_res_traits$expansion)

spp_traits <- unique(df_res_traits$species)


save(df_res_traits, file = "data/trait_analysis/trait_analysis_input.RData")
save(spp_traits, file = "data/spp_trait_analysis.RData")
