
# preamble ----------------------------------------------------------------
library(dplyr)

rm(list = ls())

# path to data
path_transfer <- "Y:/AG26/Transfer/Holle_Roennfeldt/"


# load data ---------------------------------------------------------------

# species selection:
# object name: spp_suitable
load("data/spp_suitable_after_thinning.RData")

# spp + GIFT trait data that Valén prepared
# object name: species_pacific_traits_GIFT
load(paste0(path_transfer, "trait_data_processed/species_pacific_traits_GIFT.RData")) 
load(paste0(path_transfer, "trait_data_processed/species_pacific_traits_TRY.RData")) 

# georgaphic traits:
load("data/trait_analysis/native_centroid.RData")
load("data/trait_analysis/native_range_size.RData")

load("results/ecospat/master_results.RData")

# prepare data ------------------------------------------------------------


species_pacific_traits_GIFT$species_orig <- as.factor(species_pacific_traits_GIFT$species_orig) 
species_pacific_traits_TRY$species_orig <- as.factor(species_pacific_traits_TRY$species_orig) 


species_pacific_traits_GIFT <- species_pacific_traits_GIFT %>%
  filter(species_orig %in% spp_suitable) %>%
  select(!c(habitat_GIFT, defense_GIFT, seeds_GIFT, lifespan_GIFT, min_SSD_GIFT, max_SSD_GIFT, mean_SSD_GIFT, self_fertilization_GIFT, min_elev_range_GIFT, mean_elev_range_GIFT))

species_pacific_traits_TRY <- species_pacific_traits_TRY %>%
  filter(species_orig %in% spp_suitable)


# merge trait data
trait_df <- cbind(species_pacific_traits_GIFT, species_pacific_traits_TRY$mycorrhiza_try_crit80_TRY) %>% # add the mycrrhiza (80) column from TRY to the GIFT df
  rename("mycorrhiza_try_crit80_TRY" = "species_pacific_traits_TRY$mycorrhiza_try_crit80_TRY", "species" = "species_orig") %>%
  mutate(mycorrhiza_both = case_when(!is.na(mycorrhiza_GIFT) ~ mycorrhiza_GIFT, # combine trait information from both sources in one columns
                                     is.na(mycorrhiza_GIFT) ~ mycorrhiza_try_crit80_TRY)) %>%
  select(!c(mycorrhiza_GIFT, mycorrhiza_try_crit80_TRY, work_species, min_height_GIFT)) %>%
  #make sure that numeric columns are really numeric
  mutate_at(c("mean_height_GIFT", "max_height_GIFT", 
              "mean_seedmass_GIFT", "max_seedmass_GIFT", "min_seedmass_GIFT",
              "max_elev_range_GIFT"), as.numeric)



# first selection cycle ---------------------------------------------------

# test whether woodiness and plant height are correlated
shapiro.test(trait_df$mean_height_GIFT)
shapiro.test(trait_df$woodiness)

# Add 1:1 line
qqline(cats$cheetah_kmh)


# criterion 1: plant height -> mean 

sel_1 <- trait_df %>%
  filter(complete.cases(mean_height_GIFT)) %>% 
  filter(complete.cases(mean_seedmass_GIFT)) %>%
  filter(complete.cases(growth_form_GIFT)) %>% 
  filter(complete.cases(lifecycle_GIFT)) %>% 
  filter(complete.cases(max_elev_range_GIFT)) %>%
  filter(complete.cases(woodiness_GIFT)) 


sel_2 <- trait_df %>%
  filter(complete.cases(max_height_GIFT)) %>%
  filter(complete.cases(growth_form_GIFT)) %>% 
  filter(complete.cases(lifecycle_GIFT)) %>% 
  filter(complete.cases(max_elev_range_GIFT)) %>%
  filter(complete.cases(woodiness_GIFT))  %>%
  filter(complete.cases(mycorrhiza_both))


trait_example_df <- trait_df %>%
  filter(complete.cases(mean_height_GIFT)) %>% 
  filter(complete.cases(growth_form_GIFT)) %>% 
  filter(complete.cases(lifecycle_GIFT)) %>% 
  filter(complete.cases(max_elev_range_GIFT)) %>%
  filter(complete.cases(woodiness_GIFT)) %>%
  # add geographic traits 
  left_join(select(native_centroid_df, species, lat_centroid), by = "species") %>%
  left_join(select(native_range_df, species, range_both), by = "species") %>%
  select(!c(max_height_GIFT, mean_seedmass_GIFT, min_seedmass_GIFT, max_seedmass_GIFT, dispersal_GIFT, nitrogen_GIFT)) %>%
  na.omit() 


# add results from niche comparison ---------------------------------------


trait_data_all <- master_results %>%
  select(c(species, region, overlap, expansion, stability, unfilling, rel.expansion, rel.stability, rel.unfilling, rel.abandonment, rel.pioneering)) %>%
  left_join(trait_example_df, by = "species") %>%
  na.omit() %>%
  rename("rel_expansion" = "rel.expansion",
         "rel_stability" = "rel.stability",
         "rel_unfilling" = "rel.unfilling",
         "rel_abandonment" = "rel.abandonment",
         "rel_pioneering" = "rel.pioneering",
         "mean_height" = "mean_height_GIFT",
         "woodiness" = "woodiness_GIFT",
         "growth_form" = "growth_form_GIFT",
         "lifecycle" = "lifecycle_GIFT",
         "max_elev_range" = "max_elev_range_GIFT",
         "mycorrhiza" = "mycorrhiza_both") 
  

save(trait_data_all, file = "data/trait_analysis/trait_data_all.RData")


trait_data_all <- master_results %>%
  select(c(species, region, overlap, expansion, stability, unfilling, rel.expansion, rel.stability, rel.unfilling, rel.abandonment, rel.pioneering)) %>%
  left_join(trait_example_df, by = "species") %>%
  na.omit() %>%
  rename("rel_expansion" = "rel.expansion",
         "rel_stability" = "rel.stability",
         "rel_unfilling" = "rel.unfilling",
         "rel_abandonment" = "rel.abandonment",
         "rel_pioneering" = "rel.pioneering",
         "mean_height" = "mean_height_GIFT",
         "woodiness" = "woodiness_GIFT",
         "growth_form" = "growth_form_GIFT",
         "lifecycle" = "lifecycle_GIFT",
         "max_elev_range" = "max_elev_range_GIFT",
         "mycorrhiza" = "mycorrhiza_both") %>%
  select(!c("overlap", "expansion", "unfilling", "stability", "species_id"))

save(trait_data_all, file = "data/trait_analysis/trait_data_all_example.RData")
