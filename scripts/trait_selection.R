

# preamble ----------------------------------------------------------------
library(dplyr)

rm(list = ls())

# path to data
path_transfer <- "T:/Holle_Roennfeldt/"


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

# results 
load("results/")

# prepare data ------------------------------------------------------------


spp_left <- c("Hedera helix", "Plantago major", "Prunella vulgaris", "Sonchus oleraceus", "Stellaria media", "Trifolium arvense", "Trifolium dubium", "trifolium repens",
              "Typha latifolia", "Verbascum thapsus", "Veronica serpyllifolia", "Vicia sativa", "Vulpia bromoides", "Xanthium strumarium", "Zoysia matrella")


spp_combined <- c(spp_suitable, spp_left)


species_pacific_traits_GIFT$species_orig <- as.factor(species_pacific_traits_GIFT$species_orig) 
species_pacific_traits_TRY$species_orig <- as.factor(species_pacific_traits_TRY$species_orig) 


species_pacific_traits_GIFT <- species_pacific_traits_GIFT %>%
  filter(species_orig %in% spp_combined) %>%
  select(!c(habitat_GIFT, defense_GIFT, seeds_GIFT, lifespan_GIFT, min_SSD_GIFT, max_SSD_GIFT, mean_SSD_GIFT, self_fertilization_GIFT, min_elev_range_GIFT, mean_elev_range_GIFT))

species_pacific_traits_TRY <- species_pacific_traits_TRY %>%
  filter(species_orig %in% spp_combined)


# merge trait data
trait_df <- cbind(species_pacific_traits_GIFT, species_pacific_traits_TRY$mycorrhiza_try_crit80_TRY) %>% # add the mycrrhiza (80) column from TRY to the GIFT df
  rename("mycorrhiza_try_crit80_TRY" = "species_pacific_traits_TRY$mycorrhiza_try_crit80_TRY", "species" = "species_orig") %>%
  mutate(mycorrhiza_both = case_when(!is.na(mycorrhiza_GIFT) ~ mycorrhiza_GIFT, # combine trait information from both sources in one columns
                                     is.na(mycorrhiza_GIFT) ~ mycorrhiza_try_crit80_TRY)) %>%
  select(!c(mycorrhiza_GIFT, mycorrhiza_try_crit80_TRY, work_species, min_height_GIFT))



# first selection cycle ---------------------------------------------------

# criterion 1: plant height -> mean 

sel_1 <- trait_df %>%
  filter(complete.cases(mean_height_GIFT)) %>% 
  filter(complete.cases(growth_form_GIFT)) %>% 
  filter(complete.cases(lifecycle_GIFT)) %>% 
  filter(complete.cases(max_elev_range_GIFT)) %>%
  filter(complete.cases(woodiness_GIFT)) %>%
  filter(complete.cases(mycorrhiza_both))


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
  filter(complete.cases(woodiness_GIFT))


# add geographic traits ---------------------------------------------------

trait_example_df <- trait_example_df %>%
  left_join(select(native_centroid_df, species, lat_centroid), by = "species") %>%
  left_join(select(native_range_df, species, range_both), by = "species") %>%
  select(!c(max_height_GIFT, mean_seedmass_GIFT, min_seedmass_GIFT, max_seedmass_GIFT, dispersal_GIFT, nitrogen_GIFT)) %>%
  na.omit()



# add results from niche comparison ---------------------------------------




save(trait_example_df, file = "data/trait_analysis/trait_example_df_spec.RData")

