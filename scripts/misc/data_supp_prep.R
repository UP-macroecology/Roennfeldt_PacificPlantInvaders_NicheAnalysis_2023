library(dplyr)

load("results/ecospat/master_results_AC.RData") # results niche comparison
load("data/trait_analysis/input_TA_unstand.RData") # trait data 
load("data/native_region_IDs.RData") # native range IDs
load("data/climate_zones/spp_zones_all.RData") # native main climate zones

names(master_results_AC) 

# species, native_region_ID, main_native_climate, non_native_region, overlap, similarity, p_D_shift, p_D_cons, ecospat_ESU, AUSEP, stand_ESU, traits

sup_data <- master_results_AC %>% 
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(stand_unfilling = rel_unfilling / total_esu) %>%
  mutate(stand_stability = rel_stability / total_esu) %>%
  mutate(stand_expansion = rel_expansion / total_esu) %>%
  select(species, region, 
         p_D_shift, p_D_cons, similarity, 
         unfilling, stability, expansion, 
         rel_abandonment, rel_unfilling, rel_stability, rel_expansion, rel_pioneering,
         stand_unfilling, stand_stability, stand_expansion) %>% 
  # join with trait data
  left_join(select(input_TA, species, region, mean_height, mean_seedmass, growth_form, lifecycle, range_size_nat, 
                   niche_breadth_nat, niche_centroid_a_nat, niche_centroid_b_nat, years_since_intro, lat_dist),
            join_by("species", "region")) %>% 
  left_join(native_tags, join_by("species")) %>% 
  left_join(select(spp_zones_all, species, freq_climate), join_by("species")) %>% 
  rename("non_native_region" = "region",
         "native_region_ID" = "tag",
         "main_native_climate" = "freq_climate") 



# add traits

# names(sup_data)


sup_data$non_native_region <- as.character(sup_data$non_native_region)

sup_data[sup_data == "atr"] <- "tropical_asia"
sup_data[sup_data == "nam"] <- "northern_america"
sup_data[sup_data == "pac"] <- "pacific_islands"
sup_data[sup_data == "afr"] <- "africa"
sup_data[sup_data == "ate"] <- "temperate_asia"
sup_data[sup_data == "aus"] <- "australasia"
sup_data[sup_data == "sam"] <- "southern_america"
sup_data[sup_data == "eur"] <- "europe"

unique(sup_data$non_native_region)


str(sup_data)

# save as csv
write.csv(sup_data, "results/supplementary_data.csv", row.names = FALSE)

