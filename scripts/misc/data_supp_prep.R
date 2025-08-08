library(dplyr)
library(openxlsx)
library(purrr)
library(readr)
library(tidyr)


load("results/ecospat/master_results_AC.RData") # results niche comparison
load("data/trait_data/input_TA_unstand.RData") # trait data 
load("data/native_regions/native_region_IDs.RData") # native range IDs
load("data/native_regions/spp_zones_all.RData") # native main climate zones


sup_data <- master_results_AC %>% 
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(stand_unfilling = rel_unfilling / total_esu) %>%
  mutate(stand_stability = rel_stability / total_esu) %>%
  mutate(stand_expansion = rel_expansion / total_esu) %>%
  left_join(select(spp_zones_all, species, freq_climate), join_by("species")) %>% 
  left_join(native_tags, join_by("species")) %>% 
  select(species, region, tag, freq_climate,
         p_D_shift, p_D_cons, similarity, 
         stand_unfilling, stand_stability, stand_expansion,
         unfilling, stability, expansion, 
         rel_abandonment, rel_unfilling, rel_stability, rel_expansion, rel_pioneering) %>% 
  # join with trait data
  left_join(select(input_TA, species, region, mean_height, mean_seedmass, growth_form, lifecycle, range_size_nat, 
                   niche_breadth_nat, niche_centroid_a_nat, niche_centroid_b_nat, years_since_intro, lat_dist),
            join_by("species", "region")) %>% 
  rename("ecospat_unfilling" = "unfilling",
         "ecospat_stability" = "stability",
         "ecospat_expansion" = "expansion",
         "non_native_region" = "region",
         "native_region_ID" = "tag",
         "main_native_climate" = "freq_climate") %>% 
  mutate(across(where(is.numeric), round, 4))



# change region labels to long form ---------------------------------------

sup_data$non_native_region <- as.character(sup_data$non_native_region)

sup_data[sup_data == "atr"] <- "tropical_asia"
sup_data[sup_data == "nam"] <- "northern_america"
sup_data[sup_data == "pac"] <- "pacific_islands"
sup_data[sup_data == "afr"] <- "africa"
sup_data[sup_data == "ate"] <- "temperate_asia"
sup_data[sup_data == "aus"] <- "australasia"
sup_data[sup_data == "sam"] <- "southern_america"
sup_data[sup_data == "eur"] <- "europe"


# save as csv
write.csv(sup_data, "results/supplementary_data.csv", row.names = FALSE)


# noticeable species -------------------------------------------------------

load("results/ecospat/master_results_AC.RData")

ID <- c(1:316)

df_con <- master_results_AC %>% 
  group_by(species, similarity) %>% 
  tally() %>% 
  pivot_wider(names_from=similarity, values_from=n) %>% 
  mutate(across(everything(), .fns=~replace_na(., 0))) %>% 
  mutate(total = conservatism + neither) %>%
  mutate(conservatism = conservatism * (-1)) %>% 
  mutate(perc_con = (round(100/total*abs(conservatism), 2))*(-1)) %>% 
  mutate(perc_neither = round(100/total*neither, 2)) 

df_con <- data.frame(species_ID = ID, df_con)

# niche conservatism in all regions
set_1 <- df_con %>%
  filter(perc_neither == 0) %>% 
  arrange(-perc_con) %>% 
  mutate(set = as.factor(1)) %>% 
  select(species) %>% 
  mutate(noticeable_feature = "100_perc_regional_conservatism")

# niche conservatism in none of the regions
set_2 <- df_con %>%
  mutate(perc_con = perc_con * (-1)) %>% 
  filter(perc_con == 0) %>% 
  arrange(-perc_neither) %>% 
  mutate(perc_con = perc_con * (-1)) %>% 
  mutate(set = as.factor(5)) %>% 
  select(species) %>% 
  mutate(noticeable_feature = "0_perc_regional_conservatism")

# >50 % expansion in at least one regional introductions

set_3 <- sup_data %>% 
  select(species, non_native_region, stand_expansion) %>% 
  filter(stand_expansion >= 0.5) %>% 
  select(species) %>% 
  mutate(noticeable_feature = "over_50_perc_expansion")

spp_noticeable <- rbind(set_1,set_2, set_3)

write.csv(spp_noticeable, "results/spp_noticeable.csv", row.names = FALSE)


