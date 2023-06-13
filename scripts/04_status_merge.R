library(tidyverse)
library(sf)

rm(list = ls())


# required data -----------------------------------------------------------

# status information from the different sources
load("data/testing/occ_GIFT_status.RData")
load("data/testing/occ_WCVP_status.RData")
load("data/testing/GloNAF_status.RData") # not merged with occurrences



# combine status dfs -----------------------------------------------------

# add prefixes to column names so that it can later be distinguished where the information came from
colnames(occ_wcvp_status)[c(3:9)] <- paste("wcvp", colnames(occ_wcvp_status)[c(3:9)], sep = "_")
colnames(occ_GIFT_status)[c(3:8)] <- paste("gift", colnames(occ_GIFT_status)[c(3:8)], sep = "_")
colnames(glonaf_status_info)[c(4:6)] <- paste("glonaf", colnames(glonaf_status_info)[c(4:6)], sep = "_")

# prepare main status df
occ_status_all <- merge(occ_wcvp_status, occ_GIFT_status, by = c("occ_id", "species")) %>% # join the information for WCVP and GIFT together
  arrange(occ_id) %>%
  select(-c(wcvp_LEVEL2_COD, wcvp_LEVEL1_COD, wcvp_geometry, gift_polygon_source, gift_GIFT_species)) %>% # remove columns that are not required for the next step
  rename(wcvp_status = wcvp_occurrence_type) %>%
  add_column(glonaf_status = NA, final_status = NA) %>% # add empty column for glonaf status and the final status
  relocate(wcvp_status, gift_status, glonaf_status, final_status, .after = last_col())


# identify unique combinations of species and level 3
unique_sr <- unique(occ_status_all[c("species", "wcvp_LEVEL3_COD")]) %>%
  arrange(wcvp_LEVEL3_COD) %>%
  drop_na()


for (i in 1:nrow(unique_sr)) {
  
  specs <- unique_sr[i,1]
  region <- unique_sr[i,2]
  
  print(c(specs, region))
  
  # get glonaf status for this combination
  status <- glonaf_status_info %>%
    filter(glonaf_tdwg3 == region & (species_orig == specs | species_changed == specs | species_no_x == specs | glonaf_standardized_name == specs)) %>%
    distinct() %>%
    pull(glonaf_status) 
  
  if (length(status) != 0) {
    occ_status_all[which(occ_status_all$species == specs & occ_status_all$wcvp_LEVEL3_COD == region), "glonaf_status"] <- status
  } # end of if condition
  
} # end  for loop


# clean up 
rm(list = setdiff(ls(), "occ_status_all"))

# TODO remove later on:
save(occ_status_all, file = "data/testing/occ_status_all.RData")

# check for conflicts -----------------------------------------------------

occ_status_all <- occ_status_all[1:10,]

# 1. assign final status if sources are in agreement

