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

load("data/testing/occ_status_all.RData")

occ_status_all <- occ_status_all[sample(10),]

# 1. assign final status if sources are in agreement

t <- occ_status_all %>%
  mutate(final_status = case_when(
    
    # ID 1
    wcvp_status == "native" & gift_status == "native" & is.na(glonaf_status) ~ "native",
    
    # ID 2
    wcvp_status == "native" & gift_status == "native" & glonaf_status == "naturalized" ~ "Glonaf_vs_rest",
    
    # ID 3
    wcvp_status == "native" & gift_status == "native" & glonaf_status == "alien" ~ "Glonaf_vs_rest",
    
    # ID 4
    wcvp_status == "native" & gift_status == "non-native" & is.na(glonaf_status) ~ "W_vs_G_no_Gl",
    
    # ID 5
    wcvp_status == "native" & gift_status == "non-native" & glonaf_status == "naturalized" ~ "wcvp_vs_rest",
    
    # ID 6 
    wcvp_status == "native" & gift_status == "non-native" & glonaf_status == "alien" ~ "wcvp_vs_rest",
    
    # ID 7
    wcvp_status == "native" & gift_status == "naturalized" & is.na(glonaf_status) ~ "W_vs_G_no_Gl",
    
    # ID 8
    wcvp_status == "native" & gift_status == "naturalized" & glonaf_status == "naturalized" ~ "wcvp_vs_rest",
    
    # ID 9
    wcvp_status == "native" & gift_status == "naturalized" & glonaf_status == "alien" ~ "wcvp_vs_rest",
    
    # ID 10 
    wcvp_status == "native" & is.na(gift_status) & is.na(glonaf_status) ~ "native",
    
    # ID 11
    wcvp_status == "native" & is.na(gift_status) & glonaf_status == "naturalized" ~ "W_vs_Gl_no_G",
    
    # ID 12
    wcvp_status == "native" & is.na(gift_status) & glonaf_status == "alien" ~ "W_vs_Gl_no_G",
    
    # ID 13
    wcvp_status == "introduced" & gift_status == "native" & is.na(glonaf_status) ~ "W_vs_G_no_Gl",
    
    # ID 14 
    wcvp_status == "introduced" & gift_status == "native" & glonaf_status == "naturalized" ~ "Gift_vs_rest",
    
    # ID 15 
    wcvp_status == "introduced" & gift_status == "native" & glonaf_status == "alien" ~ "Gift_vs_rest",
    
    # ID 16
    wcvp_status == "introduced" & gift_status == "non-native" & is.na(glonaf_status) ~ "introduced",
    
    # ID 17 
    wcvp_status == "introduced" & gift_status == "non-native" & glonaf_status == "naturalized" ~ "introduced",
    
    # ID 18 
    wcvp_status == "introduced" & gift_status == "non-native" & glonaf_status == "alien" ~ "introduced",
    
    # ID 19
    wcvp_status == "introduced" & gift_status == "naturalized" & is.na(glonaf_status) ~ "introduced",
    
    # ID 20
    wcvp_status == "introduced" & gift_status == "naturalized" & glonaf_status == "naturalized" ~ "introduced",
    
    # ID 21
    wcvp_status == "introduced" & gift_status == "naturalized" & glonaf_status == "alien" ~ "introduced",
    
    # ID 22
    wcvp_status == "introduced" & is.na(gift_status) & is.na(glonaf_status) ~ "introduced",
    
    # ID 23
    wcvp_status == "introduced" & is.na(gift_status) & glonaf_status == "naturalized" ~ "introduced",
    
    # ID 24
    wcvp_status == "introduced" & is.na(gift_status) & glonaf_status == "alien" ~ "introduced",
    
    # ID 25
    is.na(wcvp_status) & gift_status == "native" & is.na(glonaf_status) ~ "native",
    
    # ID 26
    is.na(wcvp_status) & gift_status == "native" & glonaf_status == "naturalized" ~ "G_vs_Gl_no_W",
    
    # ID 27
    is.na(wcvp_status) & gift_status == "native" & glonaf_status == "alien" ~ "G_vs_Gl_no_W",
    
    # ID 28
    is.na(wcvp_status) & gift_status == "non-native" & is.na(glonaf_status) ~ "introduced",
    
    # ID 29
    is.na(wcvp_status) & gift_status == "non-native" & glonaf_status == "naturalized" ~ "introduced",
    
    # ID 30
    is.na(wcvp_status) & gift_status == "non-native" & glonaf_status == "alien" ~ "introduced",
    
  ))
