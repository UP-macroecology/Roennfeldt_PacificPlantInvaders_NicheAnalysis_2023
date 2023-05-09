library(dplyr)

rm(list = ls())

# required data
load("results/intermediate/powo_page_inf.RData")

# -------------------------------------------- #
#     Get GloNAF status information         ####
# -------------------------------------------- #

# GloNAF = Global Naturalized Alien Flora: https://glonaf.org/
# GloNAF contains status information based on TDWG regions, as POWO,
# status information is either "alien" or "naturalized"


# reading the taxon csv-file downloaded from the website didn't work for me due to encoding problems,
# converted csv beforehand to UTF8 encoding using notepad++:
species_dt <- read.delim(file = "data/GloNAF/Taxon_x_List_GloNAF_vanKleunenetal2018Ecology_UTF8.csv")
region_dt <- read.delim(file = "data/GloNAF/Region_GloNAF_vanKleunenetal2018Ecology.csv", sep =",")
list_dt <- read.delim(file = "data/GloNAF/List_GloNAF_vanKleunenetal2018Ecology.csv", sep = ",")

glonaf_dt <- species_dt %>%
  left_join(region_dt) %>%
  left_join(list_dt)

glonaf_dt$species_name <- paste(glonaf_dt$standardized_name, glonaf_dt$author)

# free memory:
rm(species_dt)
rm(region_dt)
rm(list_dt)

# find species names in GloNAF that correspond to target species
# (either GloNAF tpl_input or standardized name + author must match either LCVP name or POWO name)
# then join LCVP and POWO names
glonaf_blacklist_dt <- glonaf_dt %>%
  filter(tpl_input %in% powo_page_inf$lcvp_name | # name before GloNAF standardization
           species_name %in% powo_page_inf$lcvp_name | # name after GloNAF standardization
           tpl_input %in% powo_page_inf$powo_name |
           species_name %in% powo_page_inf$powo_name) %>%
  left_join(powo_page_inf[,c("lcvp_name", "powo_name")], by = c("tpl_input" = "lcvp_name"), keep = TRUE) %>%
  distinct %>%
  left_join(powo_page_inf[,c("lcvp_name", "powo_name")], by = c("tpl_input" = "powo_name"), keep = TRUE) %>%
  distinct %>%
  left_join(powo_page_inf[,c("lcvp_name", "powo_name")], by = c("species_name" = "lcvp_name"), keep = TRUE) %>%
  distinct %>%
  left_join(powo_page_inf[,c("lcvp_name", "powo_name")], by = c("species_name" = "powo_name"), keep = TRUE) %>%
  distinct %>%
  mutate(lcvp_name = coalesce(lcvp_name.x, lcvp_name.y, lcvp_name.x.x, lcvp_name.y.y)) %>%
  mutate(powo_name = coalesce(powo_name.x, powo_name.y, powo_name.x.x, powo_name.y.y)) %>%
  select(-c(lcvp_name.x, lcvp_name.y, lcvp_name.x.x, lcvp_name.y.y, powo_name.x, powo_name.y, powo_name.x.x, powo_name.y.y)) %>%
  filter(cultivated_included == 0 | cultivated_included == 3) # 0 & 3: cultivated taxa unlikely to be included (for 2 different reasons), 1 = cultivated may be included, 2 = cultivated likely to be included
# casuals could also be excluded (= alien taxa that sometimes occur in the wild but have no persistent populations)

save(glonaf_blacklist_dt, file = "results/intermediate/glonaf_blacklist_dt.RData")
