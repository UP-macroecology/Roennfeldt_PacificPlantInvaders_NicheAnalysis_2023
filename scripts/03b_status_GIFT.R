library(dplyr)
library(GIFT)

rm(list = ls())

# make functions defined in utils.R available;
# these are:
# - getPowoNames()
# - getPowoStatus()
# - checkPowoStatus()
# - adaptPowoStatus()
# - getGiftNames()
# - getGiftStatusInf()

source("scripts/functions.R")


# required data ----------------------------------------------------------------
load("data/specs_all.RData")


# get GIFT status info ----


# GIFT uses partly other spatial entities than POWO (not only TDWG regions,
# but also administrative and custom polygons)


# find species names in GIFT that correspond to target species (getGiftNames defined in utils_1-5_dataprep.R)
# I'm again doing it in a loop to have more control 


# prepare empty df to store info
# GIFT_names <- data.frame(searched_name = character(),
#                          GIFT_genus = character(),
#                          GIFT_species_ep = character(),
#                          stringsAsFactors = FALSE)

load("results/intermediate/GIFT_names.RData")

# define species for which powo page information has not yet been checked
specs_done <- unique(GIFT_names$searched_name)
specs_left <- setdiff(specs_all, specs_done)

counter <- 0

# run loop over species
for(spec in specs_left){
  
  counter <- counter + 1
  print(counter)
  
  GIFT_names <- bind_rows(GIFT_names,
                             getGiftNames(spec, incl_lcvp_synonyms = TRUE))
  
  # stop after xx species (e.g. 200)
  if (counter >= 136){
    break
  } # end of if 
  
} # end of for loop over specs_left

save(GIFT_names, file = "results/intermediate/GIFT_names.RData")

# remove NAs
GIFT_names_no_NA <- na.omit(GIFT_names)


# 2) extract status information from GIFT:
# (getGiftStatusInf defined in utils_1-5_dataprep.R)
GIFT_status <- vector("list", length = nrow(GIFT_names_no_NA))
for (s in 1:nrow(GIFT_names_no_NA)) {
  print(s)
  GIFT_status[[s]] <- getGiftStatusInf(searched_name = GIFT_names_no_NA$searched_name[s],
                                       GIFT_spec_genus = GIFT_names_no_NA$GIFT_genus[s],
                                       GIFT_spec_epithet = GIFT_names_no_NA$GIFT_species_ep[s])
  } # end of for loop 


# some species still had NAs and were be removed from GIFT status to avoid errors when binding the rows
# GIFT_status <- GIFT_status[-2695]


list_index <- NULL
for(s in 1:length(GIFT_status)){
  
  # collect indices of list element without GIFT information (empty df)
  if(nrow(GIFT_status[[s]]) == 0){list_index <- c(list_index, s)}
  
} # end of loop over list indices

# remove empty elements from list
GIFT_status <- GIFT_status[-list_index]


GIFT_status_df <- bind_rows(GIFT_status)

save(GIFT_status_df, file = "results/intermediate/GIFT_status_df_nested_regions.RData")

# free up memory 
rm(GIFT_status)
rm(list_index)
rm(s)
rm(specs_done)
rm(specs_left)


# 3) load spatial data of the GIFT regions with status information for the considered species:
# original:
GIFT_polygons <- GIFT_shapes(unique(GIFT_status_df$entity_ID), GIFT_version = "beta")

save(GIFT_polygons, file = "results/intermediate/Gift_polygons_nested_regions.RData")



 