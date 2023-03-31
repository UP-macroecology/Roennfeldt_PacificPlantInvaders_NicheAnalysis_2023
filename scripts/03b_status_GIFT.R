
rm(list = ls())

# make functions defined in utils.R available;
# these are:
# - getPowoNames()
# - getPowoStatus()
# - checkPowoStatus()
# - adaptPowoStatus()
# - getGiftNames()
# - getGiftStatusInf()

source("scripts/utils.R")


# required data ----------------------------------------------------------------
load("results/intermediate/species_names.RData")


# get GIFT status info ----


# GIFT uses partly other spatial entities than POWO (not only TDWG regions,
# but also administrative and custom polygons)


# find species names in GIFT that correspond to target species (getGiftNames defined in utils_1-5_dataprep.R)
# I'm again doing it in a loop to have more control 


# # prepare empty df to store info
# GIFT_names <- data.frame(searched_name = character(),
#                          GIFT_genus = character(),
#                          GIFT_species_ep = character(),
#                          stringsAsFactors = FALSE)


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
  if (counter >= 200){
    break
  } # end of if 
  
} # end of for loop over specs_left

save(GIFT_names, file = "results/intermediate/GIFT_names.RData")

# 33 species with NAs in the GIFT name object (I only noticed after running the next step, which took ages, so I will remove them manually...)

# 2) extract status information from GIFT:
# (getGiftStatusInf defined in utils_1-5_dataprep.R)
GIFT_status <- vector("list", length = nrow(GIFT_names))
for (s in 1:nrow(GIFT_names)) {
  print(s)
  GIFT_status[[s]] <- getGiftStatusInf(searched_name = GIFT_names$searched_name[s],
                                       GIFT_spec_genus = GIFT_names$GIFT_genus[s],
                                       GIFT_spec_epithet = GIFT_names$GIFT_species_ep[s])
  }

# several species had NA in the GIFT_names object and will be removed from GIFT status to avoid errors when binding the rows
# GIFT_status <- GIFT_status[-2695]
# save(GIFT_status, file ="results/intermediate/GIFT_status.RData")


GIFT_status_df <- bind_rows(GIFT_status)

save(GIFT_status_df, file = "results/intermediate/GIFT_status_df_nested_regions.RData")

# free up memory 
# rm(GIFT_status)

# 3) load spatial data of the GIFT regions with status information for the considered species:
# original:
GIFT_polygons <- GIFT_shape(unique(GIFT_status_df$entity_ID), GIFT_version = "beta")

save(GIFT_polygons, file = "results/intermediate/Gift_polygons_nested_regions.RData")



 