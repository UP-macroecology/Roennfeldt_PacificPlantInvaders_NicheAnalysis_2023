#' ---------------------------
#
# Purpose of script:
# Author: Valén Holle
# Date Created: 2024-05
# Email: roennfeldt@uni-potsdam.de
#
# Notes: the initial csv file is a modified version of the Seebens data: 
# Non-numeric entries such as "mid 20 th century" where modified by hand. See publication methods.
#
#' ---------------------------



# load needed packages
library(dplyr)
library(terra)
library(sf)
library(stringr)


# required data
data_seebens <- read.csv("data/Seebens_year_intro_rev.csv") # year of first introduction based on Seebens 
load("data/spp_suitable_after_thinning.RData.RData") # data frame containing study species names
tdwg_4 <- st_read("data/spatial_data/level4.geojson") # tdwg regions level 4
tdwg_3 <- st_read("data/spatial_data/level3.geojson") # tdwg regions level 3
tdwg_2 <- st_read("data/spatial_data/level2.geojson") # tdwg regions level 2



# preparations -----------------------------------------------------------------

# create a vector containing all reference regions
regions_reference <- c("pac", "eur", "afr", "ate", "atr", "aus", "nam", "sam")

# create a vector containing all study species names
study_species <- spp_pre_thinning

# check if study species are mentioned in data collection from Seebens
species_seebens <- unique(data_seebens$TaxonName)
species_both <- spp_pre_thinning %in% species_seebens # contains all of them

# create a subset from Seebens' database to solely containing information of the study species
subset_data_seebens <- data_seebens[data_seebens$TaxonName %in% spp_pre_thinning, ]

# take a look at the regions Seebens et al. covered
seebens_regions <- unique(subset_data_seebens$Region) # 155 different regions

# extract names of the regions contained in the different tdwg levels
tdwg_4_regions <- unique(tdwg_4$Level_4_Na)
tdwg_3_regions <- unique(tdwg_3$LEVEL3_NAM)
tdwg_2_regions <- unique(tdwg_2$LEVEL2_NAM)

# create a new column in data frame from Seebens to fill in the respective tdwg level 1 regions
subset_data_seebens$Region_lv_1 <- NA



# extract corresponding level one regions --------------------------------------

# we loop over each of the studied regions by Seebens et al. and try, if available,
# to match this region with our tdwg level one reference regions using the
# tdwg regions of other levels as information transfer

for (sr in seebens_regions) { # start of the loop over all regions studied by Seebens
  
  contains_4 <- sr %in% tdwg_4_regions # check if defined tdwg level 4 regions match with the covered regions from Seebens
  
  if (contains_4 == TRUE) {
    
    
    level_1 <- as.numeric(tdwg_4$Level1_cod[tdwg_4$Level_4_Na == sr]) # if that is the case, we extract the number of the 
                                                                      # corresponding level one tdwg region
    
    # for each entry of the Seebens data stemming from that region, the tdwg level 1 region is inserted in the 
    # corresponding column of the data frame
    if (isTRUE(level_1 == 1)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "eur"
    next
    
    } else if (isTRUE(level_1 == 2)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "afr"
    next
    
    } else if (isTRUE(level_1 == 3)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "ate"
    next
    
    } else if (isTRUE(level_1 == 4)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "atr"
    next
    
    } else if (isTRUE(level_1 == 5)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "aus"
    next
    
    } else if (isTRUE(level_1 == 6)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "pac"
    next
    
    } else if (isTRUE(level_1 == 7)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "nam"
    next
    
    } else if (isTRUE(level_1 == 8)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "sam"
    }} 
    
    else if (contains_4 == FALSE) { contains_3 <- sr %in% tdwg_3_regions # if none of the regions studied by Seebens
    }                                                                    # matches with the tdwg level 4 regions, check the
                                                                         # next tdwg level region
    
    if (contains_3 == TRUE) { 
      
      level_1 <- as.numeric(tdwg_3$LEVEL1_COD[tdwg_3$LEVEL3_NAM == sr]) # we extract the number of the corresponding level one 
                                                                        # tdwg region for applicable regions
      
      # for each entry of the Seebens data stemming from that region, the tdwg level 1 region is inserted in the 
      # corresponding column of the data frame
      if (isTRUE(level_1 == 1)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "eur"
      next
      
      } else if (isTRUE(level_1 == 2)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "afr"
      next
      
      } else if (isTRUE(level_1 == 3)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "ate"
      next
      
      } else if (isTRUE(level_1 == 4)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "atr"
      next
      
      } else if (isTRUE(level_1 == 5)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "aus"
      next
      
      } else if (isTRUE(level_1 == 6)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "pac"
      next
      
      } else if (isTRUE(level_1 == 7)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "nam"
      next
      
      } else if (isTRUE(level_1 == 8)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "sam"
      }}
      
      else if (contains_3 == FALSE) { contains_2 <- sr %in% tdwg_2_regions # if none of the regions studied by Seebens
      }                                                                    # matches with the tdwg level 3 regions, check the
                                                                           # next tdwg level region
  
      if (contains_2 == TRUE) { 
        
        level_1 <- as.numeric(tdwg_2$LEVEL1_COD[tdwg_2$LEVEL2_NAM == sr]) # we extract the number of the corresponding level one 
                                                                          # tdwg region for applicable regions
        
        # for each entry of the Seebens data stemming from that region, the tdwg level 1 region is inserted in the 
        # corresponding column of the data frame
        if (isTRUE(level_1 == 1)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "eur"
        next
        
        } else if (isTRUE(level_1 == 2)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "afr"
        next
        
        } else if (isTRUE(level_1 == 3)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "ate"
        next
        
        } else if (isTRUE(level_1 == 4)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "atr"
        next
        
        } else if (isTRUE(level_1 == 5)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "aus"
        next
        
        } else if (isTRUE(level_1 == 6)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "pac"
        next
        
        } else if (isTRUE(level_1 == 7)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "nam"
        next
        
        } else if (isTRUE(level_1 == 8)) { subset_data_seebens[subset_data_seebens$Region == sr, "Region_lv_1"] <- "sam"
        next
        
        }} else { print(sr) # print the regions studied by Seebens et al., for which no corresponding tdwg level 1 region was found
        }                   # using the tdwg regions of other levels
  
} # end of the loop over all regions studied by Seebens


# manually complete corresponding level one regions ----------------------------

# the printed regions studied by Seebens et al., for which no corresponding reference region 
# was found via the other tdwg level regions, were looked up and manually assigned
# to the reference regions

subset_data_seebens[subset_data_seebens$Region == "Anguilla", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Antigua and Barbuda", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Argentina", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Aruba", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Bahamas", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Balearic Islands", "Region_lv_1"] <- "eur"
subset_data_seebens[subset_data_seebens$Region == "Barbados", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Bermuda", "Region_lv_1"] <- "nam"
subset_data_seebens[subset_data_seebens$Region == "Bolivia", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Bosnia and Herzegovina", "Region_lv_1"] <- "eur"
subset_data_seebens[subset_data_seebens$Region == "British Virgin Islands", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Campbell", "Region_lv_1"] <- "nam"
subset_data_seebens[subset_data_seebens$Region == "Canada", "Region_lv_1"] <- "nam"
subset_data_seebens[subset_data_seebens$Region == "Canary Islands", "Region_lv_1"] <- "afr"
subset_data_seebens[subset_data_seebens$Region == "Chile", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Colombia", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Congo, Democratic Republic of the", "Region_lv_1"] <- "afr"
subset_data_seebens[subset_data_seebens$Region == "Costa Rica", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Cote D'Ivoire", "Region_lv_1"] <- "afr"
subset_data_seebens[subset_data_seebens$Region == "Cuba", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Curacao", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Dominica", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Dominican Republic", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Eswatini", "Region_lv_1"] <- "afr"
subset_data_seebens[subset_data_seebens$Region == "French Polynesia", "Region_lv_1"] <- "pac"
subset_data_seebens[subset_data_seebens$Region == "Galapagos", "Region_lv_1"] <- "pac"
subset_data_seebens[subset_data_seebens$Region == "Grenada", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Guadeloupe", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Haiti", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Hawaiian Islands", "Region_lv_1"] <- "pac"
subset_data_seebens[subset_data_seebens$Region == "Indonesia", "Region_lv_1"] <- "atr"
subset_data_seebens[subset_data_seebens$Region == "Iran, Islamic Republic of", "Region_lv_1"] <- "ate"
subset_data_seebens[subset_data_seebens$Region == "Jamaica", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Kermadec Islands", "Region_lv_1"] <- "pac"
subset_data_seebens[subset_data_seebens$Region == "Kyrgyzstan", "Region_lv_1"] <- "ate"
subset_data_seebens[subset_data_seebens$Region == "Lesser Sunda Islands", "Region_lv_1"] <- "atr"
subset_data_seebens[subset_data_seebens$Region == "Lord Howe Island", "Region_lv_1"] <- "pac"
subset_data_seebens[subset_data_seebens$Region == "Malaysia", "Region_lv_1"] <- "atr"
subset_data_seebens[subset_data_seebens$Region == "Martinique", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Montserrat", "Region_lv_1"] <- "eur"
subset_data_seebens[subset_data_seebens$Region == "Peru", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Puerto Rico", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Reunion", "Region_lv_1"] <- "afr"
subset_data_seebens[subset_data_seebens$Region == "Russia", "Region_lv_1"] <- "ate"
subset_data_seebens[subset_data_seebens$Region == "Saint Helena", "Region_lv_1"] <- "nam"
subset_data_seebens[subset_data_seebens$Region == "Saint Kitts and Nevis", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Saint Lucia", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Saint Pierre and Miquelon", "Region_lv_1"] <- "nam"
subset_data_seebens[subset_data_seebens$Region == "Saint Vincent and the Grenadines", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Sao Tome and Principe", "Region_lv_1"] <- "afr"
subset_data_seebens[subset_data_seebens$Region == "Slovakia", "Region_lv_1"] <- "eur"
subset_data_seebens[subset_data_seebens$Region == "Socotra Island", "Region_lv_1"] <- "afr"
subset_data_seebens[subset_data_seebens$Region == "South Africa", "Region_lv_1"] <- "afr"
subset_data_seebens[subset_data_seebens$Region == "Sumatra", "Region_lv_1"] <- "atr"
subset_data_seebens[subset_data_seebens$Region == "Svalbard and Jan Mayen", "Region_lv_1"] <- "eur"
subset_data_seebens[subset_data_seebens$Region == "Trinidad and Tobago", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "United Kingdom", "Region_lv_1"] <- "eur"
subset_data_seebens[subset_data_seebens$Region == "United States", "Region_lv_1"] <- "nam"
subset_data_seebens[subset_data_seebens$Region == "Venezuela", "Region_lv_1"] <- "sam"
subset_data_seebens[subset_data_seebens$Region == "Zanzibar Island", "Region_lv_1"] <- "afr"



# time since introduction ------------------------------------------------------

# find for each species, if available, the earliest time of introduction per 
# considered reference region.
# additionally, for the Pacific reference region, the specific region (island group) 
# where the earliest introduction was documented, is extracted.

# create a data frame to store the results
year_first_intro_Seebens <- data.frame(expand.grid(species=c(paste(study_species))), 
                                       pac=NA, eur=NA, afr=NA, ate=NA, atr=NA, aus=NA, nam=NA, sam=NA, pac_region=NA)


# Loop over all study species and the regions, in which they were introduced

for (sp in study_species) { # start of the loop over all study species
  
  print(sp)
  
  # find out, in which reference regions the species was introduced by listing all
  # intr files that exist for the current species
  files <- list.files(path = paste0("data/coords_final_intr/"), pattern = paste0("_",sp,".RData")) 
  
  # extract the regions of introduction
  regions <- sapply(files, function(x) { unlist(str_split(x, pattern = "_"))[5]
  })
  
  # extract the regions with no introduction
  regions_diff <- setdiff(regions_reference, regions)
  
  # Insert "NI", standing for not introduced, in the applicable cells of the data frame
  year_first_intro_Seebens[year_first_intro_Seebens$species == sp, regions_diff] <- "NI"
  
  # subset the data frame from Seebens to only contain information of the current species
  data_species <- subset(subset_data_seebens, subset_data_seebens$TaxonName == sp)
  

  for (r in regions) { # start of the loop over all regions with introduced occurrences
    
    data_species_region = NULL
    year_collect_min = NULL
    region_min = NULL
    
    # further subset the species introduction time information by region
    data_species_region <- subset(data_species, data_species$Region_lv_1 == r)
    
    if (isTRUE(nrow(data_species_region) >= 1)) {
      
    # if there is more than one entry for the reference region, we want to extract the earliest time
    year_collect_min <- min(as.numeric(data_species_region$FirstRecord, na.rm = TRUE)) 
    
    # calculate the years since first introduction (taking 2023 as reference year)
    year_since_intro <- 2023 - year_collect_min
    
    # insert the year since introduction into our results data frame
    year_first_intro_Seebens[year_first_intro_Seebens$species == sp, r] <- year_since_intro
    
    if (r == "pac") { # if there is information on the time since introduction in the Pacific region,
                      # extract the specific region based on Seebens et al. (mostly referring to island group)
      
      data_species_region_sub <- subset(data_species_region, data_species_region$FirstRecord == year_collect_min)
      
      region_min <- data_species_region_sub$Region[1]
      
      year_first_intro_Seebens[year_first_intro_Seebens$species == sp, "pac_region"] <- region_min
        
      }
      
    
    } else if (isTRUE(nrow(data_species_region) == 0)) { next
    }
    
  } # end of the loop over all regions with introduced occurrences
  
} # end of the loop over all study species


# Save the data frame containing the time since introduction for each species, if available,
# for each of the 8 different reference regions
save(year_first_intro_Seebens, file = "data/trait_analysis/year_first_intro_Seebens_rev.RData") 
