
#' #############################################################################
#' PROJECT: NichePac - Objective 1: Niche Comparison
#' CONTENTS: 
#'  - preparation of the initial species list
#'  - data required to run this script: species list from Wohlwend et al. 2021
#'  
#' AUTHOR: Anna Rönnfeldt
#' DATE:   May 2023
#' #############################################################################

library(conflicted)
library(lcvplants)
library(tidyverse)

rm(list = ls())


# paths -------------------------------------------------------------------

path_home <- file.path("M:","C1")

# load data ---------------------------------------------------------------
species_list <- read.csv(file.path(path_home, "data", "wohlwend_species.csv"), header = TRUE) %>%
  pull(name_lcvp) 

species_list <- gsub(".", " ", species_list, fixed = TRUE)


# subset for testing
specs <- species_list[1:5]

# check corresponding lcvp names -----------------------------------------------

# the names used by Wohlwend et al. 2021 should already correspond to lcvp names, but they do not include Author names


# prepare empty df to store info
species_names <- data.frame(wohlwend_name = character(),
                            lcvp_name = character(),
                            lcvp_name_status = character(),
                            stringsAsFactors = FALSE)



for (spec in specs) {
  
  # get all alternative lcvp species names
  lcvp_output <- lcvp_fuzzy_search(spec, status = "accepted", "synonym")[,c("Output.Taxon", "Status")]
  
  # bind results
  species_names <- rbind(species_names,
                         data.frame(wohlwend_name = spec,
                                    lcvp_name = lcvp_output[,"Output.Taxon"],
                                    lcvp_name_status = lcvp_output[,"Status"]))
} # end of for loop over specs



