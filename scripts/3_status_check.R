library(GIFT)
library(lcvplants)
library(tidyverse)
DIFFLIB <- reticulate::import("difflib") # load Python module


rm(list = ls())

# read in data set on pacific island invaders. Source: https://bdj.pensoft.net/article/67318/
PaciFlora_specs <- read.table("data/PaciFLora.txt", header = TRUE) %>%
  arrange(Species) %>%
  dplyr::select(Species) %>%
  distinct() %>%
  drop_na() %>%
  pull(Species) 


spec <- PaciFlora_specs[56] # Achillea millefolium



getGiftNames <- function(spec, incl_lcvp_synonyms = FALSE){
  
  # searches for species name in GIFT dataset, checks species name against LCVP (on which blacklist is based),
  # from GIFT extracts WCVP harmonized species name (same taxonomic backbone as in POWO) if species is also included in LCVP
  
  # input:  
  #   - spec: LCVP based species name
  #   - incl_lcvp_synonyms: TRUE = LCVP search results include names that are considered synonyms in LCVP;
  #                         FALSE = only accepted names in LCVP are considered
  # output: 
  #   - df: searched name - GIFT result genus - GIFT result species epithet
  
  print(spec)
  
  # split name in genus and species epithet:
  spec_gen_epi <- unlist(str_split(spec, pattern = " ", n = 2))
  
  # find searched species name in GIFT:
  GIFT_spec_res <- tryCatch({
    
    GIFT_species_lookup(genus = spec_gen_epi[1], 
                        epithet = spec_gen_epi[2], 
                        namesmatched = TRUE, # TRUE = look for the species not only in the standardized names but also in the original names
                        GIFT_version = "beta")
  },
  error = function(e){
    print(paste("Connection to Gift information failed, try again later."))
    return(data.frame("searched_name" = spec,
                      "GIFT_genus" = NA,
                      "GIFT_species_ep" = NA))})
  
  # GIFT results: species names incl. author before harmonization:
  GIFT_spec_org <- paste(GIFT_spec_res$genus, GIFT_spec_res$species_epithet, GIFT_spec_res$author)
  
  # check against LCVP:
  
  # search LCVP entries connected to the searched species name:
  if(incl_lcvp_synonyms){
    status_ok <- c("accepted", "synonym") # exclude unresolved and external results
  } else {status_ok <- "accepted"} # only accepted species names
  lcvp_spec_res <- unique(lcvp_fuzzy_search(spec, status = status_ok)$Output.Taxon) # lcvp_fuzzy_search from package lcvpplants
  
  # GIFT WCVP harmonized names (column work_species) matching LCVP results:
  GIFT_res_harm <- GIFT_spec_res$work_species[which(GIFT_spec_org %in% lcvp_spec_res)]
  
  # there is no exact match (e.g. due to slightly different names; 6 species of the 122 blacklist species)
  if(length(GIFT_res_harm) == 0){
    
    # find most similar name with fuzzy matching:
    print("No exact match between Gift and LCVP name found. Used fuzzy matching instead. Consider checking the results manually.")
    best_match <- DIFFLIB$get_close_matches(word = lcvp_spec_res, 
                                            possibilities = GIFT_spec_org,
                                            n = as.integer(1), cutoff = 0)
    print(paste("LCVP name:", lcvp_spec_res))
    print(paste("Matched Gift name:", best_match))
    GIFT_res_harm <- GIFT_spec_res$work_species[which(GIFT_spec_org == best_match)]
  }
  
  
#  grep(". $", ,v=T)
  
  
  
t2 <-   
  
  
  # split in genus and species epithet:
  GIFT_harm_gen_epi <- unlist(str_split(GIFT_res_harm, pattern = " ", n = 2))
  
  if(length(GIFT_harm_gen_epi) != 0){
    return(data.frame("searched_name" = spec,
                      "GIFT_genus" = GIFT_harm_gen_epi[1],
                      "GIFT_species_ep" = GIFT_harm_gen_epi[2]))
  }else{
    print("Matching GIFT and LCVP name didn't work. Check manually.")
    return(data.frame("searched_name" = spec,
                      "GIFT_genus" = NA,
                      "GIFT_species_ep" = NA))
  }
}

getGiftNames(spec)
