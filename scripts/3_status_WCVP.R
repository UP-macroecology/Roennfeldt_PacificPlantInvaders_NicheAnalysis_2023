library(foreach)
library(lcvplants)
library(tidyverse)
library(rWCVP) # required data: remotes::install_github('matildabrown/rWCVPdata')

rm(list = ls())

wcvp_names <- rWCVPdata::wcvp_names

# data set on pacific island invaders from: https://bdj.pensoft.net/article/67318/
species_names <- read.table("data/PaciFLora.txt", header = TRUE) %>%
  arrange(Species) %>%
  dplyr::select(Species) %>%
  distinct() %>%
  drop_na() %>%
  rename("PaciFlora" = "Species")


# Paciflora names for hybrids are in a format that causes errors when running the wcvp_distribution() function
hybrids <- species_names %>%
  filter(grepl('_x', PaciFlora)) %>%
  pull() 

# hybrids_corrected <- data.frame(PaciFlora = str_replace(hybrids, "_x", ""))

hybrids_corrected <- str_replace(hybrids, "_x", "")
hybrids_corrected <- str_replace(hybrids_corrected, " ", " x ") 


# remove hybrids with the old format and add the corrected format
species_names_corrected <- species_names %>%
  filter(!grepl('_x', PaciFlora)) %>% # remove old format
  rbind(data.frame(PaciFlora = hybrids_corrected)) %>% # add corected format
  arrange(PaciFlora) # sort alphabetically

save(species_names_corrected, file = "data/initial_species_list.RData")

# WCVP status -------------------------------------------------------------
set.seed(7)

specs <- sample(species_names$PaciFlora, 5)



species_distributions <- foreach(s = 1:length(specs), .packages = c("dplyr"), .combine = "rbind", .verbose = TRUE) %do% {
  
  distribution <- wcvp_distribution(taxon = specs[s], taxon_rank = "species",
                                    location_doubtful = FALSE, extinct = FALSE) %>%
    mutate(species = specs[1]) %>%
    relocate(species)
  
  distribution
  
} # end of foreach 





# ---------------------------------------------------------------------------


t <- lcvp_search("Achillea millefolium", show_correct = TRUE, progress_bar = TRUE)
t_fuzzy <- lcvp_fuzzy_search("Achillea millefolium")



# the output of lcvp_fuzzy_search often puts an extra space at the end of the string. 
# remove the space to avoid mismatches with other names just because of the empty space
name2 <- str_trim(name, "right")




