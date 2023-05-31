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
  rename("PaciFlora" = "Species")# %>%
  #slice_head(n = 3)

# small <- species_names[1:3,]

# Paciflora names for hybrids are in a format that is problematic for working with the POWO website 



# species <- c("Achillea millefolium", "Sphagneticola trilobata")
w <- wcvp_match_names(species_names, wcvp_names, name_col = "PaciFlora") %>%
  select(PaciFlora, wcvp_name, wcvp_status, wcvp_ipni_id) %>%
  filter(wcvp_status == "Accepted" | wcvp_status == "Synonym") %>% # only go with these types of names
  mutate(mismatch = ifelse(as.character(PaciFlora) == as.character(wcvp_name),1,0)) # this checks whether PaciFlora and WCVP match (they should)

w_mismatch <- w %>%
  filter(mismatch == 0) # Paciflora and WCVP did not match if == 0



# find species for which more than one wcvp_name was returned
w_dupli <- w %>%
  group_by(PaciFlora) %>%
  filter(n()>1) %>%
  distinct(PaciFlora) %>%
  pull(PaciFlora)



# WCVP status -------------------------------------------------------------

specs <- sample(species_names$PaciFlora, 5)
# "Plantago australis" 
# "Buchnera americana" 
# "Cyperus odoratus"   
# "Ixophorus unisetus" 
# "Rumex chrysocarpus"


for (spec in specs) {
  distribution <- wcvp_distribution(taxon = "Achillea millefolium", taxon_rank = "species",
                                    location_doubtful = FALSE, extinct = FALSE)
  
  rbind
} # for loop over specs

t <- rWCVP::wcvp_distribution(taxon = "Achillea millefolium", taxon_rank = "species",
                      location_doubtful = FALSE, extinct = FALSE)

t2 <- t %>%
  mutate(species = specs[1]) %>%
  relocate(species)



# ---------------------------------------------------------------------------


t <- lcvp_search("Achillea millefolium", show_correct = TRUE, progress_bar = TRUE)
t_fuzzy <- lcvp_fuzzy_search("Achillea millefolium")



# the output of lcvp_fuzzy_search often puts an extra space at the end of the string. 
# remove the space to avoid mismatches with other names just because of the empty space
name2 <- str_trim(name, "right")




