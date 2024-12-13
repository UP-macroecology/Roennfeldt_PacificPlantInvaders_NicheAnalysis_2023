#' ---------------------------
#
# Purpose of script: download and extract trait data for the species contained 
# within the PaciFLora data set
# Author: Valén Holle
# Date Created: 2024-12-13
#
# Notes: here, more traits are downloaded then included in the analysis for the
# final publication. 
#
#' ---------------------------



# load and install needed packages
library(devtools)
install_version("GIFT", version = "1.0.0", repos = "http://cran.us.r-project.org")
library(GIFT)
library(dplyr)




# trait entries ----------------------------------------------------------------

# get an overview of available traits in the GIFT database
trait_meta <- GIFT_traits_meta()

# traits to retrieve: height, SSD (wood density), seed mass, mycorrhizal association,
# woodiness, growth form, lifecycle, lifespan, self fertilization, dispersal syndrome 1,
# dispersal syndrome 2, seeds per fruit, nitrogen fixation, elevational range, 
# habitat and defense. 

# extract all database entries related to these traits:
# trait values are downloaded as aggregated values on species level
# categorical traits take the most frequent entry
# continuous traits take the mean value of the different observations or the 
# minimum or maximum observation, depending on the considered trait

# height
mean_height <- GIFT_traits(trait_IDs = c("1.6.3"), agreement = 0.66,
                           bias_ref = FALSE, bias_deriv = FALSE)

min_height <- GIFT_traits(trait_IDs = c("1.6.1"), agreement = 0.66,
                          bias_ref = FALSE, bias_deriv = FALSE)

max_height <- GIFT_traits(trait_IDs = c("1.6.2"), agreement = 0.66,
                          bias_ref = FALSE, bias_deriv = FALSE)

# SSD
mean_SSD <- GIFT_traits(trait_IDs = c("4.3.3"), agreement = 0.66,
                        bias_ref = FALSE, bias_deriv = FALSE)

min_SSD <- GIFT_traits(trait_IDs = c("4.3.1"), agreement = 0.66,
                       bias_ref = FALSE, bias_deriv = FALSE)

max_SSD <- GIFT_traits(trait_IDs = c("4.3.2"), agreement = 0.66,
                       bias_ref = FALSE, bias_deriv = FALSE)

# seed mass
mean_seedmass <- GIFT_traits(trait_IDs = c("3.2.3"), agreement = 0.66,
                             bias_ref = FALSE, bias_deriv = FALSE)

min_seedmass <- GIFT_traits(trait_IDs = c("3.2.1"), agreement = 0.66,
                            bias_ref = FALSE, bias_deriv = FALSE)

max_seedmass <- GIFT_traits(trait_IDs = c("3.2.2"), agreement = 0.66,
                            bias_ref = FALSE, bias_deriv = FALSE)

# mycorrhizal association
mycorrhiza <- GIFT_traits(trait_IDs = c("6.4.1"), agreement = 0.66,
                          bias_ref = FALSE, bias_deriv = FALSE)

# woodiness 
woodiness <- GIFT_traits(trait_IDs = c("1.1.1"), agreement = 0.66,
                         bias_ref = FALSE, bias_deriv = FALSE)

# growth form
growth_form <- GIFT_traits(trait_IDs = c("1.2.1"), agreement = 0.66,
               bias_ref = FALSE, bias_deriv = FALSE)

# lifecycle
lifecycle <- GIFT_traits(trait_IDs = c("2.1.1"), agreement = 0.66,
                             bias_ref = FALSE, bias_deriv = FALSE)

# lifespan
lifespan <- GIFT_traits(trait_IDs = c("2.2.1"), agreement = 0.66,
                         bias_ref = FALSE, bias_deriv = FALSE)

# self fertilization
self_fertilization <- GIFT_traits(trait_IDs = c("3.1.1"), agreement = 0.66,
                                  bias_ref = FALSE, bias_deriv = FALSE)

# dispersal 1
dispersal_1 <- GIFT_traits(trait_IDs = c("3.3.1"), agreement = 0.66,
                         bias_ref = FALSE, bias_deriv = FALSE)

# dispersal 2
dispersal_2 <- GIFT_traits(trait_IDs = c("3.3.2"), agreement = 0.66,
                           bias_ref = FALSE, bias_deriv = FALSE)

# seeds per fruit
seeds <- GIFT_traits(trait_IDs = c("3.9.1"), agreement = 0.66,
                     bias_ref = FALSE, bias_deriv = FALSE)

# nitrogen fixation
nitrogen <- GIFT_traits(trait_IDs = c("4.5.1"), agreement = 0.66,
                        bias_ref = FALSE, bias_deriv = FALSE)

# elevational range
mean_elev_range <- GIFT_traits(trait_IDs = c("6.1.3"), agreement = 0.66,
                              bias_ref = FALSE, bias_deriv = FALSE)

min_elev_range <- GIFT_traits(trait_IDs = c("6.1.1"), agreement = 0.66,
                              bias_ref = FALSE, bias_deriv = FALSE)

max_elev_range <- GIFT_traits(trait_IDs = c("6.1.2"), agreement = 0.66,
                              bias_ref = FALSE, bias_deriv = FALSE)

# habitat
habitat <- GIFT_traits(trait_IDs = c("6.3.1"), agreement = 0.66,
                       bias_ref = FALSE, bias_deriv = FALSE)

# defense 
defense <- GIFT_traits(trait_IDs = c("6.5.1"), agreement = 0.66,
                       bias_ref = FALSE, bias_deriv = FALSE)




# extract traits of Pacific plant invaders -------------------------------------

# read in the data table containing all species names of the Pacific plant invaders
load("data/species_overview.RData") 

# concatenate the two columns containing the GIFT species name in the species overview
species_overview_GIFT <- species_overview
species_overview_GIFT$work_species <- paste(species_overview_GIFT$GIFT_genus, species_overview_GIFT$GIFT_species_ep)

# match the available trait information with the Pacific plant invaders based on
# their shared column containing the species name "work species"
# this is done for one trait after the other while keeping all species (all.x = TRUE)
# each column receives a name, explicitly stating the functional trait

# mean height
species_pacific_traits_GIFT_prep <- merge(species_overview_GIFT[, c(1,2,11)], mean_height[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[4] <- "mean_height_GIFT"

# min height
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, min_height[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[5] <- "min_height_GIFT"

# max height
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, max_height[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[6] <- "max_height_GIFT"

# mean SSD
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, mean_SSD[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[7] <- "mean_SSD_GIFT"

# min SSD
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, min_SSD[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[8] <- "min_SSD_GIFT"

# max SSD
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, max_SSD[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[9] <- "max_SSD_GIFT"

# mean seed mass
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, mean_seedmass[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[10] <- "mean_seedmass_GIFT"

# min seed mass
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, min_seedmass[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[11] <- "min_seedmass_GIFT"

# max seed mass
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, max_seedmass[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[12] <- "max_seedmass_GIFT"

# mycorrhizal association
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, mycorrhiza[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[13] <- "mycorrhiza_GIFT"

# woodiness
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, woodiness[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[14] <- "woodiness_GIFT"

# growth form
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, growth_form[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[15] <- "growth_form_GIFT"

# lifecycle
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, lifecycle[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[16] <- "lifecycle_GIFT"

# lifespan
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, lifespan[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[17] <- "lifespan_GIFT"

# self fertilization
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, self_fertilization[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[18] <- "self_fertilization_GIFT"

# dispersal 1
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, dispersal_1[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[19] <- "dispersal_1_GIFT"

# dispersal 2
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, dispersal_2[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[20] <- "dispersal_2_GIFT"

# seeds per fruit
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, seeds[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[21] <- "seeds_GIFT"

# nitrogen fixation 
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, nitrogen[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[22] <- "nitrogen_GIFT"

# mean elevational range 
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, mean_elev_range[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[23] <- "mean_elev_range_GIFT"

# min elevational range
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, min_elev_range[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[24] <- "min_elev_range_GIFT"

# max elevational range
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, max_elev_range[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[25] <- "max_elev_range_GIFT"

# habitat
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, habitat[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[26] <- "habitat_GIFT"

# defense
species_pacific_traits_GIFT_prep <- merge(species_pacific_traits_GIFT_prep, defense[,c(2,4)], by = "work_species", all.x = TRUE)
names(species_pacific_traits_GIFT_prep)[27] <- "defense_GIFT"



# entry summary ----------------------------------------------------------------
# get an overview of for how many Pacific plant invaders entries
# were available per trait
colSums(!is.na(species_pacific_traits_GIFT_prep))



# finalize data table ----------------------------------------------------------
# select the order of columns
species_pacific_traits_GIFT <- select(species_pacific_traits_GIFT_prep, 2,3,1,4:27)

# save the created data frame containing available trait entries for all
# Pacific plant invaders
save(species_pacific_traits_GIFT, file = "data/trait_analysis/species_pacific_traits_GIFT.RData")



