rm(list = ls())

# species selection pre thinning

# criterion 1 ------------------------------------------------------------------

load("results/intermediate/occ_count_criterion1.RData")

# check which species have enough occurrences over the islands (suitable for general analysis)
spp_enough1 <- subset(occ_count_1, native_occs >= 20 & pac_occs >= 20) # 229

# check how many species can be for the analysis for each mainland region
eur_enough1 <- subset(spp_enough1, eur_occs >= 20)  # 0
afr_enough1 <- subset(spp_enough1, afr_occs >= 20)  # 138
ate_enough1 <- subset(spp_enough1, ate_occs >= 20)  # 111
atr_enough1 <- subset(spp_enough1, atr_occs >= 20)  # 101
aus_enough1 <- subset(spp_enough1, aus_occs >= 20)  # 142
nam_enough1 <- subset(spp_enough1, nam_occs >= 20)  # 136
sam_enough1 <- subset(spp_enough1, sam_occs >= 20)  # 138

# create list of species with enough occurrences depending on the region
occ_enough_1 <- list(nr_occs = spp_enough1, 
                   spp_names = list(general = spp_enough1$species, 
                                    europe = eur_enough1$species,
                                    africa = afr_enough1$species,
                                    a_temperate = ate_enough1$species,
                                    a_tropical = atr_enough1$species,
                                    australasia = aus_enough1$species,
                                    north_america = nam_enough1$species,
                                    south_america = sam_enough1$species))

# save results
save(occ_enough_1, file = "results/intermediate/list_enough_occs1.RData")




# criterion 2 ------------------------------------------------------------------

load("results/intermediate/occ_count_criterion2.RData")

# check which species have enough occurrences over the islands (suitable for general analysis)
spp_enough2 <- subset(occ_count_2, native_occs >= 20 & pac_occs >= 20) # 478

# check how many species can be for the analysis for each mainland region
eur_enough2 <- subset(spp_enough2, eur_occs >= 20)  # 117
afr_enough2 <- subset(spp_enough2, afr_occs >= 20)  # 302
ate_enough2 <- subset(spp_enough2, ate_occs >= 20)  # 248
atr_enough2 <- subset(spp_enough2, atr_occs >= 20)  # 185
aus_enough2 <- subset(spp_enough2, aus_occs >= 20)  # 315
nam_enough2 <- subset(spp_enough2, nam_occs >= 20)  # 322
sam_enough2 <- subset(spp_enough2, sam_occs >= 20)  # 309


# create list of species with enough occurrences depending on the region
occ_enough_2 <- list(nr_occs = spp_enough2, 
                     spp_names = list(general = spp_enough2$species, 
                                      europe = eur_enough2$species,
                                      africa = afr_enough2$species,
                                      a_temperate = ate_enough2$species,
                                      a_tropical = atr_enough2$species,
                                      australasia = aus_enough2$species,
                                      north_america = nam_enough2$species,
                                      south_america = sam_enough2$species))

# save results
save(occ_enough_2, file = "results/intermediate/list_enough_occs2.RData")


