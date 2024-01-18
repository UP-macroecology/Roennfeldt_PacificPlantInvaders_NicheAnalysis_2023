library(dotwhisker)
library(dplyr)
library(phylolm)
library(stringr)
library(tidyr)

# preamble ----------------------------------------------------------------

rm(list = ls())

# path to transfer folder
# path_transfer <- "Y:/AG26/Transfer/Holle_Roennfeldt/"
path_transfer <- "T:/Holle_Roennfeldt/"

logit <- function(x) {x = ifelse(x < 0.0001,0.0001,ifelse(x > 0.9999,.9999,x));log(x/(1 - x))}

# load required data ------------------------------------------------------

load(paste0(path_transfer, "trait_data_processed/species_pacific_traits_GIFT.RData")) 
load("results/ecospat/master_results.RData")

# geographic traits:
load("data/trait_analysis/native_centroid.RData")
load("data/trait_analysis/native_range_size.RData")
 
# phylogeny
phylo_pacific <- read.tree("data/Phylogeny_mod.tre", keep.multi = TRUE)

# prepare trait data ------------------------------------------------------

trait_df <- species_pacific_traits_GIFT %>%
  select(species_orig, mean_height_GIFT, mean_seedmass_GIFT, growth_form_GIFT, lifecycle_GIFT, max_elev_range_GIFT) %>%
  rename_with(stringr::str_replace, 
              pattern = "_GIFT", replacement = "", 
              matches("_GIFT")) %>%
  rename("species" = "species_orig") %>%
  # add geographic traits
  left_join(select(native_centroid_df, species, lat_centroid, lon_centroid), by = "species") %>%
  left_join(select(native_range_df, species, range_both), by = "species") %>%
  rename("range_size" = "range_both")

# recode the categorical traits

# growth form
trait_df[trait_df == "herb"] <- 1
trait_df[trait_df == "shrub"] <- 2
trait_df[trait_df == "tree"] <- 3

# life-cycle
trait_df[trait_df == "annual"] <- 1
trait_df[trait_df == "biennial"] <- 2
trait_df[trait_df == "perennial"] <- 3


# make sure all trait columns are numeric
trait_df <- trait_df %>%
  mutate(across(!species, as.numeric))



# add ecospat results -----------------------------------------------------

trait_eco_df <- master_results %>%
  mutate(region = factor(region, levels = c("pac", "afr", "ate", "atr", "aus", "eur", "nam", "sam"))) %>%
  mutate(total_esu = rel_expansion + rel_stability + rel_unfilling) %>%
  mutate(expansion = rel_expansion / total_esu) %>%
  mutate(stability = rel_stability / total_esu) %>%
  mutate(unfilling = rel_unfilling / total_esu) %>%
  select(c(species, region, unfilling, stability, expansion)) %>%
  left_join(trait_df, by = "species") %>%
  na.omit() 



# standardise trait data --------------------------------------------------

# Standardise traits
trait_eco_df$mean_height <- scale(as.numeric(trait_eco_df$mean_height))
trait_eco_df$mean_seedmass <- scale(as.numeric(trait_eco_df$mean_seedmass))
trait_eco_df$max_elev_range <- scale(as.numeric(trait_eco_df$max_elev_range))
trait_eco_df$range_size <- scale(as.numeric(trait_eco_df$range_size))
trait_eco_df$lat_centroid <- trait_eco_df$lat_centroid/90
trait_eco_df$lon_centroid <- trait_eco_df$lon_centroid/180

# Logit-transform response variables
trait_eco_df$unfilling <- logit(trait_eco_df$unfilling)
trait_eco_df$stability <- logit(trait_eco_df$stability)
trait_eco_df$expansion <- logit(trait_eco_df$expansion)



# add rownames to trait_eco_df to match the tip labels in the phylogeny
trait_eco_df <- trait_eco_df %>%
  mutate(species = str_replace(species, " ", "_")) %>%
  unite(species_region, c("species", "region"))

# save(trait_eco_df, file = "data/trait_analysis/trait_eco_df.RData")

spp_traits <- unique(trait_eco_df$species)

# remove clutter
rm(list = setdiff(ls(), c("trait_eco_df", "phylo_pacific", "spp_traits")))

# prepare phylogeny -------------------------------------------------------

no_match <- trait_eco_df$species[!sub(' ','_',spp_traits) %in% phylo_pacific[[1]][["tip.label"]]]

# phylo_subset <- drop.tip(phylo_pacific, no_match)
phylo_subset <- drop.tip(phylo_pacific, phylo_pacific[[1]][["tip.label"]][!phylo_pacific[[1]][["tip.label"]] %in% sub(' ','_',spp_traits)])

#add rownames - needed for matching phylogenetic information
rownames(trait_eco_df) = trait_eco_df$species_region


# define models -----------------------------------------------------------

# stability
null_stability <- phylolm(stability ~ 1, data = trait_eco_df, phy = phylo_subset[[1]], model = "lambda", lower.bound = 10^-9, upper.bound = 10^-7)

# full models
lm_stability <- phylolm(stability ~ mean_height + mean_seedmass + growth_form + lifecycle + max_elev_range + lat_centroid + lon_centroid + range_size, 
                        data = trait_eco_df, phy = phylo_subset[[1]], model = "lambda", lower.bound = 10^-9)


# step model
step_lm_stability <- phylostep(lm_stability$formula, data = trait_eco_df, phy = phylo_subset[[1]], model = "lambda")