library(dplyr)

rm(list = ls())


#  required data ----------------------------------------------------------

load("data/trait_analysis/phylo_subset.RData")
load("data/trait_analysis/trait_example_df_spec.RData")
load("results/master_results_new.RData")

# Define logit function
logit = function(x) {x = ifelse(x < 0.0001,0.0001,ifelse(x > 0.9999,.9999,x));log(x/(1 - x))}

# Standardise traits
trait_example_df$mean_height_GIFT <- scale(as.numeric(trait_example_df$mean_height_GIFT))
trait_example_df$max_elev_range_GIFT <- scale(as.numeric(trait_example_df$max_elev_range_GIFT))
trait_example_df$range_both <- scale(as.numeric(trait_example_df$range_both))
trait_example_df$lat_centroid <- trait_example_df$lat_centroid/90

# recode categorical traits

# woodiness_GIFT
trait_example_df[trait_example_df == "non-woody"] <- 1
trait_example_df[trait_example_df == "woody"] <- 2

# growth form
trait_example_df[trait_example_df == "herb"] <- 1
trait_example_df[trait_example_df == "shrub"] <- 2
trait_example_df[trait_example_df == "tree"] <- 3

# life-cycle
trait_example_df[trait_example_df == "annual"] <- 1
trait_example_df[trait_example_df == "perennial"] <- 2

# mycorrhiza
trait_example_df[trait_example_df == "no"] <- 1
trait_example_df[trait_example_df == "partial"] <- 2
trait_example_df[trait_example_df == "yes"] <- 3


# merge traits with response variables ------------------------------------

spp_traits <- unique(trait_example_df$species)

traits_all <- master_results %>%
  filter(species %in% spp_traits) %>%
  select(species, region, overlap, rel.abandonment, rel.unfilling, rel.stability, rel.expansion, rel.pioneering) %>%
  left_join(trait_example_df, by = "species") %>%
  relocate(species_id)



# add rownames - needed for matching phylogenetic information
rownames(traits_all) = sub(' ','_', traits_all$species)

# Logit-transform response variables
results_all$overlap <- logit(results_all)


# Logit-transform response variables
traits_all$range_D_logit <- logit(traits_all$range_D)
traits_all$range_stability_std_logit <- logit(traits_all$range_stability_std)
traits_all$range_unfilling_std_logit <- logit(traits_all$range_unfilling_std)
traits_all$range_expansion_std_logit <- logit(traits_all$range_expansion_std)
traits_all$niche_D_logit <- logit(traits_all$niche_D)
traits_all$niche_stability_std_logit <- logit(traits_all$niche_stability_std)
traits_all$niche_unfilling_std_logit <- logit(traits_all$niche_unfilling_std)
traits_all$niche_expansion_std_logit <- logit(traits_all$niche_expansion_std)
