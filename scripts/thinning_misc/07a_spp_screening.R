library(maps)
library(dplyr)

load("data/testing/spp_left.RData")
load("data/occ_status_resolved_lonlat.RData")

# subset occ df to only contain native occs (to free memory)
occ_df <- subset(occ_status_resolved, criterion_1 == "native")
rm(occ_status_resolved)

# create df for an overview
spp_df <- data.frame(spp = spp, 
                     common_name = as.character(NA),
                     native_occs = as.numeric(NA),
                     skip = as.character(NA))


# select a species
(spec <- spp[61])

# subset occ_df for current species
spp_occ <- subset(occ_df, species == spec)

# plot native occurrences
map("world")
points(spp_occ[,c("lon", "lat")], col = "blue")

spp_df[spp_df$spp == spec, "common_name"] <- "Haselwurz"
spp_df[spp_df$spp == spec, "native_occs"] <- nrow(spp_occ)
spp_df[spp_df$spp == spec, "skip"] <- "no"


spp_few <- subset(spp_df, skip == "no")[,1]

save(spp_few, file = "data/species_few.RData")

