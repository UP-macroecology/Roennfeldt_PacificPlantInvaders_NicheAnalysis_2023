rm(list = ls())

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# get species names for species with enough occs
load("results/enough_occs2.RData")
specs_all <- occ_enough2[["nr_occs"]][["species"]]
# rm(occ_enough)

# get all file names for these species in the regional_occs folder

files <- list.files(path = "data/regional_occs/", pattern = paste0("nat2_", specs_all, collapse = "|"))
# for file in files
# load and relocate files
for(file in files){
  
  print(file)
  # load file from old folder
  t <- loadRData(paste0("data/regional_occs/",file))
  # save file in new folder
  save(t, file = paste0("data/regional_occs_cluster_new2/", file))
}


# remove unnecessary files from regional_occs_cluster


# europe is 0 f
specs_afr <- occ_enough2[["spp_names"]][["africa"]]
specs_ate <- occ_enough2[["spp_names"]][["a_temperate"]]
specs_atr <- occ_enough2[["spp_names"]][["a_tropical"]]
specs_aus <- occ_enough2[["spp_names"]][["australasia"]]
specs_nam <- occ_enough2[["spp_names"]][["north_america"]]
specs_sam <- occ_enough2[["spp_names"]][["south_america"]]

files_afr <- list.files(path = "data/regional_occs/", pattern = paste0("intr2_afr_", specs_afr, collapse = "|"))
files_ate <- list.files(path = "data/regional_occs/", pattern = paste0("intr2_ate_", specs_ate, collapse = "|"))
files_atr <- list.files(path = "data/regional_occs/", pattern = paste0("intr2_atr_", specs_atr, collapse = "|"))
files_aus <- list.files(path = "data/regional_occs/", pattern = paste0("intr2_aus_", specs_aus, collapse = "|"))
files_nam <- list.files(path = "data/regional_occs/", pattern = paste0("intr2_nam_", specs_nam, collapse = "|"))
files_sam <- list.files(path = "data/regional_occs/", pattern = paste0("intr2_sam_", specs_sam, collapse = "|"))

# for file in files
# load and relocate files
for(file in files_afr){
  
  print(file)
  # load file from old folder
  t <- loadRData(paste0("data/regional_occs/",file))
  # save file in new folder
  save(t, file = paste0("data/regional_occs_cluster_new2/", file))
}


for(file in files_ate){
  
  # print(file)
  # load file from old folder
  t <- loadRData(paste0("data/regional_occs/",file))
  # save file in new folder
  save(t, file = paste0("data/regional_occs_cluster_new2/", file))
}

for(file in files_atr){
  
  print(file)
  # load file from old folder
  t <- loadRData(paste0("data/regional_occs/",file))
  # save file in new folder
  save(t, file = paste0("data/regional_occs_cluster_new2/", file))
}

for(file in files_aus){
  
  print(file)
  # load file from old folder
  t <- loadRData(paste0("data/regional_occs/",file))
  # save file in new folder
  save(t, file = paste0("data/regional_occs_cluster_new2/", file))
}

for(file in files_nam){
  
  print(file)
  # load file from old folder
  t <- loadRData(paste0("data/regional_occs/",file))
  # save file in new folder
  save(t, file = paste0("data/regional_occs/", file))
}

for(file in files_sam){
  
  print(file)
  # load file from old folder
  t <- loadRData(paste0("data/regional_occs/",file))
  # save file in new folder
  save(t, file = paste0("data/regional_occs_cluster_new2/", file))
}
