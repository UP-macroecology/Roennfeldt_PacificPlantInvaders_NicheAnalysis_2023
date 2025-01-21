#' ---------------------------
#
# Purpose of script: check whether required folder structure exists. Create missing folders.
# Author: Anna Rönnfeldt
# Date Created: 2025-01-10
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------




#  create base folders ----------------------------------------------------

if(!dir.exists("data")){dir.create("data")}
if(!dir.exists("results")){dir.create("results")}
if(!dir.exists("scripts")){dir.create("scripts")}
if(!dir.exists("plots")){dir.create("plots")}


# create data subfolders --------------------------------------------------


if(!dir.exists(file.path("data", "species_selection"))){
  dir.create(file.path("data", "species_selection"))
}

#'--------------

if(!dir.exists(file.path("data", "occurrence_data"))){
  dir.create(file.path("data", "occurrence_data"))
}

# subfolders within occurrence_data

if(!dir.exists(file.path("data", "occurrence_data", "regional_occs"))){
  dir.create(file.path("data", "occurrence_data", "regional_occs"))
}

if(!dir.exists(file.path("data", "occurrence_data", "status_occs"))){
  dir.create(file.path("data", "occurrence_data", "status_occs"))
}

if(!dir.exists(file.path("data", "occurrence_data", "coords_final_nat"))){
  dir.create(file.path("data", "occurrence_data", "coords_final_nat"))
}

if(!dir.exists(file.path("data", "occurrence_data", "coords_final_intr"))){
  dir.create(file.path("data", "occurrence_data", "coords_final_intr"))
}

if(!dir.exists(file.path("data", "occurrence_data", "final_input_nat"))){
  dir.create(file.path("data", "occurrence_data", "final_input_nat"))
}

if(!dir.exists(file.path("data", "occurrence_data", "final_input_intr"))){
  dir.create(file.path("data", "occurrence_data", "final_input_intr"))
}

#'--------------

if(!dir.exists(file.path("data", "status_assignment"))){
  dir.create(file.path("data", "status_assignment"))
}

# subfolder within status_assignment
if(!dir.exists(file.path("data", "status_assignment", "GloNAF"))){
  dir.create(file.path("data", "status_assignment", "GloNAF"))
}

#'--------------

if(!dir.exists(file.path("data", "spatial_data"))){
  dir.create(file.path("data", "spatial_data"))
}

# subfolder within spatial_data
if(!dir.exists(file.path("data", "spatial_data", "tdwg"))){
  dir.create(file.path("data", "spatial_data", "tdwg"))
}

#'--------------

if(!dir.exists(file.path("data", "native_region_ID"))){
  dir.create(file.path("data", "native_region_ID"))
}

#'--------------

if(!dir.exists(file.path("data", "trait_data"))){
  dir.create(file.path("data", "trait_data"))
}


# subfolder within trait_data
if(!dir.exists(file.path("data", "trait_data", "PCA"))){
  dir.create(file.path("data", "trait_data", "PCA"))
}

if(!dir.exists(file.path("data", "trait_data", "niche_breadth_centroid"))){
  dir.create(file.path("data", "trait_data", "niche_breadth_centroid"))
}

#'--------------

if(!dir.exists(file.path("data", "phylogenies"))){
  dir.create(file.path("data", "phylogenies"))
}



# create results subfolders ------------------------------------------------

if(!dir.exists(file.path("results", "ecospat"))){
  dir.create(file.path("results", "ecospat"))
}

# subfolder within ecospat
if(!dir.exists(file.path("results", "ecospat", "niche_overlap"))){
  dir.create(file.path("results", "ecospat", "niche_overlap"))
}

if(!dir.exists(file.path("results", "ecospat", "niche_similarity"))){
  dir.create(file.path("results", "ecospat", "niche_similarity"))
}

if(!dir.exists(file.path("results", "ecospat", "niche_dynamics"))){
  dir.create(file.path("results", "ecospat", "niche_dynamics"))
}

#'--------------

if(!dir.exists(file.path("results", "trait_analysis"))){
  dir.create(file.path("results", "trait_analysis"))
}

# subfolder within trait_analysis
if(!dir.exists(file.path("results", "trait_analysis", "main_analysis"))){
  dir.create(file.path("results", "trait_analysis", "main_analysis"))
}

if(!dir.exists(file.path("results", "trait_analysis", "full_models"))){
  dir.create(file.path("results", "trait_analysis", "full_models"))
}

if(!dir.exists(file.path("results", "trait_analysis", "univariate"))){
  dir.create(file.path("results", "trait_analysis", "univariate"))
}
