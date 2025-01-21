#' ---------------------------
#
# Purpose of script: merge species data (native) with environmental data
# Author: Anna Rönnfeldt
# Date Created: ~ 2023-11
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
#
#' ---------------------------

# packages ----------------------------------------------------------------
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "doParallel", "foreach", "terra", "stringr", "tidyr" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)


# required paths and data -------------------------------------------------

path_data  <- ""
path_chelsa <- ""

# species selection
load(paste0(path_data, "/species_selection/spp_first_selection.RData")) # object: spp_final


# merge occurrences and climate data --------------------------------------

no_cores <- 2
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# loop over all species
foreach(spp_index = 1:length(spp_final), .packages = c("terra", "dplyr", "stringr", "tidyr")) %dopar% {
  try({
    # load coords final for the current species
    load(paste0(path_data, "/occurrence_data/coords_final_nat/coords_final_nat_200_",spp_final[spp_index],".RData")) #TODO
    
    # load in chelsa data 
    chelsa_bioclim <- terra::rast(str_sort(list.files(paste0(path_chelsa, "/"), pattern = ".tif", full.names = TRUE), numeric = TRUE))
    
    # Extract BioClim variables
    env_vars <- terra::extract(chelsa_bioclim, y = coords_final_nat_200[,c("lon", "lat")]) %>%
      as.data.frame()
    
    # change column names
    names(env_vars) <- str_replace(names(env_vars), pattern = "CHELSA_bio", "bio")
    
    # Prepare dataset including occurrence and environmental data
    data_prep_nat <- bind_cols(coords_final_nat_200, env_vars) %>% drop_na()
    
    save(data_prep_nat, file = paste0(path_data,"/occurrence_data/final_input_nat/input_nat_",spp_final[spp_index],".RData")) 
    
    print(spp_final[spp_index])
    
  })} # end of try and foreach

stopCluster(cl)