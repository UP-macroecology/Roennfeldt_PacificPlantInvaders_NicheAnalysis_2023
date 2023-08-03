
# packages ----------------------------------------------------------------
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "doParallel", "foreach", "terra" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)


rm(list = ls())


# required paths and data -------------------------------------------------

path_chelsa <- "Z:/Arbeit/datashare/data/envidat/biophysical/CHELSA_V2/global/"
path_imp  <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1/") # TODO

# final species selection
load(paste0(path_imp, "output/final_species_list.RData")) #TODO

# merge occurrences and climate data --------------------------------------


no_cores <- 15
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# loop over all species
foreach(spp_index = 1:length(spp_final), .packages = c("terra", "dplyr")) %dopar% {
  try({
    # load coords final for the current species
    load(paste0(path_imp, "coords_final_nat/coords_final_nat_200_",spp_final[spp_index],".RData")) #TODO
    
    # load in chelsa data 
    chelsa_bioclim <- terra::rast(str_sort(list.files(paste0(path_chelsa), pattern = ".tif", full.names = TRUE), numeric = TRUE))
    
    # Extract BioClim variables
    env_vars <- terra::extract(chelsa_bioclim, y = coords_final_nat_200[,c("lon", "lat")]) %>%
      as.data.frame()
    
    # change column names
    names(env_vars) <- str_replace(names(env_vars), pattern = "CHELSA_bio", "bio")
    
    # Prepare dataset including occurrence and environmental data
    data_prep_nat <- bind_cols(coords_final, env_vars) %>% drop_na()
    
    save(data_prep_nat, file = paste0(path_imp,"/final_input_nat/input_nat_",spp[spp_index],".RData")) 
    
    spec <- spp[spp_index]
    print(spec)
    
  })} # end of try and foreach

stopCluster(cl)