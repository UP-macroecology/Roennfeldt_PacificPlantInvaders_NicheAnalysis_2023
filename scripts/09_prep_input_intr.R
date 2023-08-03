
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
    
    # load chelsa data
    chelsa_bioclim <- terra::rast(str_sort(list.files(paste0(path_chelsa), pattern = ".tif", full.names = TRUE), numeric = TRUE))
    
    
    # get all intr files that exist for the current species
    files <- list.files(path = paste0(path_imp,"/coords_final_intr/"), pattern = paste0("_",spp_final[spp_index],".RData")) #TODO
    
    for (file in files) { 
      
      region <- unlist(str_split(file, pattern = "_"))[5]
      
      # load coords_final
      # note: accidentally kept the object names for the intr objects as "nat"
      load(paste0(path_imp,"/coords_final_intr/coords_final_intr1_200_",region,"_",spp[spp_index],".RData")) #TODO
      
      # Extract BioClim variables
      env_vars <- terra::extract(chelsa_bioclim, y = coords_final_nat_200[,c("lon", "lat")]) %>%
        as.data.frame()
      
      # change column names
      names(env_vars) <- str_replace(names(env_vars), pattern = "CHELSA_bio", "bio")
      
      # Prepare dataset including occurrence and environmental data
      data_prep_intr <- bind_cols(coords_final, env_vars) %>% drop_na()
      
      save(data_prep_intr, file = paste0(path_imp,"/final_input_intr/input_intr_",region,spp[spp_index],".RData")) #TODO
      } # end of for loop over files
  })} # end of try and foreach 

stopCluster(cl)