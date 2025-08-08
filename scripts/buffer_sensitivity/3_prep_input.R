

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

path_data <- "/import/ecoc9z/data-zurell/roennfeldt/C1"
path_chelsa <- "/import/ecoc9z/data-zurell/roennfeldt/C1/input/Chelsa_V2"

load(paste0(path_data, "/input/species_selection/spp_buf_comp.RData")) # object: spp_buf_comp

spp_final <- spp_buf_comp
# merge occurrences and climate data --------------------------------------


no_cores <- 10
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# loop over all species
foreach(spp_index = 1:length(spp_final), .packages = c("terra", "dplyr", "stringr", "tidyr")) %dopar% {
  try({
    
    # load chelsa data
    chelsa_bioclim <- terra::rast(str_sort(list.files(paste0(path_chelsa, "/"), pattern = ".tif", full.names = TRUE), numeric = TRUE))
    
    
    
    # introduced files --------------------------------------------------------
    
    
    
    # get all intr files that exist for the current species
    files <- list.files(path = paste0(path_data,"/output/buffer_sensitivity/coords_final_intr/"), pattern = paste0("_",spp_final[spp_index],".RData"))
    
    for (file in files) {
      
      region <- unlist(str_split(file, pattern = "_"))[5]
      
      print(region)
      # load coords_final
      # note: accidentally kept the object names for the intr objects as "nat"
      load(paste0(path_data, "/output/buffer_sensitivity/coords_final_intr/coords_final_intr1_50_",region,"_",spp_final[spp_index],".RData"))
      
      # Extract BioClim variables
      env_vars <- terra::extract(chelsa_bioclim, y = coords_final_nat_50[,c("lon", "lat")]) %>%
        as.data.frame()
      
      # change column names
      names(env_vars) <- str_replace(names(env_vars), pattern = "CHELSA_bio", "bio")
      
      # Prepare dataset including occurrence and environmental data
      data_prep_intr <- bind_cols(coords_final_nat_50, env_vars) %>% drop_na()
      
      save(data_prep_intr, file = paste0(path_data, "/output/buffer_sensitivity/final_input_intr/input_intr_",region,"_",spp_final[spp_index],".RData"))
    } # end of for loop over files
    
    
    
    
    # native files ------------------------------------------------------------
    
    
    # load coords final for the current species
    load(paste0(path_data, "/output/buffer_sensitivity/coords_final_nat/coords_final_nat_50_",spp_final[spp_index],".RData")) #TODO
    
    # Extract BioClim variables
    env_vars <- terra::extract(chelsa_bioclim, y = coords_final_nat_50[,c("lon", "lat")]) %>%
      as.data.frame()
    
    # change column names
    names(env_vars) <- str_replace(names(env_vars), pattern = "CHELSA_bio", "bio")
    
    # Prepare dataset including occurrence and environmental data
    data_prep_nat <- bind_cols(coords_final_nat_50, env_vars) %>% drop_na()
    
    save(data_prep_nat, file = paste0(path_data,"/output/buffer_sensitivity/final_input_nat/input_nat_",spp_final[spp_index],".RData"))
    
    
  })} # end of try and foreach

stopCluster(cl)