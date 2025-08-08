

#' ---------------------------
#
# Purpose of script: thin presences points in the introduced ranges, sample pseudo-absences (within 50 km buffer) and thin them as well
# Author: Anna Rönnfeldt
# Date Created: 2025-06-10
# Email: roennfeldt@uni-potsdam.de
#
# Notes: designed to be run on HPC, not local machine
#
#' ---------------------------



install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "sf", "doParallel", "foreach", "terra", "purrr", "stringr" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)


# required paths ----------------------------------------------------------


# path to data location (here on HPC)
path_data <- "/import/ecoc9z/data-zurell/roennfeldt/C1"

# required functions -----------------------------------------------------------

# function to load in an object and assign it to an object name of choice
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


thin <- function(sf, thin_dist = 3000, runs = 10, ncores = 1){
  
  require(sf, quietly = TRUE)
  require(purrr, quietly = TRUE)
  require(furrr, quietly = TRUE)
  
  sample.vec <- function(x, ...) x[sample(length(x), ...)]
  
  sf_buffer <- st_buffer(sf, thin_dist)
  buff_int <- st_intersects(sf, sf_buffer) 
  buff_int <- setNames(buff_int, 1:length(buff_int))
  
  n_int <- map_dbl(buff_int, length)
  
  plan(multisession, workers = ncores)
  
  seeds <- sample.int(n = runs)
  results_runs <- future_map(seeds, function(i){
    
    set.seed(i)
    while (max(n_int) > 1) {
      max_neighbors <- names(which(n_int == max(n_int)))
      
      # remove point with max neighbors
      sampled_id <- sample.vec(max_neighbors, 1)
      
      pluck(buff_int, sampled_id) <- NULL
      buff_int <- map(buff_int, function(x) setdiff(x, as.numeric(sampled_id)))
      n_int <- map_dbl(buff_int, length)
    }
    
    unlist(buff_int) %>% unique()
    
  })
  
  lengths <- map_dbl(results_runs, length)
  
  selected_run <- results_runs[[sample.vec(which(lengths == max(lengths)), 1)]]
  
  out <- sf[selected_run,]
  
  out <- sf_to_df(out)[,3:4] %>%
    rename("lon" = "x", "lat" = "y")
  
  out
}



# required data -----------------------------------------------------------

# species selection
load(paste0(path_data, "/input/species_selection/spp_buffer_2.RData"))

spp <- spp_buffer_2


# specs_done <- list.files(paste0(path_data, "/occurrence_data/coords_final_intr/")) %>% 
#   str_remove(".RData") %>% 
#   str_split(pattern = "_") %>%
#   map(~ .x[[6]]) %>%
#   simplify() %>%
#   unique()
# 
# spp <- setdiff(spp_final, specs_done)



# Start parallel computing
no_cores <- 12
cl <- makeCluster(no_cores)
registerDoParallel(cl)


foreach(spp_index = 1:length(spp), .packages = c("terra", "tidyverse", "sf", "purrr", "furrr", "sfheaders")) %dopar% {
  # try({

    # load world mask
    world_mask <- rast(paste0(path_data, "/input/spatial_data/world_mask.tif"))

    # get all intr files that exist for one species
    files_all <- list.files(path = paste0(path_data,"/regional_occs/criterion_1/introduced/"), pattern = paste0("_",spp[spp_index],".RData"))

    # for loop over files
    for (file in files_all) {

      region <- unlist(str_split(file, pattern = "_"))[3]

      print(region)

      t <- loadRData(paste0(path_data,"/regional_occs/criterion_1/introduced/intr_occs_",region,"_",spp[spp_index],".RData"))

      print(nrow(t))

      # select coordinates of the occs
      occ_coords <- as.data.frame(t[,c("lon", "lat")])

      cellnumbers <- terra::extract(world_mask, occ_coords, cells = TRUE)
      occ_coords <- occ_coords[!duplicated(cellnumbers[,"cell"]),]
      occ_coords_sf <- st_as_sf(occ_coords, coords = c("lon", "lat"), crs = crs(world_mask)) # transform it into sf object to use in thin function

      # skip the thinnin gof the presence points (already done in main analysis)
      # # spatial thinning using the thin function
      # occ_thinned <- thin(occ_coords_sf, thin_dist = 3000, runs = 10, ncores = 1)
      
      # instead, load the thinned data from the main analysis and take the presence points from there
      load(paste0(path_data, "/output/coords_final_intr_rev/coords_final_intr1_200_",region,"_",spp[spp_index],".RData")) # object is mistakelny called nat as well
      
      occ_thinned <- coords_final_nat_200 %>% 
        filter(present == 1) %>% 
        select(lon, lat)
      
      rm(coords_final_nat_200)

      # presence points as reference for the buffer
      presences <- vect(occ_coords, crs = "+proj=longlat +datum=WGS84")

      # place buffer around presence points (radius = 50)
      buf_50 <- buffer(presences, width = 50000)

      # create mask
      mask_buf_50 <- crop(world_mask, ext(buf_50))
      values(mask_buf_50)[!is.na(values(mask_buf_50))] <- 1

      # use mask_buf to rasterize buf (which has been a vector so far; !raster required for later steps)
      buf_50 <- rasterize(buf_50, mask_buf_50)

      # set raster cells outside the buffer to NA
      buf_50 <- terra::mask(mask_buf_50, buf_50, overwrite = TRUE)

      # randomly select background data within the buffer, excluding presence locations (sampling 10x as many background points as presences)
      occ_cells_50 <- terra::extract(buf_50, occ_coords, cells = TRUE)[,"cell"]
      buf_cells_50 <- terra::extract(buf_50, crds(buf_50), cells = TRUE)[,"cell"]
      diff_cells_50 <- setdiff(buf_cells_50, occ_cells_50)

      abs_indices_50 <- sample(diff_cells_50, ifelse(length(diff_cells_50) < nrow(occ_thinned)*10, length(diff_cells_50), nrow(occ_thinned)*10))
      abs_coords_50 <- as.data.frame(xyFromCell(buf_50, abs_indices_50))
      colnames(abs_coords_50) = c("lon", "lat")
      abs_coords_50_sf <- st_as_sf(abs_coords_50, coords = c("lon", "lat"), crs = crs(world_mask)) # transform it into sf object to use in thin function

      # thin background data
      abs_thinned_50 <- thin(abs_coords_50_sf, thin_dist = 3000, runs = 10, ncores = 1)

      #Merge presence and absence coordinates
      coords_final_nat_50 <- bind_rows(bind_cols(species = spp[spp_index], present = 1, occ_thinned),
                                        bind_cols(species = spp[spp_index], present = 0, abs_thinned_50)) %>%
        as.data.frame() %>%
        mutate(status = "Intr")

      save(coords_final_nat_50, file = paste0(path_data, "/output/buffer_sensitivity/coords_final_intr/coords_final_intr1_50_",region,"_",spp[spp_index],".RData"))

    } # end of for loop over files

  # })
} # end of foreach

stopCluster(cl)



# load(paste0(path_data, "/input/species_selection/df_pending.RData"))
# 
# 
# foreach(row_index = 1:nrow(df_pending), .packages = c("terra", "tidyverse", "sf", "purrr", "furrr", "sfheaders")) %dopar% {
#   
#   spp <- df_pending[row_index, "species"]
#   reg <- df_pending[row_index, "region"]
#   
#   
#   # load world mask
#   world_mask <- rast(paste0(path_data, "/input/spatial_data/world_mask.tif"))
#   
#   t <- loadRData(paste0(path_data,"/regional_occs/criterion_1/introduced/intr_occs_",reg,"_",spp,".RData"))
#   
#   print(nrow(t))
#   
#   # select coordinates of the occs
#   occ_coords <- as.data.frame(t[,c("lon", "lat")])
#   
#   cellnumbers <- terra::extract(world_mask, occ_coords, cells = TRUE)
#   occ_coords <- occ_coords[!duplicated(cellnumbers[,"cell"]),]
#   occ_coords_sf <- st_as_sf(occ_coords, coords = c("lon", "lat"), crs = crs(world_mask)) # transform it into sf object to use in thin function
#   
#   # spatial thinning using the thin function
#   occ_thinned <- thin(occ_coords_sf, thin_dist = 3000, runs = 10, ncores = 1)
#   
#   # presence points as refference for the buffer
#   presences <- vect(occ_coords, crs = "+proj=longlat +datum=WGS84")
#   
#   # place buffer around presence points (radius = 50)
#   buf_50 <- buffer(presences, width = 50000)
#   
#   # create mask
#   mask_buf_50 <- crop(world_mask, ext(buf_50))
#   values(mask_buf_50)[!is.na(values(mask_buf_50))] <- 1
#   
#   # use mask_buf to rasterize buf (which has been a vector so far; !raster required for later steps)
#   buf_50 <- rasterize(buf_50, mask_buf_50)
#   
#   # set raster cells outside the buffer to NA
#   buf_50 <- terra::mask(mask_buf_50, buf_50, overwrite = TRUE)
#   
#   # randomly select background data within the buffer, excluding presence locations (sampling 10x as many background points as presences)
#   occ_cells_50 <- terra::extract(buf_50, occ_coords, cells = TRUE)[,"cell"]
#   buf_cells_50 <- terra::extract(buf_50, crds(buf_50), cells = TRUE)[,"cell"]
#   diff_cells_50 <- setdiff(buf_cells_50, occ_cells_50)
#   
#   abs_indices_50 <- sample(diff_cells_50, ifelse(length(diff_cells_50) < nrow(occ_thinned)*10, length(diff_cells_50), nrow(occ_thinned)*10))
#   abs_coords_50 <- as.data.frame(xyFromCell(buf_50, abs_indices_50))
#   colnames(abs_coords_50) = c("lon", "lat")
#   abs_coords_50_sf <- st_as_sf(abs_coords_50, coords = c("lon", "lat"), crs = crs(world_mask)) # transform it into sf object to use in thin function
#   
#   # thin background data
#   abs_thinned_50 <- thin(abs_coords_50_sf, thin_dist = 3000, runs = 10, ncores = 1)
#   
#   #Merge presence and absence coordinates
#   coords_final_nat_50 <- bind_rows(bind_cols(species = spp, present = 1, occ_thinned),
#                                    bind_cols(species = spp, present = 0, abs_thinned_50)) %>%
#     as.data.frame() %>%
#     mutate(status = "Intr")
#   
#   save(coords_final_nat_50, file = paste0(path_data, "/output/buffer_sensitivity/coords_final_intr/coords_final_intr1_50_",reg,"_",spp,".RData"))
#   
#   
# } # end of foreach
# 
# stopCluster(cl)

gc()
rm(list = ls())