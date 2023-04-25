install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "sf", "doParallel", "foreach", "terra", "purrr", "furrr", "sfheaders" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)

# required paths
# path_ds <- "Z:/AG26/Arbeit/datashare/data/biodat/distribution/Pacific_invaders/"
path_imp <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1/")

# required functions -----------------------------------------------------------

# function to load in an object and assign it to an object name of choice
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


thin <- function(sf, thin_dist = 5000, runs = 10, ncores = 1){
  
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
    while(max(n_int) > 1){
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


# load("results/enough_occs.RData")
load(paste0(path_imp, "input_data/list_enough_occs1.RData"))
# get names of relevant species ------------------------------------------------
# species suitable for the analysis (>= 20 occs over the island region)
spp <- occ_enough_1[["spp_names"]][["general"]]

# # create vector containing all the abbreviated region names
# regions <- c("afr", "ate", "atr", "aus", "nam", "pac", "sam")
# 

# Start parallel computing
no_cores <- 10
cl <- makeCluster(no_cores)
registerDoParallel(cl)

foreach(spp_index = 1:length(spp), .packages = c("terra", "tidyverse", "sf", "purrr", "furrr", "sfheaders")) %dopar% {
  try({
    
    # load world mask
    world_mask <- rast(paste0(path_imp, "input_data/world_mask.tif"))
    
    # get all intr files that exist for one species
    files_all <- list.files(path = paste0(path_imp,"output/regional_occs/"), pattern = paste0("_",spp[spp_index],".RData"))
    files <- files_all[grep("intr1", files_all)]
    
    # for loop over files
    for(file in files){
      
      region <- unlist(str_split(file, pattern = "_"))[2]
      
      t <- loadRData(paste0(path_imp,"output/regional_occs/intr1_",region,"_",spp[spp_index],".RData"))
      
      # select coordinates of the occs
      occ_coords <- as.data.frame(t[,c("lon", "lat")])
      
      cellnumbers <- terra::extract(world_mask, occ_coords, cells = TRUE)
      occ_coords <- occ_coords[!duplicated(cellnumbers[,"cell"]),]
      occ_coords_sf <- st_as_sf(occ_coords, coords = c("lon", "lat"), crs = crs(world_mask)) # transform it into sf object to use in thin function
      
      # spatial thinning using the thin function 
      occ_thinned <- thin(occ_coords_sf, thin_dist = 3000, runs = 10, ncores = 1)
      
      # presence points as refference for the buffer
      presences <- vect(occ_coords, crs = "+proj=longlat +datum=WGS84")
      
      # place buffer around presence points (radius = 200 | 500 km to account for dispersal limitations)
      buf_200 <- buffer(presences, width = 200000)
      buf_500 <- buffer(presences, width = 500000)
      
      # from here on each step is done twice (for the two different buffer sizes respectively)
      
      # create mask
      mask_buf_200 <- crop(world_mask, ext(buf_200))
      values(mask_buf_200)[!is.na(values(mask_buf_200))] <- 1
      
      mask_buf_500 <- crop(world_mask, ext(buf_500))
      values(mask_buf_500)[!is.na(values(mask_buf_500))] <- 1
      
      # use mask_buf to rasterize buf (which has been a vector so far; !raster required for later steps)
      buf_200 <- rasterize(buf_200, mask_buf_200)
      buf_500 <- rasterize(buf_500, mask_buf_500)
      
      # set raster cells outside the buffer to NA
      buf_200 <- terra::mask(mask_buf_200, buf_200, overwrite = TRUE)
      buf_500 <- terra::mask(mask_buf_500, buf_500, overwrite = TRUE)
      
      # randomly select background data within the buffer, excluding presence locations (sampling 10x as many background points as presences)
      occ_cells_200 <- terra::extract(buf_200, occ_coords, cells = TRUE)[,"cell"]
      buf_cells_200 <- terra::extract(buf_200, crds(buf_200), cells = TRUE)[,"cell"]
      diff_cells_200 <- setdiff(buf_cells_200, occ_cells_200)
      
      abs_indices_200 <- sample(diff_cells_200, ifelse(length(diff_cells_200) < nrow(occ_thinned)*10, length(diff_cells_200), nrow(occ_thinned)*10))
      abs_coords_200 <- as.data.frame(xyFromCell(buf_200, abs_indices_200))
      colnames(abs_coords_200) = c("lon", "lat")
      abs_coords_200_sf <- st_as_sf(abs_coords_200, coords = c("lon", "lat"), crs = crs(world_mask)) # transform it into sf object to use in thin function
      
      # do the same for the 500 km buffer
      occ_cells_500 <- terra::extract(buf_500, occ_coords, cells = TRUE)[,"cell"]
      buf_cells_500 <- terra::extract(buf_500, crds(buf_500), cells = TRUE)[,"cell"]
      diff_cells_500 <- setdiff(buf_cells_500, occ_cells_500)
      
      abs_indices_500 <- sample(diff_cells_500, ifelse(length(diff_cells_500) < nrow(occ_thinned)*10, length(diff_cells_500), nrow(occ_thinned)*10))
      abs_coords_500 <- as.data.frame(xyFromCell(buf_500, abs_indices_500))
      colnames(abs_coords_500) = c("lon", "lat")
      abs_coords_500_sf <- st_as_sf(abs_coords_500, coords = c("lon", "lat"), crs = crs(world_mask)) # transform it into sf object to use in thin function
      
      
      # thin background data
      abs_thinned_200 <- thin(abs_coords_200_sf, thin_dist = 3000, runs = 10, ncores = 1)
      abs_thinned_500 <- thin(abs_coords_500_sf, thin_dist = 3000, runs = 10, ncores = 1)
      
      
      #Merge presence and absence coordinates
      coords_final_nat_200 <- bind_rows(bind_cols(species = spp[spp_index], present = 1, occ_thinned),
                                        bind_cols(species = spp[spp_index], present = 0, abs_thinned_200)) %>%
        as.data.frame() %>%
        mutate(status = "Intr")
      
      save(coords_final_nat_200, file = paste0(path_imp, "output/coords_final_intr/coords_final_intr1_200_",region,"_",spp[spp_index],".RData"))
      
      coords_final_nat_500 <- bind_rows(bind_cols(species = spp[spp_index], present = 1, occ_thinned),
                                        bind_cols(species = spp[spp_index], present = 0, abs_thinned_500)) %>%
        as.data.frame() %>%
        mutate(status = "Intr")
      
      save(coords_final_nat_500, file = paste0(path_imp, "output/coords_final_intr/coords_final_intr1_500_",region,"_",spp[spp_index],".RData"))
      
    } # end of for loop over regions
  
  })} # end of foreach



