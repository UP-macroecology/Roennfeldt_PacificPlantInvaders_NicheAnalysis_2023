#' ---------------------------
#
# Purpose of script: thin presences points in the introduced ranges, sample pseudo-absences and thin them as well
# Author: Anna Rönnfeldt
# Date Created: ~ 2023-09
# Email: roennfeldt@uni-potsdam.de
#
# Notes: /
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
load(paste0(path_imp, "input/occ_count_crit_1.RData"))

# pre-select suitable species ------------------------------------------------

occ_count_crit_1 <- occ_count_crit_1 %>% 
  arrange(species) %>%
  distinct(species, .keep_all = TRUE)

suitable <- occ_count_crit_1[,-1]
suitable[suitable < 20] <- 0 # set cells with less than 20 occurrences to 0 (unsuitable)
suitable[suitable >= 20] <- 1 # set cells with at least than 20 occurrences to 1 (suitable)
suitable$species <- occ_count_crit_1$species
suitable <- suitable %>% relocate(species)
suitable$mainland_regions <- rowSums(suitable[,4:10]) # count how many mainland regions are suitable for each species
# only keep species with enough occurrences in their native range, the Pacific Islands and at least one mainland region
spp_suitable <- suitable[!(suitable$native_occs == 0 | suitable$pacific_occs == 0 | suitable$mainland_regions == 0),]


spp <- spp_suitable$species

specs_done <- list.files(paste0(path_imp, "output/coords_final_intr/")) %>% 
  str_remove(".RData") %>% 
  str_split(pattern = "_") %>%
  map(~ .x[[6]]) %>%
  simplify() %>%
  unique()

spp <- setdiff(spp, specs_done)


# Start parallel computing
no_cores <- 20
cl <- makeCluster(no_cores)
registerDoParallel(cl)


foreach(spp_index = 1:length(spp), .packages = c("terra", "tidyverse", "sf", "purrr", "furrr", "sfheaders")) %dopar% {
  try({
    
    # load world mask
    world_mask <- rast(paste0(path_imp, "input/world_mask.tif"))
    
    # get all intr files that exist for one species
    files_all <- list.files(path = paste0(path_imp,"regional_occs/criterion_1/introduced/"), pattern = paste0("_",spp[spp_index],".RData"))
    
    # for loop over files
    for (file in files_all) {
      
      region <- unlist(str_split(file, pattern = "_"))[3]
      
      print(region)
      
      t <- loadRData(paste0(path_imp,"regional_occs/criterion_1/introduced/intr_occs_",region,"_",spp[spp_index],".RData"))
      
      print(nrow(t))
      
      # select coordinates of the occs
      occ_coords <- as.data.frame(t[,c("lon", "lat")])
      
      cellnumbers <- terra::extract(world_mask, occ_coords, cells = TRUE)
      occ_coords <- occ_coords[!duplicated(cellnumbers[,"cell"]),]
      occ_coords_sf <- st_as_sf(occ_coords, coords = c("lon", "lat"), crs = crs(world_mask)) # transform it into sf object to use in thin function
      
      # spatial thinning using the thin function 
      occ_thinned <- thin(occ_coords_sf, thin_dist = 3000, runs = 10, ncores = 1)
      
      # presence points as refference for the buffer
      presences <- vect(occ_coords, crs = "+proj=longlat +datum=WGS84")
      
      # place buffer around presence points (radius = 200)
      buf_200 <- buffer(presences, width = 200000)
      
      # create mask
      mask_buf_200 <- crop(world_mask, ext(buf_200))
      values(mask_buf_200)[!is.na(values(mask_buf_200))] <- 1
      
      # use mask_buf to rasterize buf (which has been a vector so far; !raster required for later steps)
      buf_200 <- rasterize(buf_200, mask_buf_200)
      
      # set raster cells outside the buffer to NA
      buf_200 <- terra::mask(mask_buf_200, buf_200, overwrite = TRUE)
      
      # randomly select background data within the buffer, excluding presence locations (sampling 10x as many background points as presences)
      occ_cells_200 <- terra::extract(buf_200, occ_coords, cells = TRUE)[,"cell"]
      buf_cells_200 <- terra::extract(buf_200, crds(buf_200), cells = TRUE)[,"cell"]
      diff_cells_200 <- setdiff(buf_cells_200, occ_cells_200)
      
      abs_indices_200 <- sample(diff_cells_200, ifelse(length(diff_cells_200) < nrow(occ_thinned)*10, length(diff_cells_200), nrow(occ_thinned)*10))
      abs_coords_200 <- as.data.frame(xyFromCell(buf_200, abs_indices_200))
      colnames(abs_coords_200) = c("lon", "lat")
      abs_coords_200_sf <- st_as_sf(abs_coords_200, coords = c("lon", "lat"), crs = crs(world_mask)) # transform it into sf object to use in thin function
      
      # thin background data
      abs_thinned_200 <- thin(abs_coords_200_sf, thin_dist = 3000, runs = 10, ncores = 1)
      
      #Merge presence and absence coordinates
      coords_final_nat_200 <- bind_rows(bind_cols(species = spp[spp_index], present = 1, occ_thinned),
                                        bind_cols(species = spp[spp_index], present = 0, abs_thinned_200)) %>%
        as.data.frame() %>%
        mutate(status = "Intr")
      
      save(coords_final_nat_200, file = paste0(path_imp, "output/coords_final_intr/coords_final_intr1_200_",region,"_",spp[spp_index],".RData"))
      
    } # end of for loop over files
    
  })} # end of foreach

stopCluster(cl)

