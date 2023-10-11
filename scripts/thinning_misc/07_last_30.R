# preamble
rm(list = ls())

## Packages --------------------------------------------------------------------
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "sf", "doParallel", "foreach", "terra", "purrr", "furrr", "sfheaders", "stringr" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)

# required paths ---------------------------------------------------------------

# path_ds <- "Z:/AG26/Arbeit/datashare/data/biodat/distribution/Pacific_invaders/"
path_imp <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1/")

# required functions -----------------------------------------------------------

# function to load in an object and assign it to an object name of choice
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# function for "fast" spatial thinning"
thin <- function(sf, thin_dist = 3000, runs = 10, ncores = 10){
  
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
# world_mask <- rast(paste0(path_imp, "input/world_mask.tif"))

# pre-select suitable species ------------------------------------------------

occ_count_crit_1 <- occ_count_crit_1 %>% 
  arrange(species) %>%
  distinct(species, .keep_all = TRUE)

suitable <- occ_count_crit_1[,-1]
suitable[suitable < 20] <- 0
suitable[suitable >= 20] <- 1
suitable$species <- occ_count_crit_1$species
suitable <- suitable %>% relocate(species)
suitable$mainland_regions <- rowSums(suitable[,4:10])
spp_suitable <- suitable[!(suitable$native_occs == 0 | suitable$pacific_occs == 0 | suitable$mainland_regions == 0),]

spp <- spp_suitable$species


specs_done <- list.files(paste0(path_imp, "output/coords_final_nat/")) %>% 
  str_remove(".RData") %>% 
  str_split(pattern = "_") %>%
  map(~ .x[[5]]) %>%
  simplify()

spp <- setdiff(spp, specs_done)


spp_last_30 <- tail(spp, 30)

save(spp_last_30, file = paste0(path_imp, "output/spp_last_30.RData"))