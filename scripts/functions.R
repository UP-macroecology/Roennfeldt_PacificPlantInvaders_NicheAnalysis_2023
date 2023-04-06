# load and rename .RData object, taken from: https://stackoverflow.com/a/25455968
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# function for "fast" spatial thinning"
thin <- function(sf, thin_dist = 5000, runs = 10, ncores = 10){
  
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
  
  out
  
}





plotStatus2 <- function(spec = NA, 
                        occs, 
                        bbox = c(-180, -60, 180, 80), 
                        alpha = NA,
                        title = NA){
  
  # plots occurrences of a species, colour-coded according to 5 classes status information (considering POWO, GIFT and GloNAF)
  # input:
  #   - spec: species name as in column "species" of occs
  #   - occs: df with occurrences matched to status information, containing columns "species", "final_status", "lon", "lat"
  #   - bbox: coordinates of bounding box of the plot
  #   - status: status to be plotted, default: all (native, introduced and unknown)
  #   - alpha: transparency of points
  #   - title: plot title (default: species name)
  
  if(!is.na(spec)){
    df_plot <- dplyr::filter(occs, species == spec)
  } else {
    df_plot <- occs
  }
  
  if(is.na(title)){
    if(!is.na(spec)){
      title <- spec
    } else {title = ""}
  }
  
  world <- map_data("world")
  if(nrow(df_plot) == 0){return("No matching occurrences")}
  if(is.na(alpha)){alpha <- 1/log10(nrow(df_plot))}
  
  ggplot(df_plot, aes(x = lon, y = lat, color = final_status)) +
    geom_map(data = world, map = world, aes(map_id = region), fill = "grey80", color = "grey80", inherit.aes = FALSE) +
    geom_point(shape = 19, size = 3, alpha = alpha) +
    scale_color_manual(values = c(native1 = "steelblue3",
                                  native2 = "#74a9cf",
                                  introduced1 = "#cb181d",
                                  introduced2 = "#fc9272", 
                                  confl = "#63f700",
                                  unknown = "lightgrey")) +
    ggtitle(title) +
    xlim(bbox[1], bbox[3]) +
    ylim(bbox[2], bbox[4]) +
    coord_fixed() +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme_bw()
}