library(tidyverse)
library(purrr)
library(sf)
library(kewr) # install from GitHub with: devtools::install_github("barnabywalker/kewr")
# install from GitHub:
#devtools::install_github("idiv-biodiversity/LCVP") # data package; needed to use lcvplants
#devtools::install_github("idiv-biodiversity/lcvplants")
library(lcvplants)
library(GIFT) # install from GitHub with: devtools::install_github("https://github.com/BioGeoMacro/GIFT")
library(reticulate) # R interface to Python modules
DIFFLIB <- reticulate::import("difflib") # load Python module






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




getPowoNames <- function(spec, incl_lcvp_synonyms = FALSE, perfect_matches_only = TRUE){
  
  # returns POWO page information corresponding to a species name;
  # (POWO is used in a later step to obtain status information)
  # the species name is first searched in LCVP (Leipzig Catalogue of Vascular Plants; Freiberg et al. 2020),
  # then the obtained LCVP names are used to search POWO (Plants of the World Online: https://powo.science.kew.org),
  # POWO results are then checked against LCVP, POWO names that are directed to another LCVP entry than the input species name are excluded
  # input:  
  #   - spec: species name
  #   - incl_lcvp_synonyms: TRUE = LCVP search results include names that are considered synonyms in LCVP;
  #                         FALSE = only accepted names in LCVP are considered
  #   - perfect_matches_only: TRUE = return only those POWO entries where species name and author match LCVP results
  #                           FALSE = return POWO entries where species name matches LCVP results without considering author information; matches should be manually checked afterwards
  # output: 
  #   - df: searched name - LCVP name - POWO name - POWO url - IPNI id (IPNI = International Plant Name Index: https://www.ipni.org)
  
  print(spec)
  
  # search LCVP for all entries connected to the searched species name:
  if(incl_lcvp_synonyms){
    status_ok <- c("accepted", "synonym") # exclude unresolved and external results
  } else {status_ok <- "accepted"} # only accepted species names
  lcvp_spec_res <- unique(lcvp_fuzzy_search(spec, status = status_ok)$Output.Taxon) # lcvp_fuzzy_search from package lcvpplants
  
  # use LCVP entries to search POWO (including author names):
  
  lcvp_query <- as.data.frame(str_split_fixed(lcvp_spec_res, " ", n = 3)) %>%
    setNames(c("genus", "species", "author")) %>% 
    filter(!grepl("var.", author)) # if species name is a variety, extracting author name doesn't work + search_powo() does not yield results when including "var." (relevant only for Emilia sonchifolia)
  
  powo_res <- apply(lcvp_query, 1, function(x){
    
    if(perfect_matches_only){
      powo_out <- search_powo(query = list(genus = x["genus"], species = x["species"], author = x["author"]))
    } else {
      powo_out <- search_powo(query = list(genus = x["genus"], species = x["species"]))
    }
    
    # no POWO result found for respective query:
    if(powo_out$total == 0){return(NULL)}
    
    # POWO results found:
    # check whether name is synonym on POWO and page is redirected:
    powo_inf <- map_dfr(.x = powo_out$results, .f = function(.x){
      
      if(.x$accepted){
        
        # species name is not a synonym:
        powo_name <- paste(.x$name, .x$author)
        powo_url <- paste0("https://powo.science.kew.org", .x$url)
        
      } else {
        
        # species name is a synonym:
        powo_name <- paste(.x$synonymOf$name, .x$synonymOf$author)
        powo_url <- paste0("https://powo.science.kew.org", .x$synonymOf$url)
        
      }
      
      return(data.frame(powo_name, powo_url))
    })
    
    powo_inf_df <- data.frame("searched_name" = spec,
                              "lcvp_name" = paste(x["genus"], x["species"], x["author"]),
                              "powo_name" = powo_inf$powo_name,
                              "powo_url" = powo_inf$powo_url)
  })
  
  powo_res_df <- powo_res %>% 
    bind_rows() %>% 
    distinct()
  
  # no POWO results found: return df without POWO information:
  if(nrow(powo_res_df) == 0){
    
    print("No matching POWO pages found.")
    powo_output_df <- data.frame("searched_name" = rep(spec, length = length(lcvp_spec_res)),
                                 "lcvp_name" = lcvp_spec_res,
                                 "powo_name" = NA,
                                 "powo_url" = NA,
                                 "ipni_id" = NA)
    return(powo_output_df)
    
  }
  
  # POWO results found:
  # add IPNI ID:
  powo_res_df$ipni_id <- sub(pattern = "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:", 
                             replacement = "", 
                             x = powo_res_df$powo_url)
  
  # check POWO results against LCVP:
  # exclude POWO entries that are directed to LCVP names not linked to the searched species name
  #  = include entry if POWO name is either not found in LCVP or if LCVP search results for POWO name include LCVP entries matched to the searched species name
  lcvp_dbl_check <- sapply(powo_res_df$powo_name, function(x){
    
    lcvp_res <- lcvp_fuzzy_search(x, status = c("accepted", "synonym"))$Output.Taxon # search POWO name in LCVP
    include <- is.null(lcvp_res) || any(lcvp_res %in% powo_res_df$lcvp_name)
    
  }, USE.NAMES = FALSE)
  
  powo_output_df <- powo_res_df[lcvp_dbl_check,]
  
  # POWO results don't properly match LCVP species name: return df without POWO information
  if(nrow(powo_output_df) == 0){
    print("No matching POWO pages found.")
    powo_output_df <- data.frame("searched_name" = rep(spec, length = length(lcvp_spec_res)),
                                 "lcvp_name" = lcvp_spec_res,
                                 "powo_name" = NA,
                                 "powo_url" = NA,
                                 "ipni_id" = NA)
    return(powo_output_df)
  } 
  
  if(perfect_matches_only){
    print("Matching POWO entries found!")
  }else{
    print("Possibly matching POWO entries found!")}
  
  return(powo_output_df)
}



getPowoStatus <- function(ipni_id, lcvp_name){
  
  # returns status information from POWO based on IPNI ID:
  # input: 
  #   - ipni_id: IPNI id corresponding to POWO page (output of getPowoNames)
  #   - lcvp_name: LCVP name corresponding to the IPNI id (output of getPowoNames)
  # output: 
  #   - df: IPNI ID - LCVP name - POWO name - TDWG region name - TDWG region code - status
  
  print(ipni_id)
  
  # use IPNI IDs to get data from POWO:
  powo_dt <- tryCatch({
    
    lookup_powo(ipni_id, distribution = TRUE)},
    
    # no POWO page found: return df without POWO information
    error = function(e){         
      print(paste("No POWO page found for IPNI ID", ipni_id))
      distr_df <- tibble(ipni_id = ipni_id,
                         lcvp_name = lcvp_name,
                         powo_name = as.character(NA),
                         tdwgName = as.character(NA),
                         tdwgCode = as.character(NA),
                         status = as.character(NA))
      return(distr_df)
    })
  
  powo_name <- paste(powo_dt$name, powo_dt$authors)
  print(powo_name)
  
  # POWO doesn't contain distribution information: return df without status information:
  if(is.null(powo_dt$distribution)){
    distr_df <- tibble(ipni_id = ipni_id,
                       lcvp_name = lcvp_name,
                       powo_name = powo_name,
                       tdwgName = as.character(NA),
                       tdwgCode = as.character(NA),
                       status = as.character(NA))
    print("No distribution information on POWO. No status information obtained.")
    return(distr_df)
  }
  
  # POWO contains distribution information:
  
  # extract native range:
  powo_distr_nat <- map_dfr(powo_dt$distribution$natives, `[`, c("name", "tdwgCode"))
  colnames(powo_distr_nat)[1] <- "tdwgName"
  
  # extract introduced range:
  powo_distr_intr <- map_dfr(powo_dt$distribution$introduced, `[`, c("name", "tdwgCode"))
  colnames(powo_distr_intr)[1] <- "tdwgName"
  
  # combine information:
  distr_df <- bind_rows(tibble(ipni_id = ipni_id,
                               lcvp_name = lcvp_name,
                               powo_name = powo_name,
                               powo_distr_nat, 
                               status = "native"),
                        tibble(ipni_id = ipni_id,
                               lcvp_name = lcvp_name,
                               powo_name = powo_name,
                               powo_distr_intr, 
                               status = "introduced"))
  return(distr_df)
}



checkPowoStatus <- function(spec, powo_stat_dt){
  
  # returns regions where POWO status information for the LCVP species names matching the searched species name contradict or agree 
  # input: 
  #   - spec: species name without author information as used as input for getPowoNames(), 
  #   - powo_stat_dt: output of getPowoStatus(): df which contains columns "searched_name", "tdwgName", "status"
  # output: 
  #   - list: - df: distr_contr: regions for which status information of the LCVP species names obtained for the searched name is contradictory
  #           - df: distr_matches: regions for which status information of the LCVP species names obtained for the searched name agree
  
  print(spec)
  
  # check if input data contain necessary columns:
  if(!all(c("searched_name", "tdwgName", "status") %in% colnames(powo_stat_dt))){
    stop("Provided data frame must contain the columns searched_name, tdwgName and status.")
  }
  
  # POWO information for considered species:
  powo_stat_dt_spec <- powo_stat_dt %>% 
    filter(searched_name == spec)
  
  # check for contradictory status information of different species names:
  contr_st_df <- powo_stat_dt_spec %>% 
    group_by(tdwgName) %>% 
    filter(n() > 1) %>% # names which occur more than once
    arrange(tdwgName) %>%
    group_by(tdwgName, status) %>% 
    filter(n() == 1) %>%  # n() = size of groups (1 = different status for same region name, 2 = same status for same region name)
    ungroup()
  
  if(nrow(contr_st_df) != 0){
    print("There are contradictions. See distr_contr for details.")
  } else {print("No contradictions. :)")}
  
  # check for matching status information between species names:
  match_st_df <- powo_stat_dt_spec %>% 
    group_by(tdwgName, status) %>% 
    arrange(tdwgName) %>%
    filter(n()>1) %>% 
    ungroup()
  
  return(list(distr_contr = contr_st_df, distr_matches = match_st_df))
}



adaptPowoStatus <- function(powo_stat_dt, spec, new_status, tdwgName = NA){
  
  # function to either manually reassign the status assigned to a species name and region or
  # to define POWO species name of which status information should be used in case multiple POWO names corresponded to an LCVP name
  # input:
  #   - powo_stat_dt: data frame with columns 'searched_name', 'tdwgName', 'status' and 'powo_name'
  #   - spec: species name as in column "searched_name" of powo_stat_dt
  #   - new_status: either status that should be assigned ('native', 'introduced' or 'unknown'), or POWO name as in powo_stat_dt whose status information should be used
  #   - tdwgName: name of TDWG region as in column tdwgName of powo_stat_dt, only necessary if new_status is not a POWO name
  # output: 
  #   - df: same as powo_stat_dt, but with modified status for species "spec"
  
  # check columns:
  if(!all(c("searched_name", "tdwgName", "status", "powo_name") %in% colnames(powo_stat_dt))){
    stop("Data frame must contain columns 'searched_name', 'twdgName', 'status' and 'powo_name'.")
  }
  # check if species name is in powo_stat_dt:
  if(!spec %in% powo_stat_dt$searched_name){
    stop("Data frame doesn't contain species name.")
  }
  # check if TDWG region name is in powo_stat_dt:
  if(!is.na(tdwgName) & !tdwgName %in% powo_stat_dt$tdwgName){
    stop("Data frame doesn't contain tdwg region name.")
  }
  
  # reassign status:
  if(new_status %in% c("native", "introduced", "unknown")){
    
    powo_stat_dt <- powo_stat_dt %>% 
      mutate(status = if_else(searched_name == spec & tdwgName == !!tdwgName, new_status, status))
    
    print(paste("changed status of", spec, "in", tdwgName, "to", new_status))
    
  } else{
    # use status information of POWO name defined in new_status:
    
    # check if species name  defined in new_status matches species name:
    powo_names <- powo_stat_dt %>% 
      filter(searched_name == spec) %>% 
      pull(powo_name) %>% 
      unique
    if(!new_status %in% powo_names){
      stop(paste("Status information for", spec, "do not include data of", new_status))
    }
    
    # keep for the species defined in spec only the status information of the POWO name defined in new_status:
    powo_stat_dt <- powo_stat_dt %>% 
      filter(!(searched_name == spec & powo_name != new_status))
    print(paste("used status of", new_status, "for", spec))
  }
  
  return(powo_stat_dt)
}



getGiftNames <- function(spec, incl_lcvp_synonyms = FALSE){
  
  # searches for species name in GIFT dataset, checks species name against LCVP (on which blacklist is based),
  # from GIFT extracts WCVP harmonized species name (same taxonomic backbone as in POWO) if species is also included in LCVP
  
  # input:  
  #   - spec: LCVP based species name
  #   - incl_lcvp_synonyms: TRUE = LCVP search results include names that are considered synonyms in LCVP;
  #                         FALSE = only accepted names in LCVP are considered
  # output: 
  #   - df: searched name - GIFT result genus - GIFT result species epithet
  
  print(spec)
  
  # split name in genus and species epithet:
  spec_gen_epi <- unlist(str_split(spec, pattern = " ", n = 2))
  
  # find searched species name in GIFT:
  GIFT_spec_res <- tryCatch({
    
    GIFT_species_lookup(genus = spec_gen_epi[1], 
                        epithet = spec_gen_epi[2], 
                        namesmatched = TRUE, # TRUE = look for the species not only in the standardized names but also in the original names
                        GIFT_version = "beta")
  },
  error = function(e){
    print(paste("Connection to Gift information failed, try again later."))
    return(data.frame("searched_name" = spec,
                      "GIFT_genus" = NA,
                      "GIFT_species_ep" = NA))})
  
  # GIFT results: species names incl. author before harmonization:
  GIFT_spec_org <- paste(GIFT_spec_res$genus, GIFT_spec_res$species_epithet, GIFT_spec_res$author)
  
  # check against LCVP:
  
  # search LCVP entries connected to the searched species name:
  if(incl_lcvp_synonyms){
    status_ok <- c("accepted", "synonym") # exclude unresolved and external results
  } else {status_ok <- "accepted"} # only accepted species names
  lcvp_spec_res <- unique(lcvp_fuzzy_search(spec, status = status_ok)$Output.Taxon) # lcvp_fuzzy_search from package lcvpplants
  
  # GIFT WCVP harmonized names (column work_species) matching LCVP results:
  GIFT_res_harm <- GIFT_spec_res$work_species[which(GIFT_spec_org %in% lcvp_spec_res)]
  
  # there is no exact match (e.g. due to slightly different names; 6 species of the 122 blacklist species)
  if(length(GIFT_res_harm) == 0){
    
    # find most similar name with fuzzy matching:
    print("No exact match between Gift and LCVP name found. Used fuzzy matching instead. Consider checking the results manually.")
    best_match <- DIFFLIB$get_close_matches(word = lcvp_spec_res, 
                                            possibilities = GIFT_spec_org,
                                            n = as.integer(1), cutoff = 0)
    print(paste("LCVP name:", lcvp_spec_res))
    print(paste("Matched Gift name:", best_match))
    GIFT_res_harm <- GIFT_spec_res$work_species[which(GIFT_spec_org == best_match)]
  }
  
  # split in genus and species epithet:
  GIFT_harm_gen_epi <- unlist(str_split(GIFT_res_harm, pattern = " ", n = 2))
  
  if(length(GIFT_harm_gen_epi) != 0){
    return(data.frame("searched_name" = spec,
                      "GIFT_genus" = GIFT_harm_gen_epi[1],
                      "GIFT_species_ep" = GIFT_harm_gen_epi[2]))
  }else{
    print("Matching GIFT and LCVP name didn't work. Check manually.")
    return(data.frame("searched_name" = spec,
                      "GIFT_genus" = NA,
                      "GIFT_species_ep" = NA))
  }
}



getGiftStatusInf <- function(searched_name, GIFT_spec_genus, GIFT_spec_epithet){
  
  # extracts status information from GIFT
  # input:  
  #   - searched_name: original species name from blacklist, used in output to later join occurrences
  #   - GIFT_spec_genus: genus of GIFT species name
  #   - GIFT_spec_epithet: species epithet of GIFT species name
  # output: 
  #   - df: searched_species, GIFT species - entity_ID (= ID of GIFT polygon) - native - naturalized - endemic_list
  
  # attention: GIFT provides status information for nested regions, e.g. status information for Honshu, but also for Japan in general,
  # whether nested regions should all be retained or how information should be dissolved can be defined within
  # GIFT_species_distribution()
  # for us it makes sense to keep all nested regions in the first place and remove nested regions after merging with occurrences
  # (occurrences may be located at different nesting levels, e.g. not on Honshu but in another location in Japan, if we had dropped Japan in the first place,
  # no status information would be assigned)
  
  print(paste(GIFT_spec_genus, GIFT_spec_epithet))
  
  # find GIFT distribution for harmonized species name: 
  GIFT_spec_distr <- tryCatch({
    
    GIFT_species_distribution(genus = GIFT_spec_genus,
                              epithet = GIFT_spec_epithet, 
                              aggregation = TRUE, # TRUE = only one status per polygon
                              namesmatched = FALSE, # TRUE not necessary since harmonized species name is used
                              #remove_overlap = TRUE, # return only one of overlapping polygons, depending on the rules defined below:
                              #overlap_th = 0.1, # default, if polygons overlap by min. 10 % they are treated as overlapping, if they overlap less, both polygons are kept
                              #area_th_mainland = 0, # overlapping mainlands: use smallest mainland (e.g. use Tanzania rather than East Tropical Africa)
                              #area_th_island = 0, # use smallest island (rather than Island Group, e.g. use Honshu rather than Japan)
                              GIFT_version = "beta") # doesn't allow including author in search
  }, error = function(e){
    print(paste("Connection to Gift information failed, try again later."))
    spec_status_inf <- data.frame(species = searched_name,
                                  GIFT_species = paste(GIFT_spec_genus, GIFT_spec_epithet),
                                  entity_ID = NA,
                                  native = NA,
                                  naturalized = NA,
                                  endemic_list = NA)
    return(spec_status_inf)
  })
  
  # extract and re-format information:
  spec_status_inf <- GIFT_spec_distr %>%
    mutate(species = searched_name) %>%
    mutate(GIFT_species = paste(GIFT_spec_genus, GIFT_spec_epithet)) %>%
    mutate(native = ifelse(native == 1, "native", "non-native"),
           naturalized = ifelse(naturalized == 1, "naturalized",
                                "non-naturalized"),
           endemic_list = ifelse(endemic_list == 1, "endemic_list",
                                 "non-endemic_list"))# %>%
    # select(species, GIFT_species, entity_ID, native, naturalized, endemic_list) %>%
    # filter_at(vars(native, naturalized, endemic_list), any_vars(!is.na(.)))  # remove entries without any status information
  
  return(spec_status_inf)
  
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