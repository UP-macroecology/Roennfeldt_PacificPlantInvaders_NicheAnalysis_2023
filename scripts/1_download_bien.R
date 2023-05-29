
# preamble
rm(list = ls())

# required packages ------------------------------------------------------------
# set up to run on HPC

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org', dep = TRUE)
  require(x, character.only = TRUE)
}
package_vec <- c(
  "BIEN", "doParallel", "fasterize", "foreach", "taxize", "tidyverse", "terra"  # names of the packages required placed here as character objects
)

# install.packages(c("pillar", "phangorn")) # install packages that caused namespace errors before running the previously defined function
sapply(package_vec, install.load.package)



# required paths ------------------------------------------------------------------------
path_import <- file.path("/import","ecoc9", "data-zurell", "roennfeldt", "C1")
# path_mnt <- file.path("/mnt", "ibb_share", "zurell", "biodat", "distribution", "Pacific_invaders")


# -------------------------------------------------- #
#              Define download function           ####
# -------------------------------------------------- #
download_species <- function(spec_name){
  # download BIEN occurrence data for 'spec_name'
  occ_df = BIEN_occurrence_species(spec_name, 
                                   natives.only = FALSE, 
                                   cultivated = TRUE, 
                                   native.status = TRUE, 
                                   political.boundaries = TRUE) 
  if(nrow(occ_df) == 0){ # if no occurrences available --> return NULL
    return(NULL)
  } else { # else --> return only relevant columns
    return(occ_df[,colnames(occ_df) %in% c("scrubbed_species_binomial", "latitude", "longitude", 
                                           "date_collected", "country", "datasource", "dataset",
                                           "is_introduced", "native_status", "native_status_country", 
                                           "native_status_state_province","native_status_reason",
                                           "is_cultivated_observation", "is_cultivated_in_region", # added additional columns to idenity cultivated occs
                                           "is_location_cultivated")])
  }
}

# -------------------------------------------------- #
#          Loop over species and download         ####
# -------------------------------------------------- #

# read in data set on pacific island invaders. Source: https://bdj.pensoft.net/article/67318/
PaciFlora_specs <- read.table(file.path(path_import, "PaciFLora.txt"), header = TRUE) %>%
  dplyr::select(Species) %>%
  distinct() %>%
  drop_na() %>%
  pull(Species)

# read in specs_all, as this contains the names of the species that were already included pre May 2023 (kick-off meeting)
load(file.path(path_import, "specs_all.RData"))


#define list of species for which a download is still required
new_specs <- setdiff(PaciFlora_specs, specs_all)


# collect names of already downloaded (in the import folder) species:
inv_specs_dl <- list.files(file.path(path_import, "download_bien")) %>% 
  str_remove(".RData") %>% 
  str_replace("_", " ")

# create list of still to-be-downloaded species:
inv_specs_final <- setdiff(new_specs, inv_specs_dl)


# set up cluster and download species (may be a good idea to run this on the cluster):
cl = makeCluster(10)
registerDoParallel(cl)

# download data, retry if not successful:
foreach(spec_index = 1:length(inv_specs_final), .packages = c("tidyverse", "BIEN")) %dopar% {
  
  spec_name <- inv_specs_final[spec_index]
  
  download_successful = FALSE
  iter = 0
  occ_df = NULL
  while(!(download_successful) & iter < 5){
    tryCatch({  # sometimes the connection to server is unstable, so wrap occurrence download in tryCatch()
      print(spec_name)
      occ_df = download_species(spec_name) # call above-defined download function with current species
      download_successful = TRUE
      save(occ_df, file = file.path(path_import, "download_bien", paste0(str_replace_all(spec_name, " ", "_"), ".RData"))) # save downloaded object
    }, error = function(e){
      cat("download error:", spec_name, "\n")
      Sys.sleep(15)
      iter <<- iter + 1
    })
  }  
}  

stopCluster(cl)