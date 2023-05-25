
# preamble
rm(list = ls())

# Packages ---------------------------------------------------------------------
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "sf", "doParallel", "foreach", "geojsonsf", "terra" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)

# required patgs ---------------------------------------------------------------

# cluster paths
path_imp <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1")
path_mnt <- file.path("/mnt/ibb_share/zurell/biodat/distribution/Pacific_invaders")

# required data ----------------------------------------------------------------
load(file.path(path_imp,"input_data/specs_all.RData"))
load(file.path(path_imp,"output/all_specs_occ_final.RData"))

# tdwg level 1 gives 9 continental boundaries
tdwg1 <- st_read(file.path(path_imp,"input_data/level1.geojson"))
tdwg_info <- st_drop_geometry(tdwg1)

# shapefiles for pacific 
pacific_islands <- vect(file.path(path_imp,"input_data/pacific_islands.shp"))


# determine regional occurrences -----------------------------------------------

# all_specs_occ_final

# create empty df to add info to 
occ_count_1 <- data.frame(matrix(ncol = 10, nrow = 0))
colnames(occ_count_1) <- c("species", "native_occs", "pac_occs", "eur_occs", "afr_occs", "ate_occs", "atr_occs", "aus_occs", "nam_occs", "sam_occs")

registerDoParallel(cores = 10) # doParallel package
getDoParWorkers()

spp <- specs_all

occ_count_1 <- foreach(spp_index = 1:length(spp), .combine = "rbind") %dopar%
  try({
    
    # reset all counters to NA
    nr_nat <- NA
    nr_pac <- NA
    nr_eur <- NA
    nr_afr <- NA
    nr_ate <- NA
    nr_atr <- NA
    nr_aus <- NA
    nr_nam <- NA
    nr_sam <- NA
    
    # subset occ for current species
    spec_df <- subset(all_specs_occ_final, species == spp[spp_index])
    
    # get native occurrences for which the continental code is not NA
    nat <- subset(spec_df, final_status == "native1" & tdwg_l1_code != is.na(NA))
    nr_nat <- nrow(nat)
    
    # only continue with the rest if there are >= 20 occs in the native range
    if(nr_nat >= 20){
      save(nat, file = paste0(path_imp,"/output/regional_occs/nat1_",spp[spp_index],".RData"))
      
      # get all introduced occurrences for which the continental code is not NA
      intr <- subset(spec_df, final_status == "introduced1" & species == spp[spp_index] & tdwg_l1_code != is.na(NA))
      intr_coords <- terra::vect(data.frame(lon = intr$lon, lat = intr$lat))
      # add crs
      crs(intr_coords) <- crs(tdwg1)
      
      
      # count occurrences over pacific islands and save results
      over_island <- terra::intersect(intr_coords, pacific_islands)
      nr_pac <- length(over_island)
      
      if(nr_pac >= 20){# get coords and use them to select occs from the intr subset
        crds_pac <- as.data.frame(crds(over_island))
        colnames(crds_pac) <- c("lon", "lat")
        pac_df <- semi_join(intr, crds_pac, by = c("lon", "lat")) 
        
        
        save(pac_df, file = paste0(path_imp,"/output/regional_occs/intr1_pac_",spp[spp_index],".RData"))
        
        # get information for remaining regions 
        
        regs <- unique(intr$tdwg_l1_code)
        regs <- regs[!is.na(regs)]
        
        for(reg in regs){
          
          region_name <- tdwg_info[tdwg_info$LEVEL1_COD == reg,2]
          
          # check which region it is and save nr of occs nd the occurrences itself
          if(region_name == "EUROPE"){
            eur_df <- subset(intr, tdwg_l1_code == reg)
            nr_eur <- nrow(eur_df)
            
            # only save results if there are enough occs to begin with to save storage space
            if(nr_eur >= 20){save(eur_df, file = paste0(path_imp,"/output/regional_occs/intr1_eur_",spp[spp_index],".RData"))}
            
          } # end outer if Europe
          
          if(region_name == "AFRICA"){
            afr_df <- subset(intr, tdwg_l1_code == reg)
            nr_afr <- nrow(afr_df)
            
            if(nr_afr >= 20){save(afr_df, file = paste0(path_imp,"/output/regional_occs/intr1_afr_",spp[spp_index],".RData"))}
          } # end outer if Africa
          
          if(region_name == "ASIA-TEMPERATE"){
            ate_df <- subset(intr, tdwg_l1_code == reg)
            nr_ate <- nrow(ate_df)
            
            if(nr_ate >= 20){save(ate_df, file = paste0(path_imp,"/output/regional_occs/intr1_ate_",spp[spp_index],".RData"))}
          } # end outer ifAsia (temperate)
          
          if(region_name == "ASIA-TROPICAL"){
            atr_df <- subset(intr, tdwg_l1_code == reg)
            nr_atr <- nrow(atr_df)
            
            if(nr_atr >= 20){save(atr_df, file = paste0(path_imp,"/output/regional_occs/intr1_atr_",spp[spp_index],".RData"))}
          } # end outer if Asia (temperate)
          
          if(region_name == "AUSTRALASIA"){
            aus_df <- subset(intr, tdwg_l1_code == reg)
            nr_aus <- nrow(aus_df)
            if(nr_aus >= 20){save(aus_df, file = paste0(path_imp,"/output/regional_occs/intr1_aus_",spp[spp_index],".RData"))}
          } # end outer if Australasia
          
          if(region_name == "NORTHERN AMERICA"){
            nam_df <- subset(intr, tdwg_l1_code == reg)
            nr_nam <- nrow(nam_df)
            if(nr_nam >= 20){save(nam_df, file = paste0(path_imp,"/output/regional_occs/intr1_nam_",spp[spp_index],".RData"))}
          } # end outer if Northern America
          
          if(region_name == "SOUTHERN AMERICA"){
            sam_df <- subset(intr, tdwg_l1_code == reg)
            nr_sam <- nrow(sam_df)
            if(nr_sam){save(sam_df, file = paste0(path_imp,"/output/regional_occs/intr1_sam_",spp[spp_index],".RData"))}
          } # end outer if Southern America
          
        } # end of for loop over regs
        
      } # end nr_pac if
      
    } # end nr_nat if
    
    occ_count <- data.frame(species = spp[spp_index],
                            native_occs  = nr_nat,
                            pac_occs = nr_pac,
                            eur_occs = nr_eur,
                            afr_occs = nr_afr,
                            ate_occs = nr_ate,
                            atr_occs = nr_atr,
                            aus_occs = nr_aus,
                            nam_occs = nr_nam,
                            sam_occs = nr_sam)
    # # combine info in df
    # occ_count_1 <- rbind(occ_count_1,
    #                    data.frame(species = spp[spp_index],
    #                               native_occs  = nr_nat,
    #                               pac_occs = nr_pac,
    #                               eur_occs = nr_eur,
    #                               afr_occs = nr_afr,
    #                               ate_occs = nr_ate,
    #                               atr_occs = nr_atr,
    #                               aus_occs = nr_aus,
    #                               nam_occs = nr_nam,
    #                               sam_occs = nr_sam))
    
  }) # end of foreach-try

save(occ_count_1, file = file.path(path_imp, "output/occ_count_criterion1.RData"))