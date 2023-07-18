install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "foreach" , "doParallel"# names of the packages required placed here as character objects
)
sapply(package_vec, install.load.package)


rm(list = ls())


path_imp <- file.path("/import","ecoc9z", "data-zurell", "roennfeldt", "C1")
load(paste0(path_imp, "/input/occ_status_merged.RData"))


# load("data/status_assignment/occ_status_merged.RData")

specs_all <- unique(occ_status_merged$species)

specs <- specs_all[1:20]

no_cores <- 10
cl <- makeCluster(no_cores)
registerDoParallel(cl)


occ_status_resolved <- foreach(s = 1:length(specs), .packages = "dplyr",
                               .combine = "rbind", .verbose = TRUE) %do% {
                                 
                                 occ_specs <- subset(occ_status_merged, species == specs[s])
                                 
                                 conflict_index <- which(occ_specs$status_check != "native" & occ_specs$status_check != "introduced" &
                                                           occ_specs$status_check != "unknown" & !is.na(occ_specs$status_check))
                                 
                                 for (i in conflict_index) {
                                   
                                   if (occ_specs[i,"status_check"] == "W_vs_G_no_Gl") {
                                     
                                     # get sizes of the two areas to compare them (both in km^2)
                                     areas <- data.frame(source = c("wcvp", "gift"),
                                                         area_km2 = c(occ_specs[i, "wcvp_area"], occ_specs[i, "gift_area"]))
                                     
                                     if (!anyNA(areas)) {
                                       
                                       if (areas[1,2] != areas[2,2]) {
                                         # if areas don't have the same size:
                                         
                                         # identify which status source refers to the smaller region
                                         smallest_source <- areas[which.min(areas$area_km2),1]
                                         
                                         # assign status info for both criteria based on this source
                                         occ_specs[i,"criterion_1"] <- occ_specs[i, paste0(smallest_source, "_status")]
                                         occ_specs[i,"criterion_2"] <- occ_specs[i, paste0(smallest_source, "_status")]
                                         
                                       } else {
                                         # if areas have the same size: assign status info for both criteria based on their rules
                                         
                                         # criterion 1 (no source preferences)
                                         occ_specs[i,"criterion_1"] <- "contradictory"
                                         # criterion 2 (WCVP preferred)
                                         occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                         
                                       } # end of ifelse area comparison
                                       
                                     } else {
                                       
                                       # if area information is missing: assign status info for both criteria based on their rules
                                       
                                       # criterion 1 (no source preferences)
                                       occ_specs[i,"criterion_1"] <- "contradictory"
                                       # criterion 2 (WCVP preferred)
                                       occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                       
                                     } # end of iselse around areas
                                     
                                     
                                   } # end of  if "W_vs_G_no_Gl"
                                   
                                   
                                   #'################# 
                                   
                                   if (occ_specs[i,"status_check"] == "W_vs_Gl_no_G") {
                                     # WCVP and GloNAF both refer to tdwg level 3, so if they contradict each other
                                     # in the status assignment, this conflict can not be resolved based on region size
                                     # assign status info for both criteria based on their rules:
                                     
                                     # criterion 1 (no source preferences)
                                     occ_specs[i,"criterion_1"] <- "contradictory"
                                     # criterion 2 (WCVP preferred)
                                     occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                     
                                   } # end of if "W_vs_Gl_no_G"
                                   
                                   #'################# 
                                   
                                   # G_vs_Gl_no_W did not occur as an conflict. Skip this part
                                   
                                   # if (occ_specs[i,"status_check"] == "G_vs_Gl_no_W") {
                                   #   
                                   #   # get sizes of the two areas to compare them (both in km^2)
                                   #   areas <- data.frame(source = c("wcvp", "gift"),
                                   #                       area_km2 = c(occ_specs[i, "wcvp_area"], occ_specs[i, "gift_area"]))
                                   #   
                                   #   
                                   #   if(!anyNA(areas)){
                                   #     
                                   #     if (areas[1,2] != areas[2,2]) {
                                   #       # if areas don't have the same size:
                                   #       
                                   #       # identify which status source refers to the smaller region
                                   #       smallest_source <- areas[which.min(areas$area_km2),1]
                                   #       
                                   #       if (smallest_source == "gift") {
                                   #         # assign status info for both criteria based on gift
                                   #         occ_specs[i,"criterion_1"] <- occ_specs[i, paste0(smallest_source, "_status")]
                                   #         occ_specs[i,"criterion_2"] <- occ_specs[i, paste0(smallest_source, "_status")]
                                   #         
                                   #       } else {
                                   #         # assign status info for both criteria based on glonaf
                                   #         occ_specs[i,"criterion_1"] <- occ_specs[i,"glonaf_status"]
                                   #         occ_specs[i,"criterion_2"] <- occ_specs[i,"glonaf_status"]
                                   #       } # end of else smallest_source
                                   #       
                                   #     } else {
                                   #       # if the areas have the same size:
                                   #       # no preferences between gift and glonaf, assign contradictory for both criteria
                                   #       
                                   #       # criterion 1 (no source preferences)
                                   #       occ_specs[i,"criterion_1"] <- "contradictory"
                                   #       # criterion 2 (WCVP preferred)
                                   #       occ_specs[i,"criterion_2"] <- "contradictory"
                                   #       
                                   #     } # end of else around equal area size 
                                   #     
                                   #   } else {
                                   #     
                                   #     # if the area info is missing:
                                   #     # no preferences between gift and glonaf, assign contradictory for both criteria
                                   #     
                                   #     # criterion 1 (no source preferences)
                                   #     occ_specs[i,"criterion_1"] <- "contradictory"
                                   #     # criterion 2 (WCVP preferred)
                                   #     occ_specs[i,"criterion_2"] <- "contradictory"
                                   #     
                                   #   } # end of ifesle around areas
                                   #   
                                   # } # end of if "G_vs_Gl_no_W"
                                   # 
                                   #'################# 
                                   
                                   if (occ_specs[i,"status_check"] == "Wcvp_vs_rest") {
                                     # this conflict can not be resolved based on region size
                                     # assign status info for both criteria based on their rules:
                                     
                                     # criterion 1 (no source preferences)
                                     occ_specs[i,"criterion_1"] <- "contradictory"
                                     # criterion 2 (WCVP preferred)
                                     occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                     
                                   } # end of if "Wcvp_vs_rest"
                                   
                                   #'################# 
                                   
                                   if (occ_specs[i,"status_check"] == "Glonaf_vs_rest") {
                                     # this conflict can not be resolved based on region size
                                     # assign status info for both criteria based on their rules:
                                     
                                     # criterion 1 (no source preferences)
                                     occ_specs[i,"criterion_1"] <- "contradictory"
                                     # criterion 2 (WCVP preferred)
                                     occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                   } # end of if "GloNAF_vs_rest"
                                   
                                   #'################# 
                                   
                                   if (occ_specs[i,"status_check"] == "Gift_vs_rest") {
                                     
                                     # get sizes of the two areas to compare them (both in km^2)
                                     areas <- data.frame(source = c("wcvp", "gift"),
                                                         area_km2 = c(occ_specs[i, "wcvp_area"], occ_specs[i, "gift_area"]))
                                     
                                     
                                     if (!anyNA(areas)) {
                                       if (areas[1,2] != areas[2,2]) {
                                         # if areas don't have the same size:
                                         
                                         # identify which status source refers to the smaller region
                                         smallest_source <- areas[which.min(areas$area_km2),1]
                                         
                                         # assign status info for both criteria based on this source
                                         occ_specs[i,"criterion_1"] <- occ_specs[i, paste0(smallest_source, "_status")]
                                         occ_specs[i,"criterion_2"] <- occ_specs[i, paste0(smallest_source, "_status")]
                                         
                                       } else {
                                         # if areas have the same size: assign status info for both criteria based on their rules
                                         
                                         # criterion 1 (no source preferences)
                                         occ_specs[i,"criterion_1"] <- "contradictory"
                                         # criterion 2 (WCVP preferred)
                                         occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                         
                                       } # end of else
                                       
                                     } else {
                                       # if area info is missing: assign status info for both criteria based on their rules
                                       
                                       # criterion 1 (no source preferences)
                                       occ_specs[i,"criterion_1"] <- "contradictory"
                                       # criterion 2 (WCVP preferred)
                                       occ_specs[i,"criterion_2"] <- occ_specs[i,"wcvp_status"]
                                       
                                     } # end of ifelse around areas
                                     
                                   } # end of if "GloNAF_vs_rest"
                                   
                                 } # for loop over conflict_index
                                 
                                 occ_specs
                               } # end of foreach

stopCluster(cl)

save(occ_status_resolved, file = paste0(path_imp, "/output/occ_status_resolved.RData"))
