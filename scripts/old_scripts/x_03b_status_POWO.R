#' library(dplyr)
#' library(sf)
#' 
#' source("scripts/functions.R")
#' 
#' # paths -------------------------------------------------------------------
#' 
#' # cluster
#' # path_import <- file.path("/import","ecoc9z", "data-zurell", "roennfeldt", "C1")
#' # path_mnt <- file.path("/mnt", "ibb_share", "zurell", "biodat", "distribution", "Pacific_invaders")
#' 
#' # work laptop
#' path_home <- "M:/C1/data"
#' # path_ds <- "Z:/Arbeit/datashare/data/biodat/distribution/Pacific_invaders"
#' 
#' # home office
#' # path_home <- "Z:/roennfeldt/C1/data"
#' # path_ds <- "Y:AG26/Arbeit/datashare/data/biodat/distribution/Pacific_invaders" 
#' 
#' 
#' # data --------------------------------------------------------------------
#' load("data/status_info/occ_GIFT_status_max_10km_dist.RData")
#' load("data/specs_all.RData")
#' 
#' # TDWG level 3 regions (TDWG = Taxonomic Databases Working Group, Regions: World Geographical Scheme for Recording Plant Distributions: https://github.com/tdwg/wgsrpd)
#' # tdwg <- st_read(file.path(path_mnt,"tdwg_lvl3.geojson"))
#' tdwg <- st_read(file.path(path_home,"TDWG/wgsrpd-master/geojson/level3.geojson"))
#' 
#' 
#' 
#' # 1. get POWO names -------------------------------------------------------
#' 
#' 
#' # remove secies names that always cause errors
#' flagged_names <- c("Acoelorraphe wrightii")
#' specs_all <- specs_all[!specs_all %in% flagged_names]
#' 
#' # prepare empty df to store info 
#' powo_page_inf <- data.frame(searched_name = character(),
#'                             lcvp_name = character(),
#'                             powo_name = character(),
#'                             powo_url = character(),
#'                             ipni_id = character(),
#'                             stringsAsFactors = FALSE)
#' 
#' 
#' # define species for which powo page information has not yet been checked
#' specs_done <- unique(powo_page_inf$searched_name)
#' 
#' specs_left <- setdiff(specs_all, specs_done)
#' 
#' # set counter to keep track on how many species have been through the loop
#' counter <- 0
#' 
#' #run loop over species
#' for(spec in specs_left){
#'   
#'   counter <- counter + 1
#'   print(counter)
#'   
#'   powo_page_inf <- bind_rows(powo_page_inf,
#'                              getPowoNames(spec, incl_lcvp_synonyms = TRUE, perfect_matches_only = TRUE))
#'   
#'   # stop after xx species (e.g. 200)
#'   if (counter >= 320){
#'     break
#'   } # end of if 
#'   
#' } # end of for loop over specs_left
#' 
#' # save(powo_page_inf, file = "data/status_info/powo_page_inf.RData")
#' load("data/status_info/powo_page_inf.RData")
#' 
#' #'#####################################
#' # 1.2. manual selection/correction ---------------------------------------------
#' #'#####################################
#' 
#' # manually select/correct POWO pages for species names for which no perfect match was found in step 1
#' specs_no_powo_inf <- powo_page_inf %>% # for 279 species no matching POWO entry is found
#'   filter(is.na(powo_name)) %>%
#'   pull(searched_name)
#' 
#' 
#' # execute getPowoNames again for these species, but without matching author information (perfect_matches_only = FALSE):
#' # (getPowoNames defined in utils_1-5_dataprep.R)
#' powo_page_opts <- bind_rows(lapply(specs_no_powo_inf, getPowoNames, incl_lcvp_synonyms = TRUE, perfect_matches_only = FALSE))
#' # save(powo_page_opts, file = "data/intermediate/powo_page_opts.RData")
#' load("data/intermediate/powo_page_opts.RData")
#' 
#' 
#' # index locations in powo_page_opts that have been checked and are supposed to be included. 
#' # source for uncertainty might have been differently spelled author names, subspecies, varities etc.
#' index_opts <- c(1, 3, 6, 7, 9, 11, 12, 14, 15, 17, 18, 20, 23, 24, 25, 28, 31, 32, 34, 35, 37, 38, 40, 46, 47, 48, 51, 54, 56,
#'                 57, 58, 61, 62, 64, 65, 66, 68, 69, 71, 74, 75, 81, 84, 86, 87, 88, 90, 91, 92, 93, 95, 96, 97, 98, 99, 101, 104,
#'                 105, 107, 109, 113, 115, 117, 119, 120, 121, 122, 124, 127, 128, 130, 131, 133, 134, 135, 140, 144, 146, 151,
#'                 152, 152, 157, 159, 160, 161, 162, 163, 172, 175, 177, 179, 181, 182, 183, 184, 187, 188, 189, 190, 193, 194, 
#'                 195, 196, 197, 201, 203, 205, 206, 209, 211, 215, 217, 220, 222, 224, 225, 227, 228, 230, 231, 233, 234, 237,
#'                 238, 241, 243, 244, 245, 247, 248, 250, 257, 258, 259, 260, 261, 262, 263, 265, 266, 267, 268, 270, 272, 273,
#'                 274, 279, 281, 285, 287, 288, 289, 292, 295, 296, 297, 300, 304, 305, 306, 307, 310, 311, 312, 313, 315, 316, 
#'                 317, 319, 320, 322, 324, 327, 328, 329, 331, 333, 336, 337, 339, 340)
#' 
#' NA_before <- which(is.na(powo_page_opts$powo_name))
#' 
#' # manually assign POWO pages and IPNI-IDs to NA locations (obviously not an elegant solution, but it works for now)
#' # I used the names in the "searched_name" column to manually check for matching results on the POWO website
#' powo_page_opts[4,3] <- "Acaena novae-zelandiae Kirk"
#' powo_page_opts[4,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:720063-1"
#' powo_page_opts[4,5] <- "720063-1"
#' 
#' powo_page_opts[27,3] <- "Anoda cristata (L.) Schltdl."
#' powo_page_opts[27,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:1218005-2" 
#' powo_page_opts[27,5] <- "1218005-2"
#' 
#' powo_page_opts[39,3] <- "Blechnum orientale L."
#' powo_page_opts[39,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:17059900-1"
#' powo_page_opts[39,5] <- "17059900-1"
#' 
#' powo_page_opts[42,3] <- "Urochloa eminii (Mez) Davidse"
#' powo_page_opts[42,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:974586-1" 
#' powo_page_opts[42,5] <- "974586-1"
#' 
#' powo_page_opts[44,3] <- "Urochloa dictyoneura (Fig. & De Not.) Veldkamp"
#' powo_page_opts[44,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:992521-1"
#' powo_page_opts[44,5] <- "992521-1"
#' 
#' powo_page_opts[45,3] <- "Urochloa distachyos (L.) T.Q.Nguyen" 
#' powo_page_opts[45,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:426250-1"
#' powo_page_opts[45,5] <- "426250-1"
#' 
#' powo_page_opts[53,3] <- "Melaleuca citrina (Curtis) Dum.Cours."
#' powo_page_opts[53,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77108602-1"
#' powo_page_opts[53,5] <- "77108602-1"
#' 
#' powo_page_opts[59,3] <- "Camonea umbellata (L.) A.R.Simões & Staples" 
#' powo_page_opts[59,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77163301-1"
#' powo_page_opts[59,5] <- "77163301-1"
#' 
#' powo_page_opts[60,3] <- "Canna × hybrida Rodigas"
#' powo_page_opts[60,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77208084-1"
#' powo_page_opts[60,5] <- "77208084-1"
#' 
#' powo_page_opts[76,3] <- "Citharexylum svensonii Moldenke" 
#' powo_page_opts[76,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:59545-2"
#' powo_page_opts[76,5] <- "59545-2"
#' 
#' powo_page_opts[78,3] <- "Citrus × aurantiifolia (Christm.) Swingle" 
#' powo_page_opts[78,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:59599-2"
#' powo_page_opts[78,5] <- "59599-2"
#' 
#' powo_page_opts[79,3] <- "Citrus × aurantium L."
#' powo_page_opts[79,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:59600-2"
#' powo_page_opts[79,5] <- "59600-2"
#' 
#' powo_page_opts[80,3] <- "Citrus × taitensis Risso"
#' powo_page_opts[80,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:772070-1"
#' powo_page_opts[80,5] <- "772070-1"
#' 
#' powo_page_opts[94,3] <- "Crocosmia × crocosmiiflora (Lemoine) N.E.Br." 
#' powo_page_opts[94,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:436427-1"
#' powo_page_opts[94,5] <- "436427-1"
#' 
#' powo_page_opts[110,3] <- "Decalobanthus peltatus (L.) A.R.Simões & Staples"
#' powo_page_opts[110,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77163209-1"
#' powo_page_opts[110,5] <- "77163209-1"
#' 
#' powo_page_opts[111,3] <- "Derris montana Benth."
#' powo_page_opts[111,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:491376-1"
#' powo_page_opts[111,5] <- "491376-1"
#' 
#' powo_page_opts[112,3] <- "Desmodium uncinatum (Jacq.) DC." 
#' powo_page_opts[112,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:1109494-2"
#' powo_page_opts[112,5] <- "1109494-2"
#' 
#' powo_page_opts[114,3] <- "Digitaria ciliaris (Retz.) Koeler" 
#' powo_page_opts[114,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:1038558-2"
#' powo_page_opts[114,5] <- "1038558-2"
#' 
#' powo_page_opts[118,3] <- "Distimake tuberosus (L.) A.R.Simões & Staples"
#' powo_page_opts[118,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77163293-1"
#' powo_page_opts[118,5] <- "77163293-1"
#' 
#' powo_page_opts[125,3] <- "Epidendrum × obrienianum Rolfe" 
#' powo_page_opts[125,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:632357-1"
#' powo_page_opts[125,5] <- "632357-1"
#' 
#' powo_page_opts[139,3] <- "Urceolina × grandiflora (Planch. & Linden) Traub"
#' powo_page_opts[139,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:261579-2"
#' powo_page_opts[139,5] <- "261579-2"
#' 
#' powo_page_opts[145,3] <- "Fallopia convolvulus (L.) Á.Löve" 
#' powo_page_opts[145,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:103354-2"
#' powo_page_opts[145,5] <- "103354-2"
#' 
#' powo_page_opts[148,3] <- "Gamochaeta americana (Mill.) Wedd." 
#' powo_page_opts[148,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:1213296-2"
#' powo_page_opts[148,5] <- "1213296-2"
#' 
#' powo_page_opts[155,3] <- "Gladiolus × hortulanus L.H.Bailey"
#' powo_page_opts[155,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:437518-1"
#' powo_page_opts[155,5] <- "437518-1"
#' 
#' powo_page_opts[166,3] <- "Heliocarpus americanus L."
#' powo_page_opts[166,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:834663-1" 
#' powo_page_opts[166,5] <- "834663-1"
#' 
#' powo_page_opts[167,3] <- "Heliotropium arboreum (Blanco) Mabb."
#' powo_page_opts[167,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77164241-1" 
#' powo_page_opts[167,5] <- "77164241-1"
#' 
#' powo_page_opts[185,3] <- "Cyperus mindorensis (Steud.) Huygh" 
#' powo_page_opts[185,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77140538-1"
#' powo_page_opts[185,5] <- "77140538-1"
#' 
#' powo_page_opts[186,3] <- "Lardizabala funaria (Molina) Looser" 
#' powo_page_opts[186,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:135596-2"
#' powo_page_opts[186,5] <- "135596-2"
#' 
#' powo_page_opts[192,3] <- "Leucanthemum × superbum (Bergmans ex J.W.Ingram) D.H.Kent" 
#' powo_page_opts[192,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:947628-1"
#' powo_page_opts[192,5] <- "947628-1"
#' 
#' powo_page_opts[199,3] <- "Cucumis melo L."
#' powo_page_opts[199,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:292238-1"
#' powo_page_opts[199,5] <- "292238-1"
#' 
#' powo_page_opts[200,3] <- "Lupinus × hybridus Lem." 
#' powo_page_opts[200,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:504577-1"
#' powo_page_opts[200,5] <- "504577-1"
#' 
#' powo_page_opts[207,3] <- "Melastoma malabathricum subsp. malabathricum"
#' powo_page_opts[207,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77178047-1"
#' powo_page_opts[207,5] <- "77178047-1"
#' 
#' powo_page_opts[208,3] <- "Mentha × piperita L." 
#' powo_page_opts[208,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:450969-1"
#' powo_page_opts[208,5] <- "450969-1"
#' 
#' powo_page_opts[216,3] <- "Phymatosorus scolopendria (Burm.f.) Pic.Serm."
#' powo_page_opts[216,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:17448130-1"
#' powo_page_opts[216,5] <- "17448130-1"
#' 
#' powo_page_opts[219,3] <- "Mimosa pigra var. pigra"
#' powo_page_opts[219,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77190600-1"
#' powo_page_opts[219,5] <- "77190600-1"
#' 
#' powo_page_opts[221,3] <- "Musa × paradisiaca L."
#' powo_page_opts[221,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:797595-1"
#' powo_page_opts[221,5] <- "797595-1"
#' 
#' powo_page_opts[226,3] <- "Trimezia gracilis (Herb.) Christenh. & Byng" 
#' powo_page_opts[226,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77182400-1"
#' powo_page_opts[226,5] <- "77182400-1"
#' 
#' powo_page_opts[229,3] <- "Nymphaea × daubenyana W.T.Baxter ex Daubeny" 
#' powo_page_opts[229,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:928088-1"
#' powo_page_opts[229,5] <- "928088-1"
#' 
#' powo_page_opts[236,3] <- "Oxalis corniculata L."
#' powo_page_opts[236,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:177893-2"
#' powo_page_opts[236,5] <- "177893-2"
#' 
#' powo_page_opts[240,3] <- "Falcataria falcata (L.) Greuter & R.Rankin"
#' powo_page_opts[240,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:60471823-2"
#' powo_page_opts[240,5] <- "60471823-2"
#' 
#' powo_page_opts[251,3] <- "Phoenix × intermedia Naudin ex Becc." 
#' powo_page_opts[251,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77244580-1"
#' powo_page_opts[251,5] <- "77244580-1"
#' 
#' powo_page_opts[252,3] <- "Phyllanthus amarus Schumach. & Thonn." 
#' powo_page_opts[252,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:326408-2"
#' powo_page_opts[252,5] <- "326408-2"
#' 
#' powo_page_opts[253,3] <- "Phyllanthus airy-shawii Jean F.Brunel & J.P.Roux" 
#' powo_page_opts[253,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:904933-1"
#' powo_page_opts[253,5] <- "904933-1"
#' 
#' powo_page_opts[254,3] <- "Phyllanthus urinaria L." 
#' powo_page_opts[254,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:354815-1"
#' powo_page_opts[254,5] <- "354815-1"
#' 
#' powo_page_opts[255,3] <- "Phyllanthus virgatus G.Forst."
#' powo_page_opts[255,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:354846-1"
#' powo_page_opts[255,5] <- "354846-1"
#' 
#' powo_page_opts[256,3] <- "Phymatosorus scolopendria (Burm.f.) Pic.Serm."
#' powo_page_opts[256,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:17448130-1"
#' powo_page_opts[256,5] <- "17448130-1"
#' 
#' powo_page_opts[264,3] <- "Pseuderanthemum maculatum (G.Lodd.) I.M.Turner"
#' powo_page_opts[264,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77158889-1"
#' powo_page_opts[264,5] <- "77158889-1"
#' 
#' powo_page_opts[269,3] <- "Pyracantha crenulata (D.Don) M.Roem" 
#' powo_page_opts[269,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:730613-1"
#' powo_page_opts[269,5] <- "730613-1"
#' 
#' powo_page_opts[271,3] <- "Raphanus raphanistrum subsp. sativus (L.) Domin"
#' powo_page_opts[271,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77159305-1"
#' powo_page_opts[271,5] <- "77159305-1"
#' 
#' powo_page_opts[280,3] <- "Rhodopentas bussei (K.Krause) Kårehed & B.Bremer"
#' powo_page_opts[280,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77086243-1"
#' powo_page_opts[280,5] <- "77086243-1"
#' 
#' powo_page_opts[282,3] <- "Rosa × damascena Herrm."
#' powo_page_opts[282,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:2968261-4"
#' powo_page_opts[282,5] <- "2968261-4"
#' 
#' powo_page_opts[283,3] <- "Roseodendron donnell-smithii (Rose) Miranda"
#' powo_page_opts[283,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:222518-2"
#' powo_page_opts[283,5] <- "222518-2"
#' 
#' powo_page_opts[284,3] <- "Ruellia longepetiolata (Oerst.) Hemsl." 
#' powo_page_opts[284,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:54242-1"
#' powo_page_opts[284,5] <- "54242-1"
#' 
#' powo_page_opts[290,3] <- "Sanchezia oblonga Ruiz & Pav." 
#' powo_page_opts[290,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:54890-1"
#' powo_page_opts[290,5] <- "54890-1"
#' 
#' powo_page_opts[291,3] <- "Scaevola taccada (Gaertn.) Roxb."
#' powo_page_opts[291,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:384339-1"
#' powo_page_opts[291,5] <- "384339-1"
#' 
#' powo_page_opts[293,3] <- "Sesuvium revolutifolium Ortega"
#' powo_page_opts[293,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:364622-1"
#' powo_page_opts[293,5] <- "364622-1"
#' 
#' powo_page_opts[294,3] <- "Sida pusilla Cav." 
#' powo_page_opts[294,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:564120-1"
#' powo_page_opts[294,5] <- "564120-1"
#' 
#' powo_page_opts[301,3] <- "Sorghum × drummondii (Nees ex Steud.) Millsp. & Chase"
#' powo_page_opts[301,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:1058910-2"
#' powo_page_opts[301,5] <- "1058910-2"
#' 
#' powo_page_opts[302,3] <- "Sporobolus pilifer (Trin.) Kunth" 
#' powo_page_opts[302,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:422879-1"
#' powo_page_opts[302,5] <- "422879-1"
#' 
#' powo_page_opts[309,3] <- "Symphyotrichum expansum (Poepp. ex Spreng.) G.L.Nesom"
#' powo_page_opts[309,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:981816-1"
#' powo_page_opts[309,5] <- "981816-1"
#' 
#' powo_page_opts[314,3] <- "Tagetes erecta L." 
#' powo_page_opts[314,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:252092-1"
#' powo_page_opts[314,5] <- "252092-1"
#' 
#' powo_page_opts[321,3] <- "Chaetogastra herbacea (DC.) P.J.F.Guim. & Michelang."
#' powo_page_opts[321,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77205941-1"
#' powo_page_opts[321,5] <- "77205941-1"
#' 
#' powo_page_opts[323,3] <- "Tilia × europaea L." 
#' powo_page_opts[323,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:835306-1"
#' powo_page_opts[323,5] <- "835306-1"
#' 
#' powo_page_opts[332,3] <- "Urceolina × grandiflora (Planch. & Linden) Traub" 
#' powo_page_opts[332,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:261579-2"
#' powo_page_opts[332,5] <- "261579-2"
#' 
#' powo_page_opts[335,3] <- "Urochloa distachyos (L.) T.Q.Nguyen" 
#' powo_page_opts[335,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:426250-1"
#' powo_page_opts[335,5] <- "426250-1"
#' 
#' powo_page_opts[338,3] <- "Xanthium chinense Mill." 
#' powo_page_opts[338,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:260826-1"
#' powo_page_opts[338,5] <- "260826-1"
#' 
#' NA_changed <- setdiff(NA_before, which(is.na(powo_page_opts$powo_name)))
#' 
#' # correcting errors:
#' 
#' # "wrong" lcvp name and accordingly the wrong POWO page
#' powo_page_opts[67,2] <- NA
#' powo_page_opts[67,3] <- "Cenchrus americanus (L.) Morrone"
#' powo_page_opts[67,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77105978-1"
#' powo_page_opts[67,5] <- "77105978-1"
#' 
#' # wrong synomym, again as a result from the assigned lcvp name 
#' powo_page_opts[83,2] <- NA
#' powo_page_opts[83,3] <- "Miconia crenata (Vahl) Michelang."
#' powo_page_opts[83,4] <- "https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:77164247-1"
#' powo_page_opts[83,5] <- "77164247-1"
#' 
#' index_corrected <- c(67, 83)
#' 
#' # select the previously defined POWO pages that likely match the species names:
#' powo_page_manually_matched <- powo_page_opts[c(index_opts, NA_changed, index_corrected),] # actual index values would be different
#' 
#' # all POWO pages from which status information will be obtained:
#' powo_page_inf_final <- powo_page_inf %>%
#'   filter(!is.na(powo_name)) %>%
#'   rbind(powo_page_manually_matched)
#' 
#' 
#' save(powo_page_inf_final, file = "data/status_info/powo_page_inf_final.RData")
#' 
#' 
#' # free up memory
#' rm(occ_cleaned_slim)
#' rm(powo_page_opts)
#' rm(powo_page_inf)
#' rm(index_corrected)
#' rm(index_opts)
#' rm(NA_changed)
#' rm(NA_before)
#' 
#' 
#' 
#' 
#' # 2. get POWO status ------------------------------------------------------
#' 
#' # (getPowoStatus defined in functions.R)
#' 
#' # use IPNI IDs to get distribution information from POWO:
#' # IPNI = International Plant Name Index: https://www.ipni.org/
#' powo_status_inf <- bind_rows(apply(powo_page_inf_final, 1,
#'                                    function(x){getPowoStatus(x["ipni_id"], x["lcvp_name"])}))
#' 
#' # join with species names:
#' powo_status_inf <- powo_status_inf %>%
#'   left_join(powo_page_inf_final) %>%
#'   select(-powo_url) # 198719 entries
#' 
#' save(powo_status_inf, file = "data/status_info/powo_status_inf.RData")
#' 
#' # 3. check for conflicts --------------------------------------------------
#' 
#' #  relevant if more than one POWO page is found for a species name:
#' # (checkPowoStatus defined in utils_1-5_dataprep.R)
#' powo_status_check <- lapply(specs_all, checkPowoStatus, powo_stat_dt = powo_status_inf)
#' # contradictions:
#' powo_status_contr <- map_dfr(powo_status_check, ~.x$distr_contr) # 2035 entries for 190 unique species 
#' 
#' save(powo_status_contr, file = "results/intermediate/powo_status_contr.RData")
#' 
#' # agreements:
#' powo_status_match <- map_dfr(powo_status_check, ~.x$distr_match) # 80343
#' 
#' # # 3.2) manually re-assign some (or all) conflicting status information:
#' # # (adaptPowoStatus defined in utils_1-5_dataprep.R)
#' # ## (all that are not reassigned, get status "native" in the next step)
#' # ## (in this example, in case of conflicting information, only the status information of those species names is used that the LCVP search would yield if only accepted species names, and not also synonyms, were considered)
#' # # these examples are from the initial example developed by Katrin Schifferle
#' # powo_status_inf_mod <- adaptPowoStatus(spec = "Allamanda schottii", new_status = "Allamanda schottii Pohl", 
#' #                                        powo_stat_dt = powo_status_inf) #!
#' # powo_status_inf_mod <- adaptPowoStatus(spec = "Alysicarpus vaginalis", new_status = "Alysicarpus vaginalis (L.) DC.", 
#' #                                        powo_stat_dt = powo_status_inf_mod)  #!
#' # powo_status_inf_mod <- adaptPowoStatus(spec = "Asclepias curassavica", new_status = "Asclepias curassavica L.", 
#' #                                        powo_stat_dt = powo_status_inf_mod)  #!
#' # powo_status_inf_mod <- adaptPowoStatus(spec = "Chloris radiata", new_status = "Chloris radiata (L.) Sw.", 
#' #                                        powo_stat_dt = powo_status_inf_mod)  #!
#' # powo_status_inf_mod <- adaptPowoStatus(spec = "Crotalaria pallida", new_status = "Crotalaria pallida Aiton", 
#' #                                        powo_stat_dt = powo_status_inf_mod)  #!
#' # powo_status_inf_mod <- adaptPowoStatus(spec = "Rumex spinosus", new_status = "Rumex spinosus L.", 
#' #                                        powo_stat_dt = powo_status_inf_mod)  #!
#' # powo_status_inf_mod <- adaptPowoStatus(spec = "Tribulus terrestris", new_status = "Tribulus terrestris L.", 
#' #                                        powo_stat_dt = powo_status_inf_mod)  #!
#' # powo_status_inf_mod <- adaptPowoStatus(spec = "Vitis vinifera", new_status = "Vitis vinifera L.", 
#' #                                        powo_stat_dt = powo_status_inf_mod)  #!
#' 
#' 
#' # create the object called "powo_status_inf_mod" which should be identical to powo_status_inf (because I'm for now not manually reassigning the info)
#' powo_status_inf_mod <- powo_status_inf
#' 
#' 
#' 
#' # 4. create final df ---------------------------------------------------------
#' 
#' powo_dt <- powo_status_inf_mod %>% #!
#'   relocate(searched_name, tdwgName, tdwgCode, status, lcvp_name, powo_name, ipni_id) %>%
#'   distinct(searched_name, tdwgCode, status, .keep_all = TRUE) %>% # remove duplicates, resulting from matching status information of multiple POWO pages found for one blacklist species name
#'   group_by(searched_name, tdwgCode) %>% # assign all conflicting status information (that have not been manually reassigned in the previous step) the status "native"
#'   mutate(n = n()) %>%
#'   mutate(status = if_else(n == 2, "native", status)) %>%
#'   select(-n) %>%
#'   ungroup %>%
#'   distinct()  # remove duplicates, resulting from assigning all conflicting status information the status "native"
#' 
#' #save(powo_dt, file = "data/status_info/powo_status_information.RData")
#' load("data/status_info/powo_status_information.RData")
#' 
#' # free memory:
#' rm(powo_status_check)
#' rm(specs_no_powo_inf)
#' rm(powo_status_inf)
#' rm(powo_status_inf_mod)
#' 
#' 
#' 
#' # 5. harmonise region names  ----------------------------------------------
#' 
#' # (align POWO region names to TDWG region names)
#' 
#' POWO_regions <- sort(unique(powo_dt$tdwgName))
#' TDWG_regions <- sort(unique(tdwg$LEVEL3_NAM))
#' setdiff(POWO_regions, TDWG_regions)
#' setdiff(TDWG_regions, POWO_regions)
#' 
#' powo_dt_harmonized <- powo_dt %>%
#'   mutate(tdwgName = recode(tdwgName,
#'                            `Amsterdam-St.Paul Is` = "Amsterdam-St.Paul Is.",
#'                            `Central African Repu` = "Central African Republic",
#'                            `Central American Pac` = "C. American Pacific Is.",
#'                            `Central European Rus` = "Central European Russia",
#'                            `Cocos (Keeling) Is.` = "Cocos (Keeling) I.",
#'                            `Gambia` = "Gambia, The",
#'                            `Kirgizstan` = "Kirgizistan",
#'                            `Leeward Is.`  = "Leeward Is. AB Ant",
#'                            `Marion-Prince Edward` = "Marion-Prince Edward Is.",
#'                            `Mozambique Channel I` = "Mozambique Channel Is.",
#'                            `North European Russi` = "North European Russia",
#'                            `Northwest European R` = "Northwest European Russia",
#'                            `Northwest Territorie` = "Northwest Territories",
#'                            `Panamá` = "Panama",
#'                            `South European Russi` = "South European Russia",
#'                            `Suriname` = "Surinam"))
#' 
#' POWO_regions2 <- sort(unique(powo_dt_harmonized$tdwgName))
#' setdiff(POWO_regions2, TDWG_regions) # should be empty (character(0))!
#' setdiff(TDWG_regions, POWO_regions2) # doesn't need to be empty; regions without occurrences of blacklist species
#' 
#' save(powo_dt_harmonized, file = "data/status_info/powo_dt_harmonized.RData")
#' 
#' # free memory:
#' rm(powo_dt)
#' rm(POWO_regions)
#' rm(TDWG_regions)
#' rm(POWO_regions2)
#' 


# Cluster: merge POWO with GIFT assignment -----------------------------------------

# paths
path_imp <- file.path("/import/ecoc9z/data-zurell/roennfeldt/C1")

## Packages ---------------------------------------------------------------
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c(
  "dplyr", "tidyverse", "sf", "lwgeom" # names of the packages required placed here as character objects
)

sapply(package_vec, install.load.package)


# load required data -----------------------------------------------------------
load(file.path(path_imp, "input_data", "specs_all.RData"))
load(file.path(path_imp, "input_data", "occ_cleaned_slim.RData"))
load(file.path(path_imp, "input_data", "powo_dt_harmonized.RData"))
load(file.path(path_imp, "output", "occ_GIFT_status_max_10km_dist.RData"))
tdwg <- st_read(file.path(path_imp, "input_data", "tdwg_lvl3.geojson"))

# switch off spherical geometry package S2, otherwise join doesn't work due to invalid geometries
# sf_use_s2(FALSE) 

# 
# # 1. spatial joining ------------------------------------
# 
# # join occurrences and TDWG level 3 regions spatially
# 
# # convert occurrences to spatial data
# occ_sf <-  occ_cleaned_slim %>%
#   filter(species %in% specs_all) %>%
#   st_as_sf(coords = c("lon", "lat"), crs = st_crs(tdwg)) #  26,186,010 occurrences for all species (old: 1'122'764 occurrences of species in blacklist)
# 
# 
# # join TDWG region in which an occurrence is located:
# sf_use_s2(FALSE) # switch off spherical geometry package S2, otherwise join doesn't work due to invalid geometries
# occ_tdwg_sf <- st_join(occ_sf, tdwg, st_intersects, left = TRUE) # all occurrences are kept, also those that don't intersect with a region
# # 26,243,289 occurrences (old: 1,125,053), ca. 57,279 occurrences joined to two regions (when regions overlap at the occurrence's location)
# 
# save(occ_tdwg_sf, file = paste0(path_imp, "/output/occurrences_tdwg_region_sf.RData"))
# 
# 
# # 1.1 additionally join occurrences which don't overlap with TDWG region to nearest TDWG region if it is not more than 10 km away:
# 
# # the outlines of the TDWG regions are defined rather roughly, there are gaps between
# # adjacent regions and they don't follow the coastlines exactly, thus there are occurrences
# # which don't spatially overlap with any TDWG region, these later get status 'unknown',
# # to reduce number of occurrences with unknown status, we match occurrence to the nearest TDWG regions,
# # if they are less than 10 km apart (10 km = maximum coordinate uncertainty, defined in 3_occurrence_cleaning.R)
# 
# # occurrences that don't intersect with a TDWG level 3 region:
# occ_no_tdwg_sf <- occ_tdwg_sf %>%
#   filter(is.na(LEVEL3_COD)) %>%
#   select(occ_id, species, LEVEL3_NAM, LEVEL3_COD, LEVEL2_COD, LEVEL1_COD) # 2,144,549 (82'082 occurrences)
# 
# # nearest TDWG region for each of these occurrences:
# nearest_tdwg <- st_nearest_feature(x = occ_no_tdwg_sf, y = tdwg, longlat = TRUE) # returns index of nearest feature
# nearest_tdwg_sf <- tdwg[nearest_tdwg,]
# 
# # distance of each occurrence to nearest TDWG region (calculation may take a while!):
# dist_nearest_tdwg <- st_distance(occ_no_tdwg_sf,
#                                  nearest_tdwg_sf,
#                                  by_element = TRUE,
#                                  tolerance = units::set_units(10000, m)) # first distance smaller than tolerance will be returned, true distance may be smaller
# 
# # use nearest TDWG region, if distance is not larger than 10 km:
# dist_okay <- which(dist_nearest_tdwg <= units::set_units(10000, m))
# nearest_tdwg <- nearest_tdwg_sf %>% st_drop_geometry
# occ_no_tdwg <- occ_no_tdwg_sf %>% st_drop_geometry
# occ_no_tdwg[dist_okay, c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")] <- nearest_tdwg[dist_okay, c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")]
# 
# # insert into complete data set:
# occ_tdwg_sf[is.na(occ_tdwg_sf$LEVEL3_COD), c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")] <- occ_no_tdwg[,c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")]
# # 9332 occurrences without TDWG region
# 
# save(occ_tdwg_sf, file = paste0(path_imp, "/output/occ_tdwg_sf.RData"))
# 
# # free memory:
# rm(occ_no_tdwg)
# rm(occ_no_tdwg_sf)
# rm(nearest_tdwg)
# rm(nearest_tdwg_sf)
# rm(dist_nearest_tdwg)
# rm(dist_okay)
# 
# # 2. join occs and status based on TDWG region: --------------------------------
# 
# occ_POWO_status <- occ_tdwg_sf %>%
#   mutate(lon = st_coordinates(.)[,1],lat = st_coordinates(.)[,2]) %>% # add coordinates to data frame for plotting
#   st_drop_geometry() %>%
#   select(-c(country, year, datasource, dataset, native)) %>%
#   left_join(powo_dt_harmonized, by = c("species" = "searched_name", "LEVEL3_NAM" = "tdwgName")) %>% # join occurrences and status, all occurrences included
#   group_by(occ_id) %>%         # some occurrences have been matched to 2 TDWG regions
#   arrange(desc(status)) %>%    # so use only one status (priority: native > introduced > NA)
#   slice(1) %>%                 # this operation is somewhat expensive, so be careful with >> 1M records
#   ungroup() %>%
#   mutate(status = replace_na(status, "unknown")) %>% # all occurrences which could not be matched to a status get status "unknown"
#   select(occ_id, lon, lat, species, tdwg_l3_name = LEVEL3_NAM, tdwg_l3_code = LEVEL3_COD,
#          tdwg_l2_code = LEVEL2_COD, tdwg_l1_code = LEVEL1_COD, status_POWO = status,
#          lcvp_name, powo_name, ipni_id)
# 
# save(occ_POWO_status, file = paste0(path_imp, "/output/occ_POWO_status.RData"))

load(file.path(path_imp, "output", "occ_POWO_status.RData"))

# combine POWO and GIFT ---------------------------------------------------

occ_status_POWO_GIFT <- left_join(occ_POWO_status,
                                  occ_GIFT_status,
                                  by = "occ_id") %>%
  rename(c(species = species.x, GIFT_polygon_ID = entity_ID, GIFT_polygon = geo_entity,
           GIFT_polygon_area = area, GIFT_polygon_source = polygon_source)) %>%
  select(-species.y)

save(occ_status_POWO_GIFT, file = paste0(path_imp, "/output/occ_status_POWO_GIFT.RData"))

