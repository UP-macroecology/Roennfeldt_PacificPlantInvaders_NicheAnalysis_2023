library(lcvplants)
library(GIFT) # install from GitHub with: devtools::install_github("https://github.com/BioGeoMacro/GIFT")
# library(reticulate) # R interface to Python modules
DIFFLIB <- reticulate::import("difflib") # load Python module



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
                                 "non-endemic_list")) %>%
    select(species, GIFT_species, entity_ID, native, naturalized, endemic_list) %>%
    filter_at(vars(native, naturalized, endemic_list), any_vars(!is.na(.)))  # remove entries without any status information
  
  return(spec_status_inf)
  
}



# modCorrplot -------------------------------------------------------------

corrplot_mod = function(m_imp,
                    m_efs,
                    method = c('circle', 'square', 'ellipse', 'number', 'shade', 'color', 'pie'),
                    type = c('full', 'lower', 'upper'), col = NULL, col.lim = NULL, is.corr = TRUE,
                    bg = 'white',   title = '', add = FALSE, diag = TRUE, outline = FALSE,
                    mar = c(0, 0, 0, 0),
                    
                    col_names = colnames(m_efs),
                    row_names = rownames(m_efs),
                    
                    r2names = NULL,
                    
                    addgrid.col = "black", addCoef.col = NULL, addCoefasPercent = FALSE,
                    
                    order = c('original', 'AOE', 'FPC', 'hclust', 'alphabet'),
                    hclust.method = c('complete', 'ward', 'ward.D', 'ward.D2', 'single',
                                      'average', 'mcquitty', 'median', 'centroid'),
                    addrect = NULL, rect.col = 'black', rect.lwd = 2,
                    
                    tl.pos = NULL, tl.cex = 1,
                    tl.col = 'red', tl.offset = 0.5, tl.srt = 45,
                    r.print = NULL,
                    cl.pos = NULL, cl.length = NULL, cl.cex = 0.8,
                    cl.ratio = 0.15, cl.align.text = 'c', cl.offset = 0.5,
                    
                    number.cex = 1, number.font = 2, number.digits = NULL,
                    
                    addshade = c('negative', 'positive', 'all'),
                    shade.lwd = 1, shade.col = 'white',
                    
                    transKeepSign = TRUE,
                    
                    p.mat = NULL, sig.level = 0.05,
                    insig = c('pch', 'p-value', 'blank', 'n', 'label_sig'),
                    pch = 4, pch.col = 'black', pch.cex = 3,
                    
                    plotCI = c('n', 'square', 'circle', 'rect'),
                    lowCI.mat = NULL, uppCI.mat = NULL,
                    na.label = '?', na.label.col = 'black',
                    win.asp = 1,
                    ...)
{
  
  # checking multi-option input parameters
  method = match.arg(method)
  type = match.arg(type)
  order = match.arg(order)
  hclust.method = match.arg(hclust.method)
  addshade = match.arg(addshade)
  insig = match.arg(insig)
  plotCI = match.arg(plotCI)
  
  
  # rescale symbols within the corrplot based on win.asp parameter
  if (win.asp != 1 && !(method %in% c('circle', 'square'))) {
    stop('Parameter \'win.asp\' is supported only for circle and square methods.')
  }
  asp_rescale_factor = min(1, win.asp) / max(1, win.asp)
  stopifnot(asp_rescale_factor >= 0 && asp_rescale_factor <= 1)
  
  
  if (is.null(col.lim)) {
    if (is.corr) {
      # if the matrix is expected to be a correlation matrix
      # it MUST be within the interval [-1,1]
      col.lim = c(-1, 1)
    } else {
      # Issue #91
      # if not a correlation matrix and the diagonal is hidden,
      # we need to compute limits from all cells except the diagonal
      
      if(!diag) {
        diag(m_efs) = NA
      }
      
      col.lim = c(min(m_efs, na.rm = TRUE), max(m_efs, na.rm = TRUE))
    }
  }
  
  
  if (!is.corr) {
    
    c_max = max(m_efs, na.rm = TRUE)
    c_min = min(m_efs, na.rm = TRUE)
    
    # if((col.lim[1] > c_min) | (col.lim[2] < c_max))
    # {
    #   stop('Wrong color: matrix should be in col.lim interval!')
    # }
    
    if(diff(col.lim)/(c_max - c_min) > 2) {
      warning('col.lim interval too wide, please set a suitable value')
    }
    
    # all negative or positive or NOT transkeepSign, trans to [0, 1]
    if (c_max <= 0 | c_min>=0 | !transKeepSign) {
      intercept = - col.lim[1]
      zoom = 1 / (diff(col.lim))
      
      #if(col.lim[1] * col.lim[2] < 0) {
      #  warning('col.lim interval not suitable to the matrix')
      #}
      
    }
    
    
    # mixed negative and positive, remain its sign, e.g. [-0.8, 1] or [-1, 0.7]
    else {
      
      # expression from the original code as a sanity check
      stopifnot(c_max * c_min < 0)
      # newly derived expression which covers the single remaining case
      stopifnot(c_min < 0 && c_max > 0)
      
      
      
      intercept = 0
      zoom = 1 / max(abs(col.lim))
      SpecialCorr = 1
    }
    
    m_efs = (intercept + m_efs) * zoom
  }
  
  
  
  col.lim2 <- (intercept + col.lim) * zoom
  int <- intercept * zoom
  
  
  
  if (is.null(col) & is.corr) {
    col <- COL2('RdBu', 200)
  }
  
  if (is.null(col) & !is.corr) {
    if(col.lim[1] * col.lim[2] < 0) {
      col <- COL2('RdBu', 200)
    } else {
      col <- COL1('YlOrBr', 200)
    }
    
  }
  
  n <- nrow(m_efs)
  m <- ncol(m_efs)
  min.nm <- min(n, m)
  ord <- 1:min.nm
  
  
  if (order != 'original') {
    ord = corrMatOrder(m_efs, order = order, hclust.method = hclust.method)
    m_efs = m_efs[ord, ord]
  }
  
  ## set up variable names
  if (is.null(rownames(m_efs))) {
    rownames(m_efs) = 1:n
  }
  if (is.null(colnames(m_efs))) {
    colnames(m_efs) = 1:m
  }
  
  # assigns Inf to cells in the matrix depending on the type paramter
  apply_mat_filter = function(mat) {
    x = matrix(1:n * m, nrow = n, ncol = m)
    switch(type,
           upper = mat[row(x) > col(x)] <- Inf,
           lower = mat[row(x) < col(x)] <- Inf
    )
    
    if (!diag) {
      diag(mat) = Inf
    }
    return(mat)
  }
  
  
  # retrieves coordinates of cells to be rendered
  getPos.Dat = function(mat) {
    tmp = apply_mat_filter(mat)
    Dat = tmp[is.finite(tmp)]
    ind  = which(is.finite(tmp), arr.ind = TRUE)
    Pos = ind
    Pos[, 1] =  ind[, 2]
    Pos[, 2] = -ind[, 1] + 1 + n
    
    PosName = ind
    PosName[, 1] = colnames(mat)[ind[, 2]]
    PosName[, 2] = rownames(mat)[ind[, 1]]
    return(list(Pos, Dat, PosName))
  }
  
  # retrieves coordinates of NA cells
  # we use this for rending NA cells differently
  getPos.NAs = function(mat) {
    tmp = apply_mat_filter(mat)
    ind = which(is.na(tmp), arr.ind = TRUE)
    Pos = ind
    Pos[, 1] =  ind[, 2]
    Pos[, 2] = -ind[, 1] + 1 + n
    return(Pos)
  }
  
  testTemp = getPos.Dat(m_efs)
  
  Pos = getPos.Dat(m_efs)[[1]]
  PosName = getPos.Dat(m_efs)[[3]]
  
  # decide whether NA labels are going to be rendered or whether we ignore them
  if (any(is.na(m_efs)) && is.character(na.label)) {
    PosNA = getPos.NAs(m_efs)
  } else {
    # explicitly set to NULL to indicate that NA labels are not going to be
    # rendered
    PosNA = NULL
  }
  
  AllCoords = rbind(Pos, PosNA)
  
  # rows
  n2 = max(AllCoords[, 2])
  n1 = min(AllCoords[, 2])
  
  nn = n2 - n1
  
  m2 = max(AllCoords[, 1])
  m1 = min(AllCoords[, 1])
  
  # Issue #19: legend color bar width 0 when using just one column matrix
  # also discussed here: http://stackoverflow.com/questions/34638555/
  mm = max(1, m2 - m1)
  
  # Issue #20: support plotmath expressions in rownames and colnames
  # expand_expression = function(s) {
  #   ifelse(grepl('^[:=$]', s), parse(text = substring(s, 2)), s)
  # }
  # 
  # newrownames = sapply(
  #   rownames(m_efs)[(n + 1 - n2):(n + 1 - n1)], expand_expression)
  # 
  # newcolnames = sapply(
  #   colnames(m_efs)[m1:m2], expand_expression)
  
  expand_expression = function(s) {
    ifelse(grepl('^[:=$]', s), parse(text = substring(s, 2)), s)
  }

  newrownames = sapply(
    row_names[(n + 1 - n2):(n + 1 - n1)], expand_expression)

  newcolnames = sapply(
    col_names[m1:m2], expand_expression)
  
  
  
  # !!!!!!!!!!!!!!THIS MIGHT BE IT!!!!!!!!!!!!!!!!
  DAT = getPos.Dat(m_imp)[[2]]
  DAT_col = getPos.Dat(m_efs)[[2]]
  len.DAT = length(DAT)
  
  
  
  
  
  
  rm(expand_expression) # making sure the function is only used here
  
  
  ## assign colors
  assign.color = function(dat = DAT_col, color = col, isSpecialCorr = SpecialCorr) {
    
    if(isSpecialCorr) {
      newcorr = (dat + 1) / 2
    } else {
      newcorr = dat
    }
    
    newcorr[newcorr <= 0]  = 0
    newcorr[newcorr >= 1]  = 1 - 1e-16
    color[floor(newcorr * length(color)) + 1] # new color returned
  }
  
  col.fill = assign.color()
  
  isFALSE = function(x) identical(x, FALSE)
  isTRUE = function(x) identical(x, TRUE)
  
  if (isFALSE(tl.pos)) {
    tl.pos = 'n'
  }
  
  if (is.null(tl.pos) || isTRUE(tl.pos)) {
    tl.pos = switch(type, full = 'lt', lower = 'ld', upper = 'td')
  }
  
  if (isFALSE(cl.pos)) {
    cl.pos = 'n'
  }
  
  if (is.null(cl.pos) || isTRUE(cl.pos)) {
    cl.pos = switch(type, full = 'r', lower = 'b', upper = 'r')
  }
  
  if (isFALSE(outline)) {
    col.border = col.fill
  } else if (isTRUE(outline)) {
    col.border = 'black'
  } else if (is.character(outline)) {
    col.border = outline
  } else {
    stop('Unsupported value type for parameter outline')
  }
  
  # restore this parameter when exiting the corrplot function in any way
  oldpar = par(mar = mar, bg = par()$bg)
  on.exit(par(oldpar), add = TRUE)
  
  
  ## calculate label-text width approximately
  if (!add) {
    plot.new()
    
    # Issue #10: code from Sebastien Rochette (github user @statnmap)
    xlabwidth = max(strwidth(newrownames, cex = tl.cex))
    ylabwidth = max(strwidth(newcolnames, cex = tl.cex))
    laboffset = strwidth('W', cex = tl.cex) * tl.offset
    
    if (!is.null(r.print)) {
      xlabwidth = 0
    } # end of if condition over r.print
    
    # Issue #10
    for (i in 1:50) {
      xlim = c(
        m1 - 0.5 - laboffset -
          xlabwidth * (grepl('l', tl.pos) | grepl('d', tl.pos)),
        m2 + 0.5 + mm * cl.ratio * (cl.pos == 'r') +
          xlabwidth * abs(cos(tl.srt * pi / 180)) * grepl('d', tl.pos)
      ) #+ c(-0.35, 0.15)
      
      ylim = c(
        n1 - 0.5 - nn * cl.ratio * (cl.pos == 'b') - laboffset,
        n2 + 0.5 + laboffset +
          ylabwidth * abs(sin(tl.srt * pi / 180)) * grepl('t', tl.pos) +
          ylabwidth * abs(sin(tl.srt * pi / 180)) * (type=='lower') * grepl('d', tl.pos)
      ) #+ c(-0.15, 0)
      
      plot.window(xlim, ylim, asp = 1, xaxs = 'i', yaxs = 'i')
      
      x.tmp = max(strwidth(newrownames, cex = tl.cex))
      y.tmp = max(strwidth(newcolnames, cex = tl.cex))
      
      laboffset.tmp = strwidth('W', cex = tl.cex) * tl.offset
      if (max(x.tmp - xlabwidth,
              y.tmp - ylabwidth,
              laboffset.tmp - laboffset) < 1e-03) {
        break
      }
      
      xlabwidth = x.tmp
      ylabwidth = y.tmp
      
      laboffset <- laboffset.tmp
      
      if (i == 50) {
        warning(c('Not been able to calculate text margin, ',
                  'please try again with a clean new empty window using ',
                  '{plot.new(); dev.off()} or reduce tl.cex'))
      }
    }
    
    if (.Platform$OS.type == 'windows') {
      grDevices::windows.options(width = 7,
                                 height = 7 * diff(ylim) / diff(xlim))
    }
    
    xlim = xlim + diff(xlim) * 0.01 * c(-1, 1)
    ylim = ylim + diff(ylim) * 0.01 * c(-1, 1)
    
    plot.window(xlim = xlim, ylim = ylim,
                asp = win.asp, xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  }
  
  ## for: add = TRUE
  laboffset = strwidth('W', cex = tl.cex) * tl.offset
  
  ## background for the cells
  symbols(Pos, add = TRUE, inches = FALSE,
          rectangles = matrix(1, len.DAT, 2), bg = bg, fg = bg)
  
  ## circle
  if (method == 'circle' && plotCI == 'n') {
    symbols(Pos, add = TRUE,  inches = FALSE,
            circles = asp_rescale_factor * 0.9 * abs(DAT) ^ 0.5 / 2,
            fg = col.border, bg = col.fill)
  }
  
  ## ellipse
  if (method == 'ellipse' && plotCI == 'n') {
    ell.dat = function(rho, length = 99) {
      k = seq(0, 2 * pi, length = length)
      x = cos(k + acos(rho) / 2) / 2
      y = cos(k - acos(rho) / 2) / 2
      cbind(rbind(x, y), c(NA, NA))
    }
    
    ELL.dat = lapply(DAT, ell.dat)
    ELL.dat2 = 0.85 * matrix(unlist(ELL.dat), ncol = 2, byrow = TRUE)
    ELL.dat2 = ELL.dat2  + Pos[rep(1: length(DAT), each = 100), ]
    polygon(ELL.dat2, border = col.border, col = col.fill)
  }
  
  ## number
  if (is.null(number.digits)) {
    number.digits = switch(addCoefasPercent + 1, 2, 0)
  }
  
  stopifnot(number.digits %% 1 == 0)  # is whole number
  stopifnot(number.digits >= 0)       # is non-negative number
  
  if (method == 'number' && plotCI == 'n') {
    x = (DAT - int) * ifelse(addCoefasPercent, 100, 1) / zoom
    text(Pos[, 1], Pos[, 2], font = number.font, col = col.fill,
         labels = format(round(x, number.digits), nsmall = number.digits),
         cex = number.cex)
  }
  
  # Issue #55: Support for multiple characters when rendering NAs
  NA_LABEL_MAX_CHARS = 2
  
  # renders NA cells
  if (is.matrix(PosNA) && nrow(PosNA) > 0) {
    
    stopifnot(is.matrix(PosNA)) # sanity check
    
    if (na.label == 'square') {
      symbols(PosNA, add = TRUE, inches = FALSE,
              squares = rep(1, nrow(PosNA)),
              bg = na.label.col, fg = na.label.col)
    } else if (nchar(na.label) %in% 1:NA_LABEL_MAX_CHARS) {
      symbols(PosNA, add = TRUE, inches = FALSE,
              squares = rep(1, nrow(PosNA)), fg = bg, bg = bg)
      text(PosNA[, 1], PosNA[, 2], font = number.font,
           col = na.label.col,
           labels = na.label, cex = number.cex, ...)
    } else {
      stop(paste('Maximum number of characters for NA label is:',
                 NA_LABEL_MAX_CHARS))
    }
  }
  
  ## pie
  if (method == 'pie' && plotCI == 'n') {
    
    # Issue #18: Corrplot background circle
    symbols(Pos, add = TRUE, inches = FALSE,
            circles = rep(0.5, len.DAT) * 0.85, fg = col.border)
    
    pie.dat = function(theta, length = 100) {
      k = seq(pi / 2, pi / 2 - theta, length = 0.5 * length * abs(theta) / pi)
      x = c(0, cos(k) / 2, 0)
      y = c(0, sin(k) / 2, 0)
      cbind(rbind(x, y), c(NA, NA)) # pie.dat returned
    }
    
    PIE.dat = lapply(DAT * 2 * pi, pie.dat)
    len.pie = unlist(lapply(PIE.dat, length)) / 2
    PIE.dat2 = 0.85 * matrix(unlist(PIE.dat), ncol = 2, byrow = TRUE)
    PIE.dat2 = PIE.dat2  + Pos[rep(1:length(DAT), len.pie), ]
    polygon(PIE.dat2, border = 'black', col = col.fill)
  }
  
  ## shade
  if (method == 'shade' && plotCI == 'n') {
    symbols(Pos, add = TRUE, inches = FALSE, squares = rep(1, len.DAT),
            bg = col.fill, fg = addgrid.col)
    
    shade.dat = function(w) {
      x = w[1]
      y = w[2]
      rho = w[3]
      x1 = x - 0.5
      x2 = x + 0.5
      y1 = y - 0.5
      y2 = y + 0.5
      dat = NA
      
      if ((addshade == 'positive' || addshade == 'all') && rho > 0) {
        dat = cbind(c(x1, x1, x), c(y, y1, y1),
                    c(x, x2, x2), c(y2, y2, y))
      }
      
      if ((addshade == 'negative' || addshade == 'all') && rho < 0) {
        dat = cbind(c(x1, x1, x), c(y, y2, y2),
                    c(x, x2, x2), c(y1, y1, y))
      }
      
      return(t(dat))
    }
    
    pos_corr = rbind(cbind(Pos, DAT))
    pos_corr2 = split(pos_corr, 1: nrow(pos_corr))
    
    SHADE.dat = matrix(na.omit(unlist(lapply(pos_corr2, shade.dat))),
                       byrow = TRUE, ncol = 4)
    
    segments(SHADE.dat[, 1], SHADE.dat[, 2], SHADE.dat[, 3],
             SHADE.dat[, 4], col = shade.col, lwd = shade.lwd)
  }
  
  ## square
  if (method == 'square' && plotCI == 'n') {
    draw_method_square(Pos, DAT, asp_rescale_factor, col.border, col.fill)
  }
  
  ## color
  if (method == 'color' && plotCI == 'n') {
    draw_method_color(Pos, col.border, col.fill)
  }
  
  draw_grid = function(coords, fg) {
    symbols(coords, add = TRUE, inches = FALSE, fg = fg, bg = NA,
            rectangles = matrix(1, nrow = nrow(coords), ncol = 2))
  }
  
  ## add grid
  draw_grid(AllCoords, addgrid.col)
  
  if (plotCI != 'n') {
    
    if (is.null(lowCI.mat) || is.null(uppCI.mat)) {
      stop('Need lowCI.mat and uppCI.mat!')
    }
    
    if (order != 'original') {
      lowCI.mat = lowCI.mat[ord, ord]
      uppCI.mat = uppCI.mat[ord, ord]
    }
    
    pos.lowNew  = getPos.Dat(lowCI.mat)[[1]]
    lowNew      = getPos.Dat(lowCI.mat)[[2]]
    pos.uppNew  = getPos.Dat(uppCI.mat)[[1]]
    uppNew      = getPos.Dat(uppCI.mat)[[2]]
    
    k1 = (abs(uppNew) > abs(lowNew))
    bigabs = uppNew
    bigabs[which(!k1)] = lowNew[!k1]
    smallabs = lowNew
    smallabs[which(!k1)] = uppNew[!k1]
    sig = sign(uppNew * lowNew)
    
    color_bigabs = col[ceiling((bigabs + 1) * length(col) / 2)]
    color_smallabs = col[ceiling((smallabs + 1) * length(col) / 2)]
    
    if (plotCI == 'circle') {
      
      symbols(pos.uppNew[, 1], pos.uppNew[, 2],
              add = TRUE,  inches = FALSE,
              circles = 0.95 * abs(bigabs) ^ 0.5 / 2,
              bg = ifelse(sig > 0, col.fill, color_bigabs),
              fg = ifelse(sig > 0, col.fill, color_bigabs)
      )
      
      symbols(pos.lowNew[, 1], pos.lowNew[, 2],
              add = TRUE, inches = FALSE,
              circles = 0.95 * abs(smallabs) ^ 0.5 / 2,
              bg = ifelse(sig > 0, bg, color_smallabs),
              fg = ifelse(sig > 0, col.fill, color_smallabs))
    }
    
    if (plotCI == 'square') {
      symbols(pos.uppNew[, 1], pos.uppNew[, 2],
              add = TRUE,  inches = FALSE,
              squares = abs(bigabs) ^ 0.5,
              bg = ifelse(sig > 0, col.fill, color_bigabs),
              fg = ifelse(sig > 0, col.fill, color_bigabs))
      
      symbols(pos.lowNew[, 1], pos.lowNew[, 2],
              add = TRUE, inches = FALSE,
              squares = abs(smallabs) ^ 0.5,
              bg = ifelse(sig > 0, bg, color_smallabs),
              fg = ifelse(sig > 0, col.fill, color_smallabs))
    }
    
    if (plotCI == 'rect') {
      rect.width = 0.25
      rect(pos.uppNew[, 1] - rect.width, pos.uppNew[, 2] + smallabs / 2,
           pos.uppNew[, 1] + rect.width, pos.uppNew[, 2] + bigabs / 2,
           col = col.fill, border = col.fill)
      segments(pos.lowNew[, 1] - rect.width, pos.lowNew[, 2] + DAT / 2,
               pos.lowNew[, 1] + rect.width, pos.lowNew[, 2] + DAT / 2,
               col = 'black', lwd = 1)
      segments(pos.uppNew[, 1] - rect.width, pos.uppNew[, 2] + uppNew / 2,
               pos.uppNew[, 1] + rect.width, pos.uppNew[, 2] + uppNew / 2,
               col = 'black', lwd = 1)
      segments(pos.lowNew[, 1] - rect.width, pos.lowNew[, 2] + lowNew / 2,
               pos.lowNew[, 1] + rect.width, pos.lowNew[, 2] + lowNew / 2,
               col = 'black', lwd = 1)
      segments(pos.lowNew[, 1] - 0.5, pos.lowNew[, 2],
               pos.lowNew[, 1] + 0.5, pos.lowNew[, 2], col = 'grey70', lty = 3)
    }
  }
  
  ## add numbers
  if (!is.null(addCoef.col) && method != 'number') {
    text(Pos[, 1], Pos[, 2],  col = addCoef.col,
         labels = round((DAT - int) * ifelse(addCoefasPercent, 100, 1) / zoom,
                        number.digits),
         cex = number.cex, font = number.font)
  }
  
  
  if (!is.null(p.mat)) {
    pos.pNew  = getPos.Dat(p.mat)[[1]]
    pNew      = getPos.Dat(p.mat)[[2]]
  }
  
  
  if (!is.null(p.mat) && insig != 'n') {
    if (order != 'original') {
      p.mat = p.mat[ord, ord]
    }
    
    if(!is.null(rownames(p.mat)) | !is.null(rownames(p.mat))) {
      if(!all(colnames(p.mat)==colnames(m_efs)) |
         !all(rownames(p.mat)==rownames(m_efs))) {
        warning('p.mat and corr may be not paired, their rownames and colnames are not totally same!')
      }
    }
    
    if (insig == 'label_sig') {
      
      # Unless another character is specified, mark sig with *
      if (!is.character(pch))
        pch = '*'
      
      fplace_points = function(sig.locs, point) {
        text(pos.pNew[, 1][sig.locs], pos.pNew[, 2][sig.locs],
             labels = point, col = pch.col, cex = pch.cex, lwd = 2)
      }
      
      if (length(sig.level) == 1) {
        place_points(sig.locs = which(pNew < sig.level), point = pch)
        
      } else {
        l = length(sig.level)
        for (i in seq_along(sig.level)) {
          iter = l + 1 - i
          pchTmp = paste(rep(pch, i), collapse = '')
          if (i == length(sig.level)) {
            locs = which(pNew < sig.level[iter])
            if (length(locs)) {
              place_points(sig.locs = locs, point = pchTmp)
            }
          } else {
            locs = which(pNew < sig.level[iter] & pNew > sig.level[iter - 1])
            if (length(locs)) {
              place_points(sig.locs = locs, point = pchTmp)
            }
          }
        }
        
      }
      
    } else {
      
      ind.p = which(pNew > sig.level)
      p_inSig = length(ind.p) > 0
      
      if (insig == 'pch' && p_inSig) {
        points(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p],
               pch = pch, col = pch.col, cex = pch.cex, lwd = 2)
      }
      
      if (insig == 'p-value' && p_inSig) {
        text(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p],
             round(pNew[ind.p], number.digits), col = pch.col)
      }
      
      if (insig == 'blank' && p_inSig) {
        symbols(pos.pNew[, 1][ind.p], pos.pNew[, 2][ind.p], inches = FALSE,
                squares = rep(1, length(pos.pNew[, 1][ind.p])),
                fg = addgrid.col, bg = bg, add = TRUE)
      }
    }
  }
  
  ### color legend
  if (cl.pos != 'n') {
    colRange = assign.color(dat = col.lim2)
    
    
    ind1 = which(col == colRange[1])
    ind2 = which(col == colRange[2])
    colbar = col[ind1:ind2]
    
    if (is.null(cl.length)) {
      cl.length = ifelse(length(colbar) > 20, 11, length(colbar) + 1)
    }
    
    labels = seq(col.lim[1], col.lim[2], length = cl.length)
    
    if (cl.pos == 'r') {
      vertical = TRUE
      xlim = c(m2 + 0.5 + mm * 0.02, m2 + 0.5 + mm * cl.ratio)
      ylim = c(n1 - 0.5, n2 + 0.5)
    }
    
    if (cl.pos == 'b') {
      vertical = FALSE
      xlim = c(m1 - 0.5, m2 + 0.5)
      ylim = c(n1 - 0.5 - nn * cl.ratio, n1 - 0.5 - nn * 0.02)
    }
    
    colorlegend(colbar = colbar, labels = round(labels, 2),
                offset = cl.offset, ratio.colbar = 0.3, cex = cl.cex,
                xlim = xlim, ylim = ylim, vertical = vertical,
                align = cl.align.text)
  }
  
  ## add variable names and title
  if (tl.pos != 'n') {
    pos.xlabel = cbind(m1:m2, n2 + 0.5 + laboffset)
    pos.ylabel = cbind(m1 - 0.5, n2:n1)
    pos.r2.label <- cbind(m1:m2 - 0.3, 0.2)
    
    if (tl.pos == 'td') {
      if (type != 'upper') {
        stop('type should be \'upper\' if tl.pos is \'dt\'.')
      }
      pos.ylabel = cbind(m1:(m1 + nn) - 0.5, n2:n1)
    }
    
    if (tl.pos == 'ld') {
      if (type != 'lower') {
        stop('type should be \'lower\' if tl.pos is \'ld\'.')
      }
      pos.xlabel = cbind(m1:m2, n2:(n2 - mm) + 0.5 + laboffset)
    }
    
    if (tl.pos == 'd') {
      pos.ylabel = cbind(m1:(m1 + nn) - 0.5, n2:n1)
      pos.ylabel = pos.ylabel[1:min(n, m), ]
      
      symbols(pos.ylabel[, 1] + 0.5, pos.ylabel[, 2], add = TRUE,
              bg = bg, fg = addgrid.col,
              inches = FALSE, squares = rep(1, length(pos.ylabel[, 1])))
      
      text(pos.ylabel[, 1] + 0.5, pos.ylabel[, 2], newcolnames[1:min(n, m)],
           col = tl.col, cex = tl.cex)
      
    } else {
      
      if(tl.pos != 'l') {
        text(pos.xlabel[, 1], pos.xlabel[, 2], newcolnames, srt = tl.srt,
             adj = ifelse(tl.srt == 0, c(0.5, 0), c(0, 0)),
             col = tl.col, cex = tl.cex, offset = tl.offset)
      }
      
      if(is.null(r.print)) {
        text(pos.ylabel[, 1], pos.ylabel[, 2], newrownames,
             col = tl.col, cex = tl.cex, pos = 2, offset = tl.offset)
      }

      
      if(!is.null(r2names)) {
        # r2 values
        text(pos.r2.label[, 1], pos.r2.label[, 2], r2names, srt = 0,
             adj = ifelse(tl.srt == 0, c(0.5, 0), c(0, 0)),
             col = tl.col, cex = cl.cex, offset = tl.offset)
      }
      
    }
  }
  
  
  
  
  ## add grid, in case of the grid is ate when 'diag=FALSE'
  if (type == 'full' && plotCI == 'n' && !is.null(addgrid.col)) {
    rect(m1 - 0.5, n1 - 0.5, m2 + 0.5, n2 + 0.5, border = addgrid.col)
  }
  
  ##  draws rectangles, call function corrRect.hclust
  if (!is.null(addrect) && order == 'hclust' && type == 'full') {
    corrRect.hclust(m_efs, k = addrect, method = hclust.method,
                    col = rect.col, lwd = rect.lwd)
  }
  
  corrPos = data.frame(PosName, Pos, DAT)
  colnames(corrPos) = c('xName', 'yName', 'x', 'y', 'corr')
  if(!is.null(p.mat)) {
    corrPos = cbind(corrPos, pNew)
    colnames(corrPos)[6] = c('p.value')
  }
  corrPos = corrPos[order(corrPos[, 3], -corrPos[, 4]), ]
  rownames(corrPos) = NULL
  
  
  res = list(corr = m_efs, corrPos = corrPos, arg = list(type = type))
  
  invisible(res) # reordered correlation matrix, and Position
}


#' @noRd
draw_method_square = function(coords, values, asp_rescale_factor, fg, bg) {
  symbols(coords, add = TRUE, inches = FALSE,
          squares = asp_rescale_factor * abs(values) ^ 0.5,
          bg = bg, fg = fg)
}


#' @noRd
draw_method_color = function(coords, fg, bg) {
  symbols(coords, squares = rep(1, nrow(coords)), fg = fg, bg = bg,
          add = TRUE, inches = FALSE)
}


#' @noRd
draw_grid = function(coords, fg) {
  symbols(coords, add = TRUE, inches = FALSE, fg = fg, bg = NA,
          rectangles = matrix(1, nrow = nrow(coords), ncol = 2))
}
