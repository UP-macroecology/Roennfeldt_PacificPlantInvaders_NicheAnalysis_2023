library(dplyr)

rm(list = setdiff(ls(), c("traits_res_e", "traits_res_s", "traits_res_u")))

data <- traits_res_e

covariates <- unique(data$term)

# prepare alternative input matrix
m_imp <- data %>% 
  select(region, varimp, term) %>% 
  pivot_wider(names_from = region, values_from = varimp) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

m_efs <- data %>% 
  select(region, estimate, term) %>% 
  pivot_wider(names_from = region, values_from = estimate) %>% 
  select(!term) %>% 
  as.matrix(rownames.force = TRUE)

rownames(m_imp) <- covariates
rownames(m_efs) <- covariates

corr <- m_efs
method <- "circle"
type <- "full"
is.corr <- FALSE
bg <- "white"
outline <- TRUE
na.label = "square"
na.label.col = "white"
col.lim <- NULL
win.asp <- 1
diag = TRUE
transKeepSign = TRUE
col <- NULL
order <- "original"

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
      diag(corr) = NA
    }
    
    col.lim = c(min(corr, na.rm = TRUE), max(corr, na.rm = TRUE))
  }
}


if (is.corr != 0) {
  
  c_max = max(corr, na.rm = TRUE)
  c_min = min(corr, na.rm = TRUE)
  
  if ((col.lim[1] > c_min) | (col.lim[2] < c_max))
  {
    stop('Wrong color: matrix should be in col.lim interval!')
  }
  
  if (diff(col.lim)/(c_max - c_min) > 2) {
    warning('col.lim interval too wide, please set a suitable value')
  }
  
  # all negative or positive or NOT transkeepSign, trans to [0, 1]
  if (c_max <= 0 | c_min >= 0 | !transKeepSign) {
    intercept = -col.lim[1]
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
  
  corr = (intercept + corr) * zoom
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

n <- nrow(corr)
m <- ncol(corr)
min.nm <- min(n, m)
ord <- 1:min.nm


if (order != 'original') {
  ord = corrMatOrder(corr, order = order, hclust.method = hclust.method)
  corr = corr[ord, ord]
}

## set up variable names
if (is.null(rownames(corr))) {
  rownames(corr) = 1:n
}
if (is.null(colnames(corr))) {
  colnames(corr) = 1:m
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

testTemp = getPos.Dat(corr)

Pos = getPos.Dat(corr)[[1]]
PosName = getPos.Dat(corr)[[3]]

# decide whether NA labels are going to be rendered or whether we ignore them
if (any(is.na(corr)) && is.character(na.label)) {
  PosNA = getPos.NAs(corr)
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
expand_expression = function(s) {
  ifelse(grepl('^[:=$]', s), parse(text = substring(s, 2)), s)
}

newrownames = sapply(
  rownames(corr)[(n + 1 - n2):(n + 1 - n1)], expand_expression)

newcolnames = sapply(
  colnames(corr)[m1:m2], expand_expression)





# !!!!!!!!!!!!!!THIS MIGHT BE IT!!!!!!!!!!!!!!!!
DAT = getPos.Dat(corr)[[2]]
len.DAT = length(DAT)






rm(expand_expression) # making sure the function is only used here


## assign colors
assign.color = function(dat = DAT, color = col, isSpecialCorr = SpecialCorr) {
  
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