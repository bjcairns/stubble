#' @title
#' Miscellaneous internal functions
#' 
#' @description
#' Internal functions for various stubble functions.
#' 
#' @importFrom stats rbinom
#' @importFrom utils installed.packages


### dtype0() ###
#' @noRd
dtype0 <- function(x, ...){
  
  ## Define S3 Method ##
  UseMethod("dtype0", x)
  
}


### dtype0.default() ###
#' @export
dtype0.default <- function(x){
  
  ## Error ##
  .stop_no_method(x)
  
}


### dtype0.character() ###
#' @export
dtype0.character <- function(x){
  
  ## 0-Length Vector ##
  v <- character(length = 0L)
  
  ## Output ##
  return(v)
  
}


### dtype0.data.frame() ###
#' @export
dtype0.data.frame <- function(x){
  
  ## 0-Row data.frame ##
  d <- data.frame()
  
  ## Output ##
  return(d)
  
}


### dtype0.data.table() ###
#' @export
dtype0.data.table <- function(x){
  
  ## Create Object ##
  d <- if (is.installed.package("data.table")) {
    
    # 0-Row data.table #
    data.table::data.table()
    
  } else {
    
    # Warning #
    .warning_coercion(x)
    
    # 0-Row data.frame #
    data.frame()
    
  }
  
  ## Output ##
  return(d)
  
}


### dtype0.Date() ###
#' @export
dtype0.Date <- function(x){
  
  ## 0-Length Vector ##
  v <- as.Date(character(length = 0L), tz = "UTC")
  
  ## Output ##
  return(v)
  
}


### dtype0.double() ###
#' @export
dtype0.double <- function(x){
  
  ## 0-Length Vector ##
  v <- double(length = 0L)
  
  ## Output ##
  return(v)
  
}


### dtype0.factor() ###
#' @export
dtype0.factor <- function(x){
  
  ## 0-Length Vector ##
  v <- factor(character(length = 0L))
  
  ## Output ##
  return(v)
  
}


### dtype0.IDate() ###
#' @export
dtype0.IDate <- function(x){
  
  ## 0-Length Vector ##
  v <- if (is.installed.package("data.table")) {
    
    data.table::as.IDate(character(length = 0L), tz = "UTC")
    
  } else {
    
    NextMethod(x)
    
  }
  
  ## Output ##
  return(v)
  
}


### dtype0.integer() ###
#' @export
dtype0.integer <- function(x){
  
  ## 0-Length Vector ##
  v <- integer(length = 0L)
  
  ## Output ##
  return(v)
  
}


### dtype0.integer64() ###
#' @export
dtype0.integer64 <- function(x){
  
  ## Create Object ##
  v <- if (is.installed.package("bit64")) {
    
    # 0-Length Vector #
    bit64::integer64(length = 0L)
    
  } else {
    
    # Warning #
    .warning_coercion(x)
    
    # 0-Length Vector #
    logical(length = 0L)
    
  }
  
  ## Output ##
  return(v)
  
}


### dtype0.ITime() ###
#' @export
dtype0.ITime <- function(x){
  
  ## Create Object ##
  v <- if(is.installed.package("data.table")) {
    
    # 0-Length Vector #
    data.table::as.ITime(character(length = 0L))
    
  } else {
    
    # Warning #
    .warning_coercion(x)
    
    # 0-Length Vector #
    as.POSIXct(character(length = 0L), tz = "UTC")
    
  }
  
}


### dtype0.list() ###
#' @export
dtype0.list <- function(x){
  
  ## 0-Length List ##
  l <- list()
  
  ## Output ##
  return(l)
  
}


### dtype0.logical() ###
#' @export
dtype0.logical <- function(x){
  
  ## 0-Length List ##
  v <- logical(length = 0)
  
  ## Output ##
  return(v)
  
}


### dtype0.ordered() ###
#' @export
dtype0.ordered <- function(x){
  
  ## 0-Length Vector ##
  v <- ordered(character(length = 0L))
  
  ## Output ##
  return(v)
  
}


### dtype0.POSIXct() ###
#' @export
dtype0.POSIXct <- function(x){
  
  ## 0-Length Vector ##
  v <- as.POSIXct(character(length = 0L), tz = "UTC")
  
  ## Output ##
  return(v)
  
}


### dtype0.POSIXlt() ###
#' @export
dtype0.POSIXlt <- function(x){
  
  ## 0-Length Vector ##
  v <- as.POSIXlt(character(length = 0L), tz = "UTC")
  
  ## Output ##
  return(v)
  
}


### dtype0.tbl_df() ###
#' @export
dtype0.tbl_df <- function(x){
  
  
  d <- if (is.installed.package("tibble")) {
    
    # 0-Length tibble #
    tibble::tibble()
    
  } else {
    
    # Warning #
    .warning_coercion(x)
    
    # 0-Length data.frame #
    data.frame()
    
  }
  
  ## Output ##
  return(d)
  
}


### impute_na() ###
#' @noRd
impute_na <- function(syn_col, p_na){
  
  ## Simulate Missing Data ##
  if (p_na == 1) {
    
    syn_col[] <- NA
    
  } else if (p_na > 0) {
    
    syn_col[rbinom(n = length(syn_col), size = 1L, prob = p_na) == 1L] <- NA
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### is.installed.package() ###
#' @noRd
is.installed.package <- function(pkg, ...){
  
  ## Non-NA Indices ##
  ind <- which(!is.na(pkg))
  
  ## Empty Results Vector ##
  status <- rep(NA, length(pkg))
  
  ## Assert Package Installation ##
  status[ind] <- pkg[ind] %in% rownames(installed.packages(...))
  
  ## Output ##
  return(status)
  
}


### sample_chars() ###
#' @noRd
sample_chars <- function(x, size, nchar_min = 0L, nchar_max = 10L, agn_chr_sep = ""){
  
  ## Character Number Range ##
  nchar_range <- nchar_min:nchar_max
  
  ## Lengths of Each String in the Synthetic Character Vector ##
  nchars <- nchar_range[sample.int(n = length(nchar_range), size = size, replace = TRUE)]
  
  ## Generate List of Character Vectors of Lengths == nchars ##
  char_list <- lapply(X = nchars, FUN = sample, x = x, replace = TRUE)
  
  ## Collapse List of Character Vectors ##
  char_vec <- vapply(X = char_list, FUN = paste0, FUN.VALUE = character(1), collapse = agn_chr_sep)
  
  ## Output ##
  return(char_vec)
  
}


### fuzz_binom() ###
## Add noise to the probability mass function of a discrete distribution, using 
## a symmetric noise distribution based on a binomial.
## Uses recursion to improve a first approximation to the requested amount of 
## noise
#' @noRd
fuzz_binom <- function(p, sd_ratio, recurse = TRUE) {
  
  ## Preliminaries
  x <- seq_along(p)
  n <- length(p) - 1
  sd_p <- sqrt(sum((x - sum(x * p))^2 * p))
  MM <- matrix(0, nrow = n+1, ncol = n+1)
  
  ## Guess at the appropriate success probability parameter for a binomial
  cc <- -(sd_p * sd_ratio)^2/n
  pp <- suppressWarnings((bb - sqrt(1 + 4*cc)) * 0.5)
  if (is.na(pp)) {
    if (recurse) 
      warning("cannot add requested noise; reverting to binomial(n, 0.5)")
    pp <- 0.5
  }
  
  ## Construct a reflected binomial PMF in order to xconstruct the 
  ## matrix multiplier
  dd <- dbinom(x - 1, n, pp)
  dd <- c(rev(dd), dd[-1]) 
  
  ## Construct the matrix multiplier
  for (i in rev(x)) {
    MM[,i] <- dd[1:(n+1)]
    dd <- dd[-1]
  }
  
  ## Normalise columns
  MM <- MM %*% diag(1/as.vector(matrix(1, nrow = 1, ncol = n+1) %*% MM))
  
  ## Correct the SD of the noise
  if (recurse == TRUE) {
    sd_obv <- sqrt((sd_disc(MM %*% p))^2 - sd_p^2) / sd_p
    MM <- fuzz_mult(p, sd_ratio * sd_ratio / sd_obv, recurse = FALSE)
  }
  
  return(MM %*% p)
  
}

