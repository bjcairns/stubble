#' @title
#' Miscellaneous internal functions
#' 
#' @description
#' Internal functions for various stubble functions.
#' 
#' @importFrom stats rbinom
#' @importFrom utils installed.packages


### impute_na() ###
#' @noRd
impute_na <- function(syn_col, p_na){
  
  ## Simulate Missing Data ##
  if(p_na == 1){
    
    syn_col[] <- NA
    
  } else {
    
    syn_col[rbinom(n = length(syn_col), size = 1L, prob = p_na) == 1L] <- NA
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### is.installed.package() ###
#' @noRd
is.installed.package <- function(pkg, ...){
  
  ## Assert Package Installation ##
  status <- pkg %in% rownames(installed.packages(...))
  
  ## Output ##
  return(status)
  
}


### dtype0() ###
#' @noRd
dtype0 <- function(x, ...){
  
  ## Define S3 Method ##
  UseMethod("dtype0", x)
  
}


### dtype0.default() ###
#' @noRd
dtype0.default <- function(x){
  
  ## Error ##
  .stop_no_method(x)
  
}


### dtype0.character() ###
#' @noRd
dtype0.character <- function(x){
  
  ## 0-Length Vector ##
  v <- character(length = 0L)
  
  ## Output ##
  return(v)
  
}


### dtype0.data.frame() ###
#' @noRd
dtype0.data.frame <- function(x){
  
  ## 0-Row data.frame ##
  d <- data.frame()
  
  ## Output ##
  return(d)
  
}


### dtype0.data.table() ###
#' @noRd
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
#' @noRd
dtype0.Date <- function(x){
  
  ## 0-Length Vector ##
  v <- as.Date(character(length = 0L), tz = "UTC")
  
  ## Output ##
  return(v)
  
}


### dtype0.double() ###
#' @noRd
dtype0.double <- function(x){
  
  ## 0-Length Vector ##
  v <- double(length = 0L)
  
  ## Output ##
  return(v)
  
}


### dtype0.factor() ###
#' @noRd
dtype0.factor <- function(x){
  
  ## 0-Length Vector ##
  v <- factor(character(length = 0L))
  
  ## Output ##
  return(v)
  
}


### dtype0.IDate() ###
#' @noRd
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
#' @noRd
dtype0.integer <- function(x){
  
  ## 0-Length Vector ##
  v <- integer(length = 0L)
  
  ## Output ##
  return(v)
  
}


### dtype0.integer64() ###
#' @noRd
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


### dtype0.list() ###
#' @noRd
dtype0.list <- function(x){
  
  ## 0-Length List ##
  l <- list()
  
  ## Output ##
  return(l)
  
}


### dtype0.logical() ###
#' @noRd
dtype0.logical <- function(x){
  
  ## 0-Length List ##
  v <- logical(length = 0)
  
  ## Output ##
  return(v)
  
}


### dtype0.ordered() ###
#' @noRd
dtype0.ordered <- function(x){
  
  ## 0-Length Vector ##
  v <- ordered(character(length = 0L))
  
  ## Output ##
  return(v)
  
}


### dtype0.POSIXct() ###
#' @noRd
dtype0.POSIXct <- function(x){
  
  ## 0-Length Vector ##
  v <- as.POSIXct(character(length = 0L), tz = "UTC")
  
  ## Output ##
  return(v)
  
}


### dtype0.POSIXlt() ###
#' @noRd
dtype0.POSIXlt <- function(x){
  
  ## 0-Length Vector ##
  v <- as.POSIXlt(character(length = 0L), tz = "UTC")
  
  ## Output ##
  return(v)
  
}


### dtype0.tbl_df() ###
#' @noRd
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
