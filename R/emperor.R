#' @title
#' Generate synthetic data using the empirical distribution of an R vector
#' 
#' @description
#' `emperor()` does the real work for [stubblise()]. Currently supported column
#' classes are `numeric`, `integer`, `character`, `factor`, `ordered`, `logical`,
#' `POSIXct`, `POSIXlt`, `Date` and `IDate`. There is limited support for `list` (returns a list
#' with all `NA`s).
#' 
#' @param col the vector from which the type of the synthetic data is taken.
#' @param elements the number of elements to generate.
#' @param index the index of the column (i.e. the position in the data frame)
#' for the purposes of extracting control parameters.
#' @param control a named list of control parameters for generating the
#' synthetic data. See [control].
#' @param ... named individual control parameters, which take precedence over
#' those in the `control` list.
#'
#' @details
#' `emperor()` calls an internal S3 generic function, `emperor_()`,
#' with built-in methods for base R vector types.
#' 
#' All methods return values sampled uniformly at random, with the exception of
#' `emperor_.character()`, which samples string lengths uniformly at random and
#' then populates those strings with symbols (strings of one or more
#' characters) chosen uniformly at random from the values in the `chr_sym`
#' control parameter (see [emperor_control()]). Character results of
#' `emperor()` are therefore not chosen uniformly at random from the set of all
#' strings which are valid according to the control parameters; short strings
#' are overrepresented.
#' 
#' @return
#' Returns a vector of the same class as `col` with `elements` elements.
#' 
#' @note
#' This function is entirely experimental and should not be used yet. There are
#' currently risks of data leakage and many of the methods do not yet work.
#' 
#' @author
#' Benjamin G. Feakins, \email{benjamin.feakins@@ndph.ox.ac.uk}
#' 
#' @seealso
#' \code{\link{gen_col}}
#' 
#' @examples
#' emperor(iris$Sepal.length)
#' emperor(iris$Species)
#' 
#' @keywords empirical
#' @keywords ecdf
#' @keywords simulate
#' @keywords simulated
#' @keywords simulation
#' 
#' @importFrom stats ecdf quantile runif
#' @importFrom utils installed.packages


### TODO ###
# - Check for a certain proportion of values occurring < p or > p.
# - Add fuzz to each double equal to 100th range of distribution. Ensure Values do not exceed range.
# - Add `tails` parameter to emperor_control.
# - Find why devtools::check() if flagging emperor() as being undocumented.
# - Leverage gen_col_control() or make a new emperor_control().
# - Make use of ecdf() in the other categorical functions.
# - Split out emperor_.numeric() into emperor_.integer() and emperor_.double().


### var_ident() ###
#' @noRd
var_ident <- function(col){
  type <- class(col)
  if(type == "numeric"){
    if(any(col %% 1 != 0)){
      type <- "double"
    } else {
      type <- "integer"
    }
  }
  
  ## Output ##
  return(type)
}


### emperor() ###
#' @export
emperor <- function(col, elements = length(col), ...){
  syn_col <- emperor_(col, elements)
  
  if(!exists("syn_col")){
    syn_col <- emperor_.default(col, elements)
  }
  
  ## Output ##
  return(syn_col)
}


### emperor_() ###
#' @noRd
emperor_ <- function(col, ...){
  UseMethod("emperor_", col)
}


### emperor_.default() ###
#' @export
emperor_.default <- function(col, elements){
  warning("Could not generate data for ", class(col), "; returning NAs")
  rep(NA_integer_, elements)
}


### emperor_.logical() ###
#' @export
emperor_.logical <- function(col, elements){
  syn_col <- rep(NA, elements)
  
  ## Output ##
  return(syn_col)
}


### emperor_.numeric() ###
#' @export
emperor_.numeric <- function(col, elements, tails = T, tailsize = 0.05){
  ## Type Identification ##
  type <- var_ident(col)
  
  if(type == "double"){
    ## Empirical CDF ##
    fn <- ecdf(col)
    
    ## Tail Omission ##
    if(tails){
      p <- runif(elements)
    } else {
      p <- runif(elements, 0 + tailsize, 1 - tailsize)
    }
    
    ## Quantile ECDF ##
    syn_col <- quantile(fn, p)
    
    ## Strip Quantile Values ##
    names(syn_col) <- NULL
    
  } else if(type == "integer"){
    syn_col <- sample(col, elements, replace = T)
  } else {
    emperor_.default(col, elements)
  }
  
  ## Output ##
  return(syn_col)
}


### emperor_.factor() ###
#' @export
emperor_.factor <- function(col, elements){
  syn_col <- factor(rep(NA_integer_, elements))
  syn_col <- addNA(syn_col)
  
  ## Output ##
  return(syn_col)
}


### emperor_.ordered() ###
#' @export
emperor_.ordered <- function(col, elements){
  syn_col <- ordered(rep(NA_integer_, elements))
  syn_col <- addNA(syn_col)
  
  ## Output ##
  return(syn_col)
}


### emperor_.character() ###
#' @export
emperor_.character <- function(col, elements){
  syn_col <- rep(NA_character_, elements)
  
  ## Output ##
  return(syn_col)
}


### emperor_.POSIXct() ###
#' @export
emperor_.POSIXct <- function(col, elements){
  syn_col <- as.POSIXct(rep(NA_integer_, elements), origin = "1970-01-01", tz = "UTC")
  
  ## Output ##
  return(syn_col)
}


### emperor_.POSIXlt() ###
#' @export
emperor_.POSIXlt <- function(col, elements){
  syn_col <- as.POSIXlt(rep(NA_integer_, elements), origin = "1970-01-01", tz = "UTC")
  
  ## Output ##
  return(syn_col)
}


### emperor_.Date() ###
#' @export
emperor_.Date <- function(col, elements){
  syn_col <- as.Date(rep(NA_integer_, elements), origin = "1970-01-01", tz = "UTC")
  
  ## Output ##
  return(syn_col)
}


### emperor_.IDate() ###
#' @export
emperor_.IDate <- function(col, elements){
  if("data.table" %in% rownames(installed.packages())){
    syn_col <- data.table::as.IDate(rep(NA_integer_, elements), origin = "1970-01-01", tz = "UTC")
  } else {
    warning("Package 'data.table' not found. IDates will be converted to Dates.")
    emperor_.Date(col, elements)
  }
  
  ## Output ##
  return(syn_col)
}


### emperor_.list() ###
#' @export
gen_col_.list <- function(col, elements){
  syn_col <- as.list(rep(NA_integer_, elements))
  
  ## Output ##
  return(syn_col)
}
