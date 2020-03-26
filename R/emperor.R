#' @title
#' Generate synthetic data using the empirical distribution of an R vector
#' 
#' @description
#' `emperor()` does the real work for [stubblise()]. Currently supported column
#' classes are `numeric`, `integer`, `character`, `factor`, `ordered`, `logical`,
#' `POSIXct`, `POSIXlt`, `Date` and `IDate`. There is limited support for `list`
#' (returns a list with all `NA`s).
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
#' All methods return values sampled from the cumulative empirical distribution
#' function, with the exception of `emperor_.character()`, which samples string
#' lengths uniformly at random and then populates those strings with symbols
#' (strings of one or more characters) chosen uniformly at random from the
#' values in the `chr_sym` control parameter (see [emperor_control()]).
#' Character results of `emperor()` are therefore not chosen uniformly at random
#' from the set of all strings which are valid according to the control
#' parameters; short strings are overrepresented.
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
#' [emperor_control()]
#' 
#' @examples
#' emperor(iris$Sepal.length)
#' emperor(iris$Species)
#' 
#' @keywords empirical
#' @keywords ecdf
#' @keywords resample
#' @keywords simulate
#' @keywords simulated
#' @keywords simulation
#' 
#' @importFrom stats ecdf quantile runif
#' @importFrom utils installed.packages


### TODO ###
# - Decide how and when to implement emperor() in stubble.
# - Check for a certain proportion of values occurring < p or > p.
# - Add some means to determine whether sample() or ecdf() is used for any given integer.
# - Add fuzz to each double. Ensure Values do not exceed range.
# - Find why devtools::check() if flagging emperor() as being undocumented.
# - Split out emperor_.numeric() into emperor_.integer() and emperor_.double().
# - Check how emperor_.factor() and emperor_.ordered() handle columns in which addNA() has been used.
# - See what gen_col() is doing with the date origins in gen_con_ctrl(). I can't see why this needs to be settable.
# - Add checks for rounding and try to mirror the number of dps of sig figs in the output.


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
emperor <- function(col, elements = length(col), index = 1L, control = list(), ...){
  
  ## Set Control Parameters ##
  this_ctrl <- emperor_control(..., old_ctrl = control, index = index)
  
  ## Simulate According to Data Type ##
  syn_col <- emperor_(col, elements = elements, ctrl = this_ctrl)
  
  ## Fallback Simulation Method ##
  if(!exists("syn_col")){
    syn_col <- emperor_.default(col, elements = elements, ctrl = this_ctrl)
  }
  
  ## Handle NA Values ##
  if(!is.na(this_ctrl[["p_na"]])){
    p_na <- as.double(this_ctrl[["p_na"]])
  } else {
    p_na <- sum(is.na(col))/length(col)
  }
  if(p_na != 0){
    if(p_na > 1){
      warning("Control parameter p_na > 1; value has been reset to 1")
      p_na <- 1
    } else if(p_na == 1){
      syn_col[] <- NA
    } else {
      syn_col[rbinom(elements, 1L, p_na) == 1L] <- NA
    }
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
emperor_.default <- function(col, elements = elements, ctrl){
  warning("Could not generate data for ", class(col), "; returning NAs")
  rep(NA_integer_, elements)
}


### emperor_.logical() ###
#' @export
emperor_.logical <- function(col, elements = elements, ctrl){
  
  ## Tabulate Values ##
  p_obs <- prop.table(table(col))
  
  ## Simulate Values ##
  if(length(p_obs != 0)){
    syn_col <- sample(as.logical(names(p_obs)), size = elements, replace = TRUE, prob = p_obs)
  } else {
    syn_col <- rep(NA, elements)
  }
  
  ## Output ##
  return(syn_col)
}


### emperor_.numeric() ###
#' @export
emperor_.numeric <- function(col, elements = elements, ctrl){
  ## Type Identification ##
  type <- var_ident(col)
  
  if(type == "double"){
    ## Empirical CDF ##
    fn <- ecdf(col)
    
    ## Tail Omission ##
    p <- runif(elements, 0 + ctrl[["tailsize"]], 1 - ctrl[["tailsize"]])
    
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
emperor_.factor <- function(col, elements = elements, ctrl){
  
  ## Tabulate Values ##
  p_obs <- prop.table(table(col))
  
  ## Simulate Values ##
  if(length(p_obs != 0)){
    syn_col <- sample(names(p_obs), size = elements, replace = TRUE, prob = p_obs)
  } else {
    syn_col <- rep(NA_integer_, elements)
  }
  
  ## Coerce to Factor ##
  syn_col <- factor(syn_col, levels = names(p_obs))
  
  ## Output ##
  return(syn_col)
}


### emperor_.ordered() ###
#' @export
emperor_.ordered <- function(col, elements = elements, ctrl){
  
  ## Tabulate Values ##
  p_obs <- prop.table(table(col))
  
  ## Simulate Values ##
  if(length(p_obs != 0)){
    syn_col <- sample(names(p_obs), size = elements, replace = TRUE, prob = p_obs)
  } else {
    syn_col <- rep(NA_integer_, elements)
  }
  
  ## Coerce to Factor ##
  syn_col <- ordered(syn_col, levels = names(p_obs))
  
  ## Output ##
  return(syn_col)
}


### emperor_.character() ###
#' @export
emperor_.character <- function(col, elements = elements, ctrl){
  syn_col <- rep(NA_character_, elements)
  
  ## Output ##
  return(syn_col)
}


### emperor_.POSIXct() ###
#' @export
emperor_.POSIXct <- function(col, elements = elements, ctrl){
  
  ## Empirical CDF ##
  fn <- ecdf(col)
  
  ## Tail Omission ##
  p <- runif(elements, 0 + ctrl[["tailsize"]], 1 - ctrl[["tailsize"]])
  
  ## Quantile ECDF ##
  syn_col <- quantile(fn, p)
  
  ## Strip Quantile Values ##
  names(syn_col) <- NULL
  
  ## Coerce to POSIXct ##
  syn_col <- as.POSIXct(round(syn_col), origin = "1970-01-01", tz = ctrl[["dtm_tz"]])
  
  ## Output ##
  return(syn_col)
}


### emperor_.POSIXlt() ###
#' @export
emperor_.POSIXlt <- function(col, elements = elements, ctrl){
  
  ## Coerce to POSIXct ##
  col <- as.POSIXct(col)
  
  ## Empirical CDF ##
  fn <- ecdf(col)
  
  ## Tail Omission ##
  p <- runif(elements, 0 + ctrl[["tailsize"]], 1 - ctrl[["tailsize"]])
  
  ## Quantile ECDF ##
  syn_col <- quantile(fn, p)
  
  ## Strip Quantile Values ##
  names(syn_col) <- NULL
  
  ## Coerce to POSIXlt ##
  syn_col <- as.POSIXlt(round(syn_col), origin = "1970-01-01", tz = ctrl[["dtm_tz"]])
  
  ## Output ##
  return(syn_col)
}


### emperor_.Date() ###
#' @export
emperor_.Date <- function(col, elements = elements, ctrl){
  
  ## Empirical CDF ##
  fn <- ecdf(col)
  
  ## Tail Omission ##
  p <- runif(elements, 0 + ctrl[["tailsize"]], 1 - ctrl[["tailsize"]])
  
  ## Quantile ECDF ##
  syn_col <- quantile(fn, p)
  
  ## Strip Quantile Values ##
  names(syn_col) <- NULL
  
  ## Coerce to Date ##
  syn_col <- as.Date(round(syn_col), origin = "1970-01-01", tz = ctrl[["dtm_tz"]])
  
  ## Output ##
  return(syn_col)
}


### emperor_.IDate() ###
#' @export
emperor_.IDate <- function(col, elements = elements, ctrl){
  if("data.table" %in% rownames(installed.packages())){
    
    ## Empirical CDF ##
    fn <- ecdf(col)
    
    ## Tail Omission ##
    p <- runif(elements, 0 + ctrl[["tailsize"]], 1 - ctrl[["tailsize"]])
    
    ## Quantile ECDF ##
    syn_col <- quantile(fn, p)
    
    ## Strip Quantile Values ##
    names(syn_col) <- NULL
    
    ## Coerce to Date ##
    syn_col <- data.table::as.IDate(round(syn_col), origin = "1970-01-01", tz = ctrl[["dtm_tz"]])
    
  } else {
    warning("Package 'data.table' not found. IDates will be converted to Dates.")
    emperor_.Date(col, elements)
  }
  
  ## Output ##
  return(syn_col)
}


### emperor_.list() ###
#' @export
gen_col_.list <- function(col, elements = elements, ctrl){
  syn_col <- as.list(rep(NA_integer_, elements))
  
  ## Output ##
  return(syn_col)
}
