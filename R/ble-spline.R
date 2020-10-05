#' @title
#' Generate synthetic data using linear spline interpolation
#'
#' @description
#' Internal function for generating synthetic data using linear spline
#' interpolation of the empirical cumulative distribution function.
#'
#' @concept empirical
#' @concept ecdf
#' @concept sample
#' @concept simulate
#' @concept simulated
#' @concept simulation
#'
#' @keywords datagen
#'
#' @importFrom stats rnorm runif


### ToDo ###
# - Evaluate need for setting the origin when coercing from POSIXct to POSIXlt.


### ble_spline() ###
#' @noRd
ble_spline <- function(x, elements, ctrl){
  
  ## Extract stub Params ##
  dtype <- x[["dtype"]]
  f <- x[["sim"]][["fun"]]
  col_sd <- x[["sim"]][["sd"]]
  
  ## Generate Data ##
  v <- runif(n = elements)
  syn_col <- f(v = v)
  
  ## Coerce Output ##
  syn_col <- ble_spline_(dtype = dtype, syn_col = syn_col, ctrl = ctrl)
  
  ## Output ##
  return(syn_col)
  
}


### ble_spline_() ###
#' @noRd
ble_spline_ <- function(dtype, ...){
  
  ## Define S3 Method ##
  UseMethod("ble_spline_", dtype)
  
}


### ble_spline_.default() ###
#' @export
ble_spline_.default <- function(dtype, ...){
  
  ## Error ##
  .stop_no_method(dtype)
  
}


### ble_spline_.Date() ###
#' @export
ble_spline_.Date <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Coerce to Date ##
  syn_col <- as.Date(syn_col, origin = "1970-01-01")
  
  ## Output ##
  return(syn_col)
  
}


### ble_spline_.double() ###
#' @export
ble_spline_.double <- function(dtype, syn_col, ctrl){
  
  ## Coerce to double ##
  syn_col <- as.double(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### ble_spline_.IDate() ###
#' @export
ble_spline_.IDate <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Attempt Coercion to IDate ##
  syn_col <- if (is.installed.package("data.table")) {
    
    # Coerce to IDate #
    data.table::as.IDate(syn_col, origin = "1970-01-01")
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
    # Coerce to Date #
    NextMethod(dtype)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_spline_.integer() ###
#' @export
ble_spline_.integer <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Coerce to integer ##
  syn_col <- as.integer(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### ble_spline_.integer64() ###
#' @export
ble_spline_.integer64 <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Attempt Coercion to integer64 ##
  syn_col <- if (is.installed.package("bit64")) {
    
    # Coerce to integer64 #
    bit64::as.integer64(syn_col)
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
    # Coerce to double #
    as.double(syn_col)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_spline_.ITime() ###
#' @export
ble_spline_.ITime <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Attempt Coercion to ITime ##
  syn_col <- if (is.installed.package("data.table")) {
    
    # Coerce to ITime #
    data.table::as.ITime(syn_col, origin = "1970-01-01")
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
    # Coerce to POSIXct #
    as.POSIXct(syn_col, origin = "1970-01-01", tz = ctrl[["dttm_tz"]])
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_spline_.POSIXct() ###
#' @export
ble_spline_.POSIXct <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Coerce to POSIXct ##
  syn_col <- as.POSIXct(syn_col, origin = "1970-01-01", tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}


### ble_spline_.POSIXlt() ###
#' @export
ble_spline_.POSIXlt <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Coerce to POSIXct ##
  syn_col <- as.POSIXct(syn_col, origin = "1970-01-01", tz = ctrl[["dttm_tz"]])
  
  ## Coerce to POSIXlt ##
  syn_col <- as.POSIXlt(syn_col, tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}
