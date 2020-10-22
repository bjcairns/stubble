#' @title
#' Generate synthetic data using sampling methods
#' 
#' @description
#' Internal function for generating synthetic data using sampling methods.
#' 
#' @concept empirical
#' @concept ecdf
#' @concept sample
#' @concept simulate
#' @concept simulated
#' @concept simulation
#' 
#' @keywords datagen


### ble_sample() ###
#' @noRd
ble_sample <- function(x, elements, ctrl){
  
  ## Extract stub Params ##
  val <- x[["sim"]][["values"]]
  wt <- x[["sim"]][["wt"]]
  
  ## OO Class Gatekeeper ##
  syn_col <- ble_sample_(val = val, elements = elements, wt = wt, ctrl = ctrl)
  
  ## Output ##
  return(syn_col)
  
}


### ble_sample_() ###
#' @noRd
ble_sample_ <- function(val, ...){
  
  ## Define S3 Method ##
  UseMethod("ble_sample_", val)
  
}


### ble_sample_.default() ###
#' @export
ble_sample_.default <- function(val, elements, ...){
  
  ## Warning ##
  .warning_no_method(val)
  
  ## Generate NA Data ##
  syn_col <- rep(NA_integer_, elements)
  
  ## Output ##
  return(syn_col)
  
}


### ble_sample_.character() ###
#' @export
ble_sample_.character <- function(val, elements, wt, ctrl){
  
  ## Call Internal Function ##
  syn_col <- ble_sample__(val = val, elements = elements, wt = wt)
  
  ## Output ##
  return(syn_col)
  
}


### ble_sample_.Date() ###
#' @export
ble_sample_.Date <- function(val, elements, wt, ctrl){
  
  ## Call Internal Function ##
  syn_col <- ble_sample__(val = val, elements = elements, wt = wt)
  
  ## Output ##
  return(syn_col)
  
}


### ble_sample_.factor() ###
#' @export
ble_sample_.factor <- function(val, elements, wt, ctrl){
  
  ## Call Internal Function ##
  syn_col <- ble_sample__(val = val, elements = elements, wt = wt)
  
  ## Output ##
  return(syn_col)
  
}


### ble_sample_.IDate() ###
#' @export
ble_sample_.IDate <- function(val, elements, wt, ctrl){
  
  ## Attempt Use of Current Class ##
  if (getOption("stubble_has_data.table")) {
  
    # Call Internal Function #
    syn_col <- ble_sample__(val = val, elements = elements, wt = wt)
    
  } else {
    
    # Warning #
    .warning_coercion(val)
    
    # Coerce to Date #
    val <- as.Date(val)
    
    # Use Date Method #
    syn_col <- ble_sample_.Date(val = val, elements = elements, wt = wt)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_sample_.integer64() ###
#' @export
ble_sample_.integer64 <- function(val, elements, wt, ctrl){
  
  ## Attempt Use of Current Class ##
  if (getOption("stubble_has_bit64")) {
    
    # Call Internal Function #
    syn_col <- ble_sample__(val = val, elements = elements, wt = wt)
    
  } else {
    
    # Warning #
    .warning_coercion(val)
    
    # Coerce to double #
    val <- as.double(val)
    
    # Use numeric Method #
    syn_col <- ble_sample_.numeric(val = val, elements = elements, wt = wt)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_sample_.ITime() ###
#' @export
ble_sample_.ITime <- function(val, elements, wt, ctrl){
  
  ## Attempt Use of Current Class ##
  if (getOption("stubble_has_data.table")) {
    
    # Call Internal Function #
    syn_col <- ble_sample__(val = val, elements = elements, wt = wt)
    
  } else {
    
    # Warning #
    .warning_coercion(val)
    
    # Coerce to POSIXct #
    val <- as.POSIXct(val, tz = ctrl[["dttm_tz"]])
    
    # Use POSIXt Method #
    syn_col <- ble_sample_.POSIXt(val = val, elements = elements, wt = wt)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_sample_.logical() ###
#' @export
ble_sample_.logical <- function(val, elements, wt, ctrl){
  
  ## Call Internal Function ##
  syn_col <- ble_sample__(val = val, elements = elements, wt = wt)
  
  ## Output ##
  return(syn_col)
  
}


### ble_sample_.POSIXt() ###
#' @export
ble_sample_.POSIXt <- function(val, elements, wt, ctrl){
  
  ## Call Internal Function ##
  syn_col <- ble_sample__(val = val, elements = elements, wt = wt)
  
  ## Output ##
  return(syn_col)
  
}


### ble_sample_.numeric() ###
#' @export
ble_sample_.numeric <- function(val, elements, wt, ctrl){
  
  ## Call Internal Function ##
  syn_col <- ble_sample__(val = val, elements = elements, wt = wt)
  
  ## Output ##
  return(syn_col)
  
}


### ble_sample__() ###
#' @noRd
ble_sample__ <- function(val, elements, wt){
  
  ## Sample if Possible ##
  syn_col <- if (length(val) != 0) {
    
    # Re-Sample #
    val[sample.int(n = length(val), size = elements, replace = TRUE, prob = wt)]
    
  } else {
    
    # Warning #
    .warning_zero_sample()
    
    # Zero-Length Value #
    val
    
  }
  
  ## Output ##
  return(syn_col)
  
}
