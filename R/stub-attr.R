#' @title
#' Encode vector attributes
#'
#' @description
#' Internal function to encode vector attributes into a list.
#'
#' @concept empirical
#' @concept ecdf
#' @concept sample
#' @concept simulate
#' @concept simulated
#' @concept simulation
#'
#' @keywords datagen


### Notes ###
# - stub_attr_.ordered() may need to change if stub_attr_.ordered() is finalised.


### stub_attr() ###
#' @noRd
stub_attr <- function(col, elements = length(col), method = "agnostic", index = 1L, ..., ctrl = list()){
  
  ## Set Control Parameters ##
  ctrl <- stubble_ctrl(..., old_ctrl = ctrl, index = index)
  
  ## Class ##
  dtype <- dtype0(x = col)
  
  ## Missing Values ##
  p_na <- switch(
    EXPR = method,
    agnostic = if (is.na(ctrl[["p_na"]])) 0 else ctrl[["p_na"]],
    empirical = {
      if (is.na(ctrl[["p_na"]])) {
        x <- sum(is.na(col))/length(col)
        x <- if (is.nan(x)) 0 else x
      } else {
        ctrl[["p_na"]]
      }
    }
  )
  
  ## Method ##
  if (method != "agnostic") method <- stub_attr_(col = col, ctrl = ctrl)
  
  ### Generate sim ##
  sim <- switch(
    EXPR = method,
    agnostic = c(),
    spline = stub_spline(col = col, ctrl = ctrl),
    sample = stub_sample(col = col, ctrl = ctrl)
  )
  sim <- append(list(method = method), sim)
  
  ## Form Output ##
  out <- list(
    dtype = dtype,
    n = elements,
    p_na = p_na,
    sim = sim
  )
  
  ## Output ##
  return(out)
  
}

### stub_attr_() ###
#' @noRd
stub_attr_ <- function(col, ...){
  
  ## Define S3 Method ##
  UseMethod("stub_attr_", col)
  
}


### stub_attr_.default() ###
#' @export
stub_attr_.default <- function(col, ctrl){
  
  ## Warning ##
  .warning_no_method(col)
  
  ## Sample Method ##
  method <- "sample"
  
  ## Output ##
  return(method)
  
}


### stub_attr_.character() ###
#' @export
stub_attr_.character <- function(col, ctrl){
  
  ## Sample Method ##
  method <- "sample"
  
  ## Output ##
  return(method)
  
}


### stub_attr_.Date() ###
#' @export
stub_attr_.Date <- function(col, ctrl){
  
  ## Determine Method ##
  method <- stub_attr__(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### stub_attr_.factor() ###
#' @export
stub_attr_.factor <- function(col, ctrl){
  
  ## Use character Method ##
  method <- stub_attr_.character(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### stub_attr_.integer64() ###
#' @export
stub_attr_.integer64 <- function(col, ctrl){
  
  ## Determine Method ##
  method <- stub_attr__(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### stub_attr_.ITime() ###
#' @export
stub_attr_.ITime <- function(col, ctrl){
  
  ## Determine Method ##
  method <- stub_attr__(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### stub_attr_.logical() ###
#' @export
stub_attr_.logical <- function(col, ctrl){
  
  ## Use character Method ##
  method <- stub_attr_.character(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### stub_attr_.numeric() ###
#' @export
stub_attr_.numeric <- function(col, ctrl){
  
  ## Determine Method ##
  method <- stub_attr__(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### stub_attr_.POSIXct() ###
#' @export
stub_attr_.POSIXct <- function(col, ctrl){
  
  ## Determine Method ##
  method <- stub_attr__(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### stub_attr_.POSIXlt() ###
#' @export
stub_attr_.POSIXlt <- function(col, ctrl){
  
  ## Coerce to POSIXct ##
  col <- as.POSIXct(col)
  
  ## Use POSIXct Method ##
  method <- stub_attr__(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### stub_attr__() ###
#' @noRd
stub_attr__ <- function(col, ctrl){
  
  ## Checks ##
  if (ctrl[["emp_sw"]] < 0 | ctrl[["emp_sw"]] > 1)
    stop("The 'emp_sw' control parameter must be between 0 and 1.")
  
  ## emp_sw Override ##
  if (ctrl[["emp_sw"]] == 0) {
    
    # Always Sample #
    method <- "spline"
    
  } else if (ctrl[["emp_sw"]] == 1){
    
    # Always Spline #
    method <- "sample"
    
  } else {
    
    # Complete Cases Only #
    cc_col <- col[!is.na(col)]
    
    # Only Assess Vectors Containing Non-Missing/Zero-Length Data #
    if (length(cc_col) != 0){
      
      # Uniqueness #
      p_uniq <- length(unique(cc_col))/length(cc_col)
      
      # Method Selection #
      method <- if (p_uniq > ctrl[["emp_sw"]]) "spline" else "sample"
      
    } else {
      
      # Zero Data Fallback #
      method <- "sample"
      
    }
    
  }
  
  ## Output ##
  return(method)
  
}
