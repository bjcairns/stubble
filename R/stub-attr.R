#' @title
#' Encode vector attributes
#'
#' @description
#' Internal function to encode vector attributes into a list.
#'
#' @concept empirical
#' @concept ecdf
#' @concept resample
#' @concept simulate
#' @concept simulated
#' @concept simulation
#'
#' @keywords datagen


### Notes ###
# - stub_method.ordered() may need to change if stub_method_.ordered() is finalised.


### stub_attr() ###
#' @noRd
stub_attr <- function(col, elements = length(col), index = 1L, ctrl = list(), ...){

  ## Set Control Parameters ##
  ctrl <- stubble_ctrl(..., old_ctrl = ctrl, index = index)

  ## Data Extraction ##
  p_na <- ctrl[["p_na"]]

  ## Class ##
  dtype <- dtype0(x = col)

  ## Method ##
  method <- stub_method(col = col, ctrl = ctrl)

  ### Generate sim ##
  sim <- switch(method,
                spline = stub_spline(col = col, ctrl = ctrl),
                sample = stub_sample(col = col, ctrl = ctrl))
  sim <- append(list(method = method), sim)

  ## Missing Values ##
  p_na <- if (is.na(p_na)) sum(is.na(col))/length(col) else p_na

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

### stub_method() ###
#' @noRd
stub_method <- function(col, ...){

  ## Define S3 Method ##
  UseMethod("stub_method", col)

}


### stub_method.default() ###
#' @noRd
stub_method.default <- function(col, ...){

  ## Warning ##
  .warning_no_method(col)

  method <- "sample"

  ## Output ##
  return(method)

}


### stub_method.character() ###
#' @noRd
stub_method.character <- function(col, ctrl){

  ## Sample Method ##
  method <- "sample"

  ## Output ##
  return(method)

}


### stub_method.Date() ###
#' @noRd
stub_method.Date <- function(col, ctrl){

  ## Determine Method ##
  method <- stub_method_(col = col, ctrl = ctrl)

  ## Output ##
  return(method)

}


### stub_method.factor() ###
#' @noRd
stub_method.factor <- function(col, ctrl){

  ## Use character Method ##
  method <- stub_method.character(col = col, ctrl = ctrl)

  ## Output ##
  return(method)

}


### stub_method.integer64() ###
#' @noRd
stub_method.integer64 <- function(col, ctrl){

  ## Determine Method ##
  method <- stub_method_(col = col, ctrl = ctrl)

  ## Output ##
  return(method)

}


### stub_method.logical() ###
#' @noRd
stub_method.logical <- function(col, ctrl){

  ## Use character Method ##
  method <- stub_method.character(col = col, ctrl = ctrl)

  ## Output ##
  return(method)

}


### stub_method.numeric() ###
#' @noRd
stub_method.numeric <- function(col, ctrl){

  ## Determine Method ##
  method <- stub_method_(col = col, ctrl = ctrl)

  ## Output ##
  return(method)

}


### stub_method.POSIXct() ###
#' @noRd
stub_method.POSIXct <- function(col, ctrl){

  ## Determine Method ##
  method <- stub_method_(col = col, ctrl = ctrl)

  ## Output ##
  return(method)

}


### stub_method.POSIXlt() ###
#' @noRd
stub_method.POSIXlt <- function(col, ctrl){

  ## Coerce to POSIXct ##
  col <- as.POSIXct(col)

  ## Use POSIXct Method ##
  method <- stub_method_(col = col, ctrl = ctrl)

  ## Output ##
  return(method)

}


### stub_method_() ###
#' @noRd
stub_method_ <- function(col, ctrl){

  ## Checks ##
  if (!is.numeric(ctrl[["emp_sw"]]))
    stop("The 'emp_sw' control parameter must be of class numeric.")
  if (ctrl[["emp_sw"]] < 0 | ctrl[["emp_sw"]] > 1)
    stop("The 'emp_sw' control parameter must be between 0 and 1.")

  ## Complete Cases Only ##
  cc_col <- col[!is.na(col)]

  ## Only Assess Vectors Containing Non-Missing/Zero-Length Data ##
  if (length(cc_col) != 0){

    ## Uniqueness ##
    p_uniq <- length(unique(cc_col))/length(cc_col)

    ## Method Selection ##
    method <- if(p_uniq > ctrl[["emp_sw"]]) "spline" else "sample"

  } else {

    method <- "sample"

  }

  ## Output ##
  return(method)

}
