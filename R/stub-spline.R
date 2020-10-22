#' @title
#' Derive functions to generate synthetic data using linear spline interpolation
#'
#' @description
#' Internal function for producing functions to generate synthetic data using
#' linear spline interpolation of the empirical cumulative distribution
#' function.
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
#' @importFrom grDevices nclass.FD
#' @importFrom graphics hist
#' @importFrom stats approxfun quantile sd


### ToDo ###
# - Create a method for ordered factors.


### stub_spline() ###
#' @noRd
stub_spline <- function(col, ...){
  
  ## Define S3 Method ##
  UseMethod("stub_spline", col)
  
}


### stub_spline.default() ###
#' @export
stub_spline.default <- function(col, ...){

  ## Warning ##
  .warning_no_method(col)

  ## Define Function ##
  f <- function(v){rep(NA_integer_, length(v))}

  ## Form Output ##
  out <- list(
    fun = f
  )

  ## Output ##
  return(out)

}


### stub_spline.Date() ###
#' @export
stub_spline.Date <- function(col, ctrl){

  ## Coerce to Integer ##
  col <- as.integer(col)

  ## Define Spline Function ##
  out <- stub_spline_(col = col, ctrl = ctrl)

  ## Output ##
  return(out)

}


### stub_spline.double() ###
#' @export
stub_spline.double <- function(col, ctrl){

  ## Define Spline Function ##
  out <- stub_spline_(col = col, ctrl = ctrl)

  ## Output ##
  return(out)

}


### stub_spline.integer() ###
#' @export
stub_spline.integer <- function(col, ctrl){

  ## Define Spline Function ##
  out <- stub_spline_(col = col, ctrl = ctrl)

  ## Output ##
  return(out)

}


### stub_spline.integer64() ###
#' @export
stub_spline.integer64 <- function(col, ctrl){

  ## Coerce to double ##
  col <- as.double(col)

  ## Define Spline Function ##
  out <- stub_spline_(col = col, ctrl = ctrl)

  ## Output ##
  return(out)

}


### stub_spline.ITime() ###
#' @export
stub_spline.ITime <- function(col, ctrl){
  
  ## Coerce to integer ##
  col <- as.integer(col)
  
  ## Define Spline Function ##
  out <- stub_spline_(col = col, ctrl = ctrl)
  
  ## Output ##
  return(out)
  
}


### stub_spline.POSIXct() ###
#' @export
stub_spline.POSIXct <- function(col, ctrl){

  ## Coerce to Integer ##
  col <- as.integer(col)

  ## Define Spline Function ##
  out <- stub_spline_(col = col, ctrl = ctrl)

  ## Output ##
  return(out)

}


### stub_spline.POSIXlt() ###
#' @export
stub_spline.POSIXlt <- function(col, ctrl){

  ## Coerce to POSIXct ##
  col <- as.POSIXct(col, tz = ctrl[["dttm_tz"]])

  ## Use POSIXct Method ##
  out <- stub_spline.POSIXct(col = col, ctrl = ctrl)

  ## Output ##
  return(out)

}


### stub_spline_() ###
#' @noRd
stub_spline_ <- function(col, ctrl){
  
  ## Checks ##
  if (ctrl[["emp_tail_exc"]] < 0 | ctrl[["emp_tail_exc"]] >= 0.5)
    stop("The 'emp_tail_exc' control parameter must be between 0 and 0.5.")
  if (sign(ctrl[["emp_fuzz_spl"]]) == -1)
    stop("The 'emp_fuzz_spl' control parameter must be a positive value.")
  
  ## Omit NA Values ##
  col <- col[!is.na(col)]
  
  if (length(col) >= 2){
    
    ## Fuzz ##
    if (ctrl[["emp_fuzz_spl"]] > 0){
      
      col <- col + rnorm(n = length(col), mean = 0, sd = sd(col)*ctrl[["emp_fuzz_spl"]])
      
    }
    
    ## Tail Exclusions ##
    limits <- quantile(
      x = col,
      probs = c(0 + ctrl[["emp_tail_exc"]], 1 - ctrl[["emp_tail_exc"]]),
      names = FALSE
    )
    col <- col[col >= limits[1] & col <= limits[2]]
    
    ## Determine Optimal Number of Breaks ##
    nclass <- nclass.FD(col)
    breaks <- seq(
      from = min(col),
      to = max(col),
      length.out = nclass + 1L
    )
    
    ## Histogram Object ##
    h <- hist(x = col, breaks = breaks, plot = FALSE)
    
    ## Cumulative Sum of Densities
    width <- diff(h[["breaks"]])
    area <- width*h[["density"]]
    cdf <- c(0, cumsum(area))
    
    ## Generate a Spline Function ##
    f <- approxfun(x = cdf, y = breaks, rule = 1, ties = list("ordered", min))
    
  } else {
    
    ## Warning ##
    warning("Insufficient values to estimate the ECDF.")
    
    ## NULL Function ##
    f <- function(v){rep(NA_integer_, length(v))}
    
  }
  
  ## Form Output ##
  out <- list(
    fun = f
  )
  
  ## Output ##
  return(out)
  
}
