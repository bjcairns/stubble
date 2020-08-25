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
#' @concept resample
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
  
  UseMethod("stub_spline", col)
  
}


### stub_spline.default() ###
#' @noRd
stub_spline.default <- function(col, ...){
  
  ## Warning ##
  .warning_no_method(col)
  
  ## Define Function ##
  f <- function(v){rep(NA_integer_, v)}
  
  ## Form Output ##
  out <- list(
    fun = f,
    sd = NA_real_
  )
  
  ## Output ##
  return(out)
  
}


### stub_spline.Date() ###
#' @noRd
stub_spline.Date <- function(col, ctrl){
  
  ## Coerce to Integer ##
  col <- as.integer(col)
  
  ## Define Spline Function ##
  out <- stub_spline_(col = col, ctrl = ctrl)
  
  ## Output ##
  return(out)
  
}


### stub_spline.double() ###
#' @noRd
stub_spline.double <- function(col, ctrl){
  
  ## Define Spline Function ##
  out <- stub_spline_(col = col, ctrl = ctrl)
  
  ## Output ##
  return(out)
  
}


### stub_spline.integer() ###
#' @noRd
stub_spline.integer <- function(col, ctrl){
  
  ## Define Spline Function ##
  out <- stub_spline_(col = col, ctrl = ctrl)
  
  ## Output ##
  return(out)
  
}


### stub_spline.integer64() ###
#' @noRd
stub_spline.integer64 <- function(col, ctrl){

  ## Coerce to Double ##
  col <- as.double(col)

  ## Define Spline Function ##
  out <- stub_spline_(col = col, ctrl = ctrl)

  ## Output ##
  return(out)

}


### stub_spline.POSIXct() ###
#' @noRd
stub_spline.POSIXct <- function(col, ctrl){
  
  ## Coerce to Integer ##
  col <- as.integer(col)
  
  ## Define Spline Function ##
  out <- stub_spline_(col = col, ctrl = ctrl)
  
  ## Output ##
  return(out)
  
}


### stub_spline.POSIXlt() ###
#' @noRd
stub_spline.POSIXlt <- function(col, ctrl){
  
  ## Coerce to POSIXct ##
  col <- as.POSIXct(col)
  
  ## Use POSIXct Method ##
  out <- stub_spline.POSIXct(col = col, ctrl = ctrl)
  
  ## Output ##
  return(out)
  
}


### stub_spline_() ###
#' @noRd
stub_spline_ <- function(col, ctrl){
  
  ## Checks ##
  if (!is.numeric(ctrl[["tail_exc"]])) stop("The 'tail_exc' control parameter must be of class numeric.")
  if (ctrl[["tail_exc"]] < 0 | ctrl[["tail_exc"]] >= 0.5) stop("The 'tail_exc' control parameter must be between 0 and 0.5.")
  
  ## Tail Exclusions ##
  limits <- quantile(col, c(0 + ctrl[["tail_exc"]], 1 - ctrl[["tail_exc"]]), na.rm = TRUE, names = FALSE)
  col <- col[col >= limits[1] & col <= limits[2] & !is.na(col)]
  
  if (length(col) >= 2){
    
    ## Determine Optimal Number of Breaks ##
    nclass <- nclass.FD(col)
    breaks <- seq(min(col, na.rm = TRUE), max(col, na.rm = TRUE), length.out = nclass + 1L)
    
    ## Histogram Object ##
    h <- hist(x = col, breaks = breaks, plot = FALSE)
    
    ## Cumulative Sum of Densities
    width <- diff(h[["breaks"]])
    area <- width*h[["density"]]
    cdf <- c(0, cumsum(area))
    
    ## Generate a Spline Function ##
    f <- approxfun(x = cdf, y = breaks, rule = 1, ties = list("ordered", min))
    
  } else {
    
    f <- function(v){rep(NA_integer_, v)}
    
  }
  
  ## Standard Deviation ##
  sd <- sd(col, na.rm = TRUE)
  
  ## Form Output ##
  out <- list(
    fun = f,
    sd = sd
  )
  
  ## Output ##
  return(out)
  
}
