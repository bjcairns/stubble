#===================#
#                   #
#### ECDF SPLINE ####
#                   #
#===================#


### ToDo ###
# - Create a method for ordered factors.


### ecdf_spline() ###
ecdf_spline <- function(col, ...){
  
  UseMethod("ecdf_spline", col)
  
}


### ecdf_spline.default() ###
ecdf_spline.default <- function(col, ctrl){
  
  ## Warning ##
  warning("No method exists for vector of class: ", class(col)[1])
  
  ## Define Function ##
  f <- function(v){rep(NA_integer_, v)}
  
  ## Form Output ##
  out <- list(
    fun = f
  )
  
  ## Output ##
  return(out)
  
}


### ecdf_spline.Date() ###
ecdf_spline.Date <- function(col, ctrl){
  
  ## Coerce to Integer ##
  col <- as.integer(col)
  
  ## Define Spline Function ##
  f <- ecdf_spline_(col = col, ctrl = ctrl)
  
  ## Output ##
  return(f)
  
}


### ecdf_spline.double() ###
ecdf_spline.double <- function(col, ctrl){
  
  ## Define Spline Function ##
  f <- ecdf_spline_(col = col, ctrl = ctrl)
  
  ## Output ##
  return(f)
  
}


### ecdf_spline.integer() ###
ecdf_spline.integer <- function(col, ctrl){
  
  ## Define Spline Function ##
  f <- ecdf_spline_(col = col, ctrl = ctrl)
  
  ## Output ##
  return(f)
  
}


### ecdf_spline.integer64() ###
ecdf_spline.integer64 <- function(col, ctrl){

  ## Coerce to Double ##
  col <- as.double(col)

  ## Define Spline Function ##
  f <- ecdf_spline_(col = col, ctrl = ctrl)

  ## Output ##
  return(f)

}


# ### ecdf_spline.ordered() ###
# ecdf_spline.ordered <- function(col, ctrl){
#   
#   ## Extract Labels ##
#   labels <- levels(col)
#   
#   ## Magical VooDoo ##
#   # HERE!
#   
#   ## Coerce to Integer ##
#   col <- as.integer(col)
#   
#   ## Define Spline Function ##
#   f <- ecdf_spline_(col = col, ctrl = ctrl)
#   
#   ## Output ##
#   return(list(f = f, labels = labels))
#   
# }


### ecdf_spline.POSIXct() ###
ecdf_spline.POSIXct <- function(col, ctrl){
  
  ## Coerce to Integer ##
  col <- as.integer(col)
  
  ## Define Spline Function ##
  f <- ecdf_spline_(col = col, ctrl = ctrl)
  
  ## Output ##
  return(f)
  
}


### ecdf_spline.POSIXlt() ###
ecdf_spline.POSIXlt <- function(col, ctrl){
  
  ## Coerce to POSIXct ##
  col <- as.POSIXct(col)
  
  ## Use POSIXct Method ##
  f <- ecdf_spline.POSIXct(col = col, ctrl = ctrl)
  
  ## Output ##
  return(f)
  
}


### ecdf_spline_() ###
ecdf_spline_ <- function(col, ctrl){
  
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
  
  ## Form Output ##
  out <- list(
    fun = f
  )
  
  ## Output ##
  return(out)
  
}


# ### Testing ###
# ctrl <- list(
#   tail_exc = 0,
#   fuzz_ecdf = FALSE,
#   dttm_tz = "UTC"
# )
# 
# list_test0 <- list(
#   Date = as.Date(character(0)),
#   double = double(0),
#   IDate = data.table::as.IDate(character(0)),
#   integer = integer(0),
#   integer64 = bit64::integer64(0),
#   POSIXct = as.POSIXct(character(0), tz = ctrl[["dttm_tz"]]),
#   POSIXlt = as.POSIXlt(character(0), tz = ctrl[["dttm_tz"]])
# )
# 
# lapply(list_test0, ecdf_spline, ctrl = ctrl)
# 
# set.seed(10)
# 
# n <- 100
# list_test <- list(
#   Date = as.Date("1970-01-01") + rpois(n, 10),
#   double = rnorm(n),
#   iDate = data.table::as.IDate("1970-01-01") + rpois(n, 10),
#   integer = rpois(n, 10),
#   integer64 = bit64::as.integer64(rpois(n, 10)),
#   POSIXct = as.POSIXct("1970-01-01", tz = ctrl[["dttm_tz"]]) + runif(n, 0, 1e9),
#   POSIXlt = as.POSIXlt(as.POSIXct("1970-01-01", tz = ctrl[["dttm_tz"]]) + runif(n, 0, 1e9))
# ); rm(n)
# 
# str(list_test)
# 
# lapply(list_test, ecdf_spline, ctrl = ctrl)
