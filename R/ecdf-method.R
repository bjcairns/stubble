#===================#
#                   #
#### ECDF METHOD ####
#                   #
#===================#


### Notes ###
# - ecdf_method_.ordered() may need to change if ecdf_method_.ordered() is finalised.


### ecdf_method() ###
ecdf_method <- function(col, ...){
  
  ## Define S3 Method ##
  UseMethod("ecdf_method", col)
  
}


### ecdf_method.default() ###
ecdf_method.default <- function(col, ctrl){
  
  ## Warning ##
  warning("Using 'sample' method for unrecognized class: ", class(col)[1])
  
  method <- "sample"
  
  ## Output ##
  return(method)
  
}


# ### ecdf_method_.bit() ###
# ecdf_method.bit <- function(col, ctrl){
#   
#   ## Use logical Method ##
#   method <- ecdf_method.logical(col = col, ctrl = ctrl)
#   
#   ## Output ##
#   return(method)
#   
# }


### ecdf_method.character() ###
ecdf_method.character <- function(col, ctrl){
  
  ## Sample Method ##
  method <- "sample"
  
  ## Output ##
  return(method)
  
}


### ecdf_method.Date() ###
ecdf_method.Date <- function(col, ctrl){
  
  ## Use numeric Method ##
  method <- ecdf_method.numeric(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### ecdf_method.factor() ###
ecdf_method.factor <- function(col, ctrl){
  
  ## Use character Method ##
  method <- ecdf_method.character(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### ecdf_method.integer64() ###
ecdf_method.integer64 <- function(col, ctrl){
  
  ## Use numeric Method ##
  method <- ecdf_method.numeric(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### ecdf_method.logical() ###
ecdf_method.logical <- function(col, ctrl){
  
  ## Use character Method ##
  method <- ecdf_method.character(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### ecdf_method.numeric() ###
ecdf_method.numeric <- function(col, ctrl){
  
  ## Determine Method ##
  method <- ecdf_method_(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### ecdf_method.POSIXct() ###
ecdf_method.POSIXct <- function(col, ctrl){
  
  ## Use numeric Method ##
  method <- ecdf_method.numeric(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### ecdf_method.POSIXlt() ###
ecdf_method.POSIXlt <- function(col, ctrl){
  
  ## Coerce to POSIXct ##
  col <- as.POSIXct(col)
  
  ## Use POSIXct Method ##
  method <- ecdf_method.POSIXct(col = col, ctrl = ctrl)
  
  ## Output ##
  return(method)
  
}


### ecdf_method_() ###
ecdf_method_ <- function(col, ctrl){
  
  ## Checks ##
  if (!is.numeric(ctrl[["emp_sw"]])) stop("The 'emp_sw' control parameter must be of class numeric.")
  if (ctrl[["emp_sw"]] < 0 | ctrl[["emp_sw"]] > 1) stop("The 'emp_sw' control parameter must be between 0 and 1.")
  
  ## Complete Cases Only ##
  cc_col <- col[!is.na(col)]
  
  ## Uniqueness ##
  p_uniq <- length(unique(cc_col))/length(cc_col)
  
  ## Method Selection ##
  method <- if(p_uniq > ctrl[["emp_sw"]]) "spline" else "sample"
  
  ## Output ##
  return(method)
  
}


# ### Testing ###
# ctrl <- list(
#   dttm_tz = "UTC",
#   emp_sw = 0.5
# )
# 
# n <- 10
# test_list <- list(
#   bit = bit::as.bit(c(F, T)),
#   character = letters[1:n],
#   Date = as.Date("1970-01-01") + 1:n,
#   double = as.double(1:n),
#   factor = gl(n, 1, labels = letters[1:n]),
#   IDate = data.table::as.IDate("1970-01-01") + 1:n,
#   integer = 1:n,
#   integer64 = bit64::as.integer64(1:n),
#   logical = c(F, T),
#   ordered = gl(n, 1, labels = letters[1:n], ordered = T),
#   POSIXct = as.POSIXct("1970-01-01", tz = ctrl[["dttm_tz"]]) + 1:n,
#   POSIXlt = as.POSIXlt(as.POSIXct("1970-01-01", tz = ctrl[["dttm_tz"]]) + 1:n)
# )
# 
# sapply(test_list, ecdf_method, ctrl = ctrl)
