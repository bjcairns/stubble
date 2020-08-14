#==================#
#                  #
#### DTYPE ZERO ####
#                  #
#==================#


### dtype0() ###
dtype0 <- function(x, ...){
  
  ## Define S3 Method ##
  UseMethod("dtype0", x)
  
}


### dtype0.default() ###
dtype0.default <- function(x){
  
  ## Error ##
  .stop_no_method(x)
  
}


# ### dtype0.bit() ###
# dtype0.bit <- function(x){
#   
#   ## Create Object ##
#   v <- if (is.installed.package("bit")) {
#     
#     # 0-Length Vector #
#     bit::bit(length = 0L)
#     
#   } else {
#     
#     # Warning #
#     .warning_coercion(x)
#     
#     # 0-Length Vector #
#     logical(length = 0L)
#     
#   }
#   
#   ## Output ##
#   return(v)
#   
# }


### dtype0.character() ###
dtype0.character <- function(x){
  
  ## 0-Length Vector ##
  v <- character(length = 0L)
  
  ## Output ##
  return(v)
  
}


### dtype0.data.frame() ###
dtype0.data.frame <- function(x){
  
  ## 0-Row data.frame ##
  d <- data.frame()
  
  ## Output ##
  return(d)
  
}


### dtype0.data.table() ###
dtype0.data.table <- function(x){
  
  ## Create Object ##
  d <- if (is.installed.package("data.table")) {
    
    # 0-Row data.table #
    data.table::data.table()
    
  } else {
    
    # Warning #
    .warning_coercion(x)
    
    # 0-Row data.frame #
    data.frame()
    
  }
  
  ## Output ##
  return(d)
  
}


### dtype0.Date() ###
dtype0.Date <- function(x){
  
  ## 0-Length Vector ##
  v <- as.Date(character(length = 0L), tz = "UTC")
  
  ## Output ##
  return(v)
  
}


### dtype0.double() ###
dtype0.double <- function(x){
  
  ## 0-Length Vector ##
  v <- double(length = 0L)
  
  ## Output ##
  return(v)
  
}


### dtype0.factor() ###
dtype0.factor <- function(x){
  
  ## 0-Length Vector ##
  v <- factor(character(length = 0L))
  
  ## Output ##
  return(v)
  
}


### dtype0.IDate() ###
dtype0.IDate <- function(x){
  
  ## 0-Length Vector ##
  v <- if (is.installed.package("data.table")) {
    
    data.table::as.IDate(character(length = 0L), tz = "UTC")
    
  } else {
    
    NextMethod(x)
    
  }
  
  ## Output ##
  return(v)
  
}


### dtype0.integer() ###
dtype0.integer <- function(x){
  
  ## 0-Length Vector ##
  v <- integer(length = 0L)
  
  ## Output ##
  return(v)
  
}


### dtype0.integer64() ###
dtype0.integer64 <- function(x){
  
  ## Create Object ##
  v <- if (is.installed.package("bit64")) {
    
    # 0-Length Vector #
    bit64::integer64(length = 0L)
    
  } else {
    
    # Warning #
    .warning_coercion(x)
    
    # 0-Length Vector #
    logical(length = 0L)
    
  }
  
  ## Output ##
  return(v)
  
}


### dtype0.list() ###
dtype0.list <- function(x){
  
  ## 0-Length List ##
  l <- list()
  
  ## Output ##
  return(l)
  
}


### dtype0.logical() ###
dtype0.logical <- function(x){
  
  ## 0-Length List ##
  v <- logical(length = 0)
  
  ## Output ##
  return(v)
  
}


### dtype0.ordered() ###
dtype0.ordered <- function(x){
  
  ## 0-Length Vector ##
  v <- ordered(character(length = 0L))
  
  ## Output ##
  return(v)
  
}


### dtype0.POSIXct() ###
dtype0.POSIXct <- function(x){
  
  ## 0-Length Vector ##
  v <- as.POSIXct(character(length = 0L), tz = "UTC")
  
  ## Output ##
  return(v)
  
}


### dtype0.POSIXlt() ###
dtype0.POSIXlt <- function(x){
  
  ## 0-Length Vector ##
  v <- as.POSIXlt(character(length = 0L), tz = "UTC")
  
  ## Output ##
  return(v)
  
}


### dtype0.tbl_df() ###
dtype0.tbl_df <- function(x){
  
  
  d <- if (is.installed.package("tibble")) {
    
    # 0-Length tibble #
    tibble::tibble()
    
  } else {
    
    # Warning #
    .warning_coercion(x)
    
    # 0-Length data.frame #
    data.frame()
    
  }
  
  ## Output ##
  return(d)
  
}
