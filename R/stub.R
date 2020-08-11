#============#
#            #
#### STUB ####
#            #
#============#


### ToDo ###
# - Add methods for working with Time Series objects.
# - Add methods for working with groupedData objects. These have the data.frame
#   class set, but as the last element of the class(x) vector.
# - Parallelise to_stub() on Unix.
# - Preserve rownames from data.frames


### stub() ###
stub <- function(x, ctrl = list(), ...){
  
  ## Use S3 Method ##
  l <- stub_(x = x, ctrl = ctrl, ...)
  
  ## Assign Class ##
  class(l) <- "stub"
  
  ## Output ##
  return(l)
  
}


### stub_() ###
stub_ <- function(x, ...){
  
  ## Define S3 Method ##
  UseMethod("stub_", x)
  
}


### stub_.default() ###
stub_.default <- function(x, rows = lengths(x), ctrl = list(), ...){
  
  ## Attempt List Coercion ##
  l <- tryCatch(expr = as.list(x),
                error = function(e) stop("Cannot coerce argument 'x' to list."),
                warning = function(w) warning(w))
  
  ## Data Structure ##
  dtype <- list()
  
  ## Use stub_.list Method ##
  vars <- stub_.list(x, rows = lengths(x), ctrl = ctrl, ...)
  
  ## Form Output ##
  out <- list(
    dtype = dtype,
    vars = vars
  )
  
  ## Output ##
  return(out)
  
}


### stub_.data.frame() ###
stub_.data.frame <- function(x, rows = nrow(x), ctrl = list(), ...){
  
  ## Data Structure ##
  dtype <- data.frame()
  
  ## Variable Structure ##
  vars <- to_stub(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Form Output ##
  out <- list(
    dtype = dtype,
    vars = vars
  )
  
  ## Output ##
  return(out)
  
}


### stub_.data.table() ###
stub_.data.table <- function(x, rows = nrow(x), ctrl = list(), ...){
  
  ## Data Structure ##
  dtype <- if ("data.table" %in% rownames(installed.packages())){
    
    data.table::data.table()
    
  } else {
    
    warning("Package 'data.table' not found. 'data.frame' class will be set for output.")
    
    data.frame()
    
  }
  
  ## Variable Structure ##
  vars <- to_stub(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Form Output ##
  out <- list(
    dtype = dtype,
    vars = vars
  )
  
  ## Output ##
  return(out)
  
}


### stub_.list() ###
stub_.list <- function(x, rows = lengths(x), ctrl = list(), ...){
  
  ## Data Structure ##
  dtype <- list()
  
  ## Variable Structure ##
  vars <- to_stub(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Form Output ##
  out <- list(
    dtype = dtype,
    vars = vars
  )
  
  ## Output ##
  return(out)
  
}


### stub_.tbl_df() ###
stub_.tbl_df <- function(x, rows = nrow(x), ctrl = list(), ...){
  
  ## Data Structure ##
  dtype <- if ("tibble" %in% rownames(installed.packages())){
    
    tibble::tibble()
    
  } else {
    
    warning("Package 'tibble' not found. 'data.frame' class will be set for output.")
    
    data.frame()
    
  }
  
  ## Variable Structure ##
  vars <- to_stub(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Form Output ##
  out <- list(
    dtype = dtype,
    vars = vars
  )
  
  ## Output ##
  return(out)
  
}


### to_stub() ###
to_stub <- function(x, rows, ctrl, ...){
  
  ## Create Index ##
  index <- seq_along(x)
  
  ## Apply gen_attr() ##
  vars <- mapply(
    FUN = gen_attr,
    col = x,
    elements = rows,
    index = index,
    MoreArgs = list(ctrl = ctrl, ...),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )
  
  ## Output ##
  return(vars)
  
}
