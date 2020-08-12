#============#
#            #
#### STUB ####
#            #
#============#


### ToDo ###
# - Add methods for working with Time Series objects.
# - Add methods for working with groupedData objects. These have the data.frame
#   class set, but as the last element of the class(x) vector.
# - Parallelize to_stub() on Unix.
# - Preserve rownames from data.frames
# - Embed a copy of the ctrl parameters used in the stub object.


### stub() ###
stub <- function(x, ctrl = list(), ...){
  
  ## Data Structure ##
  dtype <- dtype0(x)
  
  ## Use S3 Method ##
  vars <- stub_(x = x, ctrl = ctrl, ...)
  
  ## Form Output ##
  out <- list(
    ctrl = ctrl,
    dtype = dtype,
    vars = vars
  )
  
  ## Assign Class ##
  class(out) <- "stub"
  
  ## Output ##
  return(out)
  
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
  
  ## Use stub_.list Method ##
  vars <- stub_.list(x, rows = lengths(x), ctrl = ctrl, ...)
  
  ## Output ##
  return(vars)
  
}


### stub_.data.frame() ###
stub_.data.frame <- function(x, rows = nrow(x), ctrl = list(), ...){
  
  ## Variable Structure ##
  vars <- stub_.list(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Output ##
  return(vars)
  
}


### stub_.data.table() ###
stub_.data.table <- function(x, rows = nrow(x), ctrl = list(), ...){
  
  ## Variable Structure ##
  vars <- stub_.list(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Output ##
  return(vars)
  
}


### stub_.list() ###
stub_.list <- function(x, rows = lengths(x), ctrl = list(), ...){
  
  ## Variable Structure ##
  vars <- to_stub(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Output ##
  return(vars)
  
}


### stub_.tbl_df() ###
stub_.tbl_df <- function(x, rows = nrow(x), ctrl = list(), ...){
  
  ## Variable Structure ##
  vars <- stub_.list(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Output ##
  return(vars)
  
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
