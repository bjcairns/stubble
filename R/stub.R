#' @title
#' Generate a stub object from which synthetic data may be generated
#' 
#' @description
#' `stub()` is an internal function of [`stubble`] that is used to generate a
#' list-type object, which in turn, may be used by the [`ble`] internal function
#' to generate synthetic data. Calling `stub()` directly allows the user to
#' generate an object containing all the necessary information to generate a
#' synthetic version of the source data, without actually doing so. This may be
#' useful when sharing synthetic versions of your source data with others.
#' 
#' 
#' @param x the vector from which the type of the synthetic data is taken.
#' @param rows the number of elements to generate.
#' @param ctrl a named list of control parameters for generating the synthetic
#' data. See [ctrl].
#' @param ... named individual control parameters, which take precedence over
#' those in the `ctrl` list.
#'
#' @details
#' `stub()` calls an internal S3 generic function, `stub_()`, with built-in
#' methods for the `list` and `data.frame` base R data types, in addition to
#' `data.table` and `tbl_df` data types, where the required _data.table_ and
#' _tibble_ packages are installed.
#' 
#' @return
#' A list containing three elements:
#' * `ctrl` Any non-default control parameters set.
#' * `dtype` A zero-dimension object of the same class as the source data.
#' * `vars` Attributes relating to each element/column of the source data.
#' 
#' @note
#' Custom row names from objects with the class attribute `data.frame` are not
#' currently captured by `stub()` and will not be reproduced in any data
#' generated from the resulting `stub` object.
#' 
#' @seealso 
#' [stubble_control]
#' 
#' @examples
#' stub(iris)
#' stub(iris, rows = 10)
#' 
#' @concept empirical
#' @concept ecdf
#' @concept resample
#' @concept simulate
#' @concept simulated
#' @concept simulation
#' 
#' @keywords datagen


### ToDo ###
# - Add methods for working with Time Series objects.
# - Add methods for working with groupedData objects. These have the data.frame
#   class set, but as the last element of the class(x) vector.
# - Parallelize to_stub() on Unix.
# - Preserve rownames from data.frames


### stub() ###
#' @export
stub <- function(x, ctrl = list(), ...){
  
  ## Data Structure ##
  dtype <- dtype0(x = x)
  
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
#' @noRd
stub_ <- function(x, ...){
  
  ## Define S3 Method ##
  UseMethod("stub_", x)
  
}


### stub_.default() ###
#' @noRd
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
#' @noRd
stub_.data.frame <- function(x, rows = nrow(x), ctrl = list(), ...){
  
  ## Variable Structure ##
  vars <- stub_.list(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Output ##
  return(vars)
  
}


### stub_.data.table() ###
#' @noRd
stub_.data.table <- function(x, rows = nrow(x), ctrl = list(), ...){
  
  ## Variable Structure ##
  vars <- stub_.list(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Output ##
  return(vars)
  
}


### stub_.list() ###
#' @noRd
stub_.list <- function(x, rows = lengths(x), ctrl = list(), ...){
  
  ## Variable Structure ##
  vars <- to_stub(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Output ##
  return(vars)
  
}


### stub_.tbl_df() ###
#' @noRd
stub_.tbl_df <- function(x, rows = nrow(x), ctrl = list(), ...){
  
  ## Variable Structure ##
  vars <- stub_.list(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Output ##
  return(vars)
  
}


### to_stub() ###
#' @noRd
to_stub <- function(x, rows, ctrl, ...){
  
  ## Create Index ##
  index <- seq_along(x)
  
  ## Apply stub_attr() ##
  vars <- mapply(
    FUN = stub_attr,
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
