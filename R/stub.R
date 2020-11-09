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
#' @param method the method to use in the generation of synthetic data. Must be
#' one of `"agnostic"` (default) or `"empirical"`.
#' @param ctrl a named list of control parameters for generating the synthetic
#' data. See [`stubble_ctrl`].
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
#' [stubble_ctrl]
#'
#' @examples
#' stub(penguins_ext)
#' stub(penguins_ext, rows = 10)
#'
#' @concept empirical
#' @concept ecdf
#' @concept sample
#' @concept simulate
#' @concept simulated
#' @concept simulation
#'
#' @keywords datagen


### ToDo ###
# - Add methods for working with Time Series objects.
# - Add methods for working with groupedData objects. These have the data.frame
#   class set, but as the last element of the class(x) vector.
# - Parallelize stub__() on Unix.
# - Preserve rownames from data.frames


### stub() ###
#' @export
stub <- function(x, rows = lengths(x), method = "agnostic", ..., ctrl = list()){
  
  ## Fuzzy Match 'method' Argument ##
  method <- fuzzy_match(x = c("agnostic", "empirical"), prefix = method)
  
  ## Control Params ##
  ctrl <- stubble_ctrl(..., old_ctrl = ctrl)
  
  ## Use S3 Method ##
  vars <- stub_(x = x, rows = rows, method = method, ctrl = ctrl)
  
  ## Missing Values ##
  p_na <- lapply(X = vars, FUN = `[[`, "p_na")
  names(p_na) <- NULL
  ctrl[["p_na"]] <- p_na
  vars <- lapply(X = vars, FUN = function(x){x[names(x) != "p_na"]})
  
  ## Data Structure ##
  dtype <- dtype0(x = x)
  
  ## Form Output ##
  out <- list(
    dtype = dtype,
    vars = vars,
    ctrl = ctrl
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
#' @export
stub_.default <- function(x, rows, method, ctrl){
  
  ## Attempt List Coercion ##
  l <- tryCatch(expr = as.list(x),
                error = function(e) stop("Cannot coerce argument 'x' to list."),
                warning = function(w) warning(w))
  
  ## Use stub_.list Method ##
  vars <- stub_.list(x, rows = rows, method = method, ctrl = ctrl)
  
  ## Output ##
  return(vars)
  
}


### stub_.data.frame() ###
#' @export
stub_.data.frame <- function(x, rows, method, ctrl){
  
  ## Variable Structure ##
  vars <- stub_.list(x = x, rows = rows, method = method, ctrl = ctrl)
  
  ## Output ##
  return(vars)
  
}


### stub_.data.table() ###
#' @export
stub_.data.table <- function(x, rows = rows, method, ctrl){
  
  ## Variable Structure ##
  vars <- stub_.list(x = x, rows = rows, method = method, ctrl = ctrl)
  
  ## Output ##
  return(vars)
  
}


### stub_.list() ###
#' @export
stub_.list <- function(x, rows = rows, method, ctrl){
  
  ## Variable Structure ##
  vars <- stub__(x = x, rows = rows, method = method, ctrl = ctrl)
  
  ## Output ##
  return(vars)
  
}


### stub_.tbl_df() ###
#' @export
stub_.tbl_df <- function(x, rows = rows, method, ctrl){
  
  ## Variable Structure ##
  vars <- stub_.list(x = x, rows = rows, method = method, ctrl = ctrl)
  
  ## Output ##
  return(vars)
  
}


### stub__() ###
#' @noRd
stub__ <- function(x, rows, method, ctrl){
  
  ## Create Index ##
  index <- seq_along(x)
  
  ## Apply stub_attr() ##
  vars <- mapply(
    FUN = stub_attr,
    col = x,
    elements = rows,
    method = method,
    index = index,
    MoreArgs = list(ctrl = ctrl),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )
  
  ## Output ##
  return(vars)
  
}
