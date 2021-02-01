#' @title
#' Errors, warnings and messages
#' 
#' @description
#' Commonly used `stop()`, `warning()` and `message()` functions in stubble.


### ToDo ###
# - Try to reuse more of the internals to avoid duplicated code.
# - Evaluate the need to have .warning_zero_sample() here.


### .warning_coercion() ###
#' @noRd
.warning_coercion <- function(x, ...){
  
  ## Define S3 Method ##
  UseMethod(".warning_coercion", x)
  
}


### .warning_coercion.data.table() ###
#' @export
.warning_coercion.data.table <- function(x){
  
  ## Message ##
  msg <- paste(
    "Package 'data.table' not found.",
    "'data.table' will be coerced to 'data.frame'",
    sep = "\n"
  )
  
  ## Output ##
  return(warning(msg, call. = FALSE))
  
}


### .warning_coercion.IDate() ###
#' @export
.warning_coercion.IDate <- function(x){
  
  ## Message ##
  msg <- paste(
    "Package 'data.table' not found.",
    "'IDate' will be coerced to 'Date'",
    sep = "\n"
  )
  
  ## Output ##
  return(warning(msg, call. = FALSE))
  
}


### .warning_coercion.integer64() ###
#' @export
.warning_coercion.integer64 <- function(x){
  
  ## Message ##
  msg <- paste(
    "Package 'bit64' not found.",
    "'integer64' will be coerced to 'double'",
    sep = "\n"
  )
  
  ## Output ##
  return(warning(msg, call. = FALSE))
  
}


### .warning_coercion.ITime() ###
#' @export
.warning_coercion.ITime <- function(x){
  
  ## Message ##
  msg <- paste(
    "Package 'data.table' not found.",
    "'ITime' will be coerced to 'POSIXct'",
    sep = "\n"
  )
  
  ## Output ##
  return(warning(msg, call. = FALSE))
  
}


### .warning_coercion.tbl_df() ###
#' @export
.warning_coercion.tbl_df <- function(x){
  
  ## Message ##
  msg <- paste(
    "Package 'tibble' not found.",
    "'tbl_df' will be coerced to 'data.frame'",
    sep = "\n"
  )
  
  ## Output ##
  return(warning(msg, call. = FALSE))
  
}


### .warning_no_method() ###
#' @noRd
.warning_no_method <- function(x){
  
  ## First Class ##
  cl <- class(x)[1]
  
  ## Generate Warning ##
  warn <- paste("No method exists for object of class:", cl)
  
  ## Output ##
  return(warning(warn, call. = FALSE))
  
}


### .stop_no_method() ###
#' @noRd
.stop_no_method <- function(x){
  
  ## First Class ##
  cl <- class(x)[1]
  
  ## Generate Warning ##
  err <- paste("No method exists for object of class:", cl)
  
  ## Output ##
  return(stop(err, call. = FALSE))
  
}
