#================#
#                #
#### MESSAGES ####
#                #
#================#


### ToDo ###
# - Try to reuse more of the internals to avoid duplicated code.


### .warning_coercion() ###
.warning_coercion <- function(x, ...){
  
  ## Define S3 Method ##
  UseMethod(".warning_coercion", x)
  
}


### .warning_coercion.bit() ###
.warning_coercion.bit <- function(x){
  
  ## Message ##
  msg <- paste(
    "Package 'bit' not found.",
    "'bit' will be coerced to 'logical'",
    sep = "\n"
  )
  
  ## Output ##
  return(warning(msg, call. = FALSE))
  
}


### .warning_coercion.data.table() ###
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


### .warning_coercion.tbl_df() ###
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
.warning_no_method <- function(x){
  
  ## First Class ##
  cl <- class(x)[1]
  
  ## Generate Warning ##
  warn <- paste("No method exists for object of class:", cl)
  
  ## Output ##
  return(warning(warn, call. = FALSE))
  
}


### .stop_no_method() ###
.stop_no_method <- function(x){
  
  ## First Class ##
  cl <- class(x)[1]
  
  ## Generate Warning ##
  err <- paste("No method exists for object of class:", cl)
  
  ## Output ##
  return(stop(err, call. = FALSE))
  
}
