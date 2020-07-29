#============#
#            #
#### STUB ####
#            #
#============#


### ToDo ###
# - Assign 'stub' class to output.


### stub() ###
stub <- function(x, ...){
  
  ## Define S3 Method ##
  UseMethod("stub", x)
  
}


### stub.default() ###
stub.default <- function(x, ctrl = list(), ...){
  
  ## Attempt List Coercion ##
  l <- tryCatch(expr = as.list(x),
                error = function(e) stop("Cannot coerce argument 'x' to list."),
                warning = function(w) warning(w))
  
  ## Use stub_ Function ##
  l <- stub_(x, rows = lengths(x), ctrl = ctrl, ...)
  
  ## Output ##
  return(l)
  
}


### stub.list() ###
stub.list <- function(x, rows = lengths(x), ctrl = list(), ...){
  
  ## Use stub_ Function ##
  l <- stub_(x, rows = rows, ctrl = ctrl, ...)
  
  ## Output ##
  return(l)
  
}


### stub.data.frame() ###
stub.data.frame <- function(x, rows = lengths(x), ctrl = list(), ...){
  
  ## Use stub.list S3 Method ##
  l <- stub.list(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Output ##
  return(l)
  
}


### stub.data.table() ###
stub.data.table <- function(x, rows = lengths(x), ctrl = list(), ...){
  
  ## Use stub.list S3 Method ##
  l <- stub.list(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Output ##
  return(l)
  
}


### stub.tbl_df() ###
stub.tbl_df <- function(x, rows = lengths(x), ctrl = list(), ...){
  
  ## Use stub.list S3 Method ##
  l <- stub.list(x = x, rows = rows, ctrl = ctrl, ...)
  
  ## Output ##
  return(l)
  
}


## stub_() ##
stub_ <- function(x, rows, ctrl, ...){
  
  ## Create Index ##
  index = seq_along(x)
  
  ## Apply gen_attr() ##
  l <- mapply(
    FUN = gen_attr,
    col = x,
    elements = rows,
    index = index,
    MoreArgs = list(ctrl = ctrl, ...),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )
  
  ## Output ##
  return(l)
  
}

# ### Testing ###
# n <- 1e3
# x <- data.frame(name = sample(c("Boris", "Tony", "Teresa", "John")),
#                 bp = as.integer(round(rnorm(n, 120, 10))),
#                 age = sample(60:85, n, replace = T),
#                 code = sample(letters, n, replace = T),
#                 dob = as.Date("1970-01-01") + 1:n)
# 
# str(stub(x))
