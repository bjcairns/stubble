#===========#
#           #
#### BLE ####
#           #
#===========#


### ble() ###
ble <- function(stub, rows, ctrl = list(), ...){
  
  ### Data Structure ###
  dtype <- stub[["dtype"]]
  
  ### Vars ###
  vars <- stub[["vars"]]
  
  ## Create Index ##
  index <- seq_along(vars)
  
  ### Rows ###
  if (missing(rows)) rows <- sapply(vars, `[[`, "n")
  
  ## Apply gen_ble() ##
  l <- mapply(
    FUN = gen_ble,
    x = vars,
    elements = rows,
    index = index,
    MoreArgs = list(ctrl = ctrl, ...),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )
  
  ## Coerce to Correct Output Type ##
  out <- ble_(dtype = dtype, l = l)
  
  ## Output ##
  return(out)
  
}


### ble_() ###
ble_ <- function(dtype, ...){
  
  ## Define S3 Method ##
  UseMethod("ble_", dtype)
  
}


### ble_.default() ###
ble_.default <- function(dtype, l){
  
  ## Error ##
  .stop_no_method(dtype)
  
}


### ble_.data.frame() ###
ble_.data.frame <- function(dtype, l){
  
  ## Coerce to data.frame ##
  out <- as.data.frame(l)
  
  ## Output ##
  return(out)
  
}


### ble_.data.table() ###
ble_.data.table <- function(dtype, l){
  
  ## Coerce to data.table ##
  out <- if ("data.table" %in% rownames(installed.packages())){
    
    data.table::as.data.table(l)
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
    as.data.frame(l)
    
  }
  
  ## Output ##
  return(out)
  
}


### ble_.tbl_df() ###
ble_.tbl_df <- function(dtype, l){
  
  ## Coerce to tbl_df ##
  out <- if ("tibble" %in% rownames(installed.packages())){
    
    tibble::as.tibble(l)
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
    as.data.frame(l)
    
  }
  
  ## Output ##
  return(out)
  
}


# ### Testing ###
# ctrl <- list(p_na = 0.5)
# 
# moo <- stub(iris)
# 
# ble(moo, elements = 10)
