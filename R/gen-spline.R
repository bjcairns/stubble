#==================#
#                  #
#### GEN SPLINE ####
#                  #
#==================#


### ToDo ###
# - Implement fuzz().
# - Evaluate need for setting the origin when coercing from POSIXct to POSIXlt.


### gen_spline() ###
gen_spline <- function(x, elements, ctrl){
  
  ## Extract stub Params ##
  dtype <- x[["dtype"]]
  f <- x[["sim"]][["fun"]]
  col_sd <- x[["sim"]][["sd"]]
  
  ## Generate Data ##
  v <- runif(n = elements, min = 0 + ctrl[["tail_exc"]], max = 1 - ctrl[["tail_exc"]])
  syn_col <- f(v = v)
  
  ## Coerce Output ##
  syn_col <- gen_spline_(dtype = dtype, syn_col = syn_col, ctrl = ctrl)
  
  ## Fuzz ##
  if (ctrl[["fuzz_ecdf"]]){
    
    syn_col <- fuzz(syn_col = syn_col, col_sd = col_sd, elements = elements, ctrl = ctrl)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### gen_spline_() ###
gen_spline_ <- function(dtype, ...){
  
  ## Define S3 Method ##
  UseMethod("gen_spline_", dtype)
  
}


### gen_spline_.default() ###
gen_spline_.default <- function(dtype, ...){
  
  ## Error ##
  .stop_no_method(dtype)
  
}


### gen_spline_.Date() ###
gen_spline_.Date <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Coerce to Date ##
  syn_col <- as.Date(syn_col, origin = "1970-01-01", tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}

### gen_spline_.double() ###
gen_spline_.double <- function(dtype, syn_col, ctrl){
  
  ## Coerce to double ##
  syn_col <- as.double(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### gen_spline_.IDate() ###
gen_spline_.IDate <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Attempt Coercion to IDate ##
  syn_col <- if (is.installed.package("data.table")) {
    
    data.table::as.IDate(syn_col, origin = "1970-01-01", tz = ctrl[["dttm_tz"]])
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
    # Coerce to Date #
    NextMethod(dtype)
    
  }
  
  ## Output ##
  return(syn_col)
  
}

### gen_spline_.integer() ###
gen_spline_.integer <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Coerce to integer ##
  syn_col <- as.integer(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### gen_spline_.integer64() ###
gen_spline_.integer64 <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Attempt Coercion to integer64 ##
  syn_col <- if (is.installed.package("bit64")) {
    
    bit64::as.integer64(syn_col)
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
    # Coerce to double #
    as.double(syn_col)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### gen_spline_.POSIXct() ###
gen_spline_.POSIXct <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Coerce to POSIXct ##
  syn_col <- as.POSIXct(syn_col, origin = "1970-01-01", tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}


### gen_spline_.POSIXlt() ###
gen_spline_.POSIXlt <- function(dtype, syn_col, ctrl){
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Coerce to POSIXct ##
  syn_col <- as.POSIXct(syn_col, origin = "1970-01-01", tz = ctrl[["dttm_tz"]])
  
  ## Coerce to POSIXlt ##
  syn_col <- as.POSIXlt(syn_col, tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}


# ### Testing ###
# old_ctrl <- list(
#   dttm_tz = "UTC",
#   tail_exc = 0.025,
#   fuzz_ecdf = TRUE,
#   p_na = 0.1
# )
# ctrl <- gen_stubble_ctrl(old_ctrl = old_ctrl, index = 1L)
# 
# moo <- stub(iris, ctrl = ctrl)
# x <- moo[[1]]
# y <- moo[1:4]
# 
# gen_spline(x, ctrl = ctrl)
# hist(gen_spline(x, elements = 1e6, ctrl = ctrl), main = "Histogram", xlab = "syn_col")
# 
# mapply(FUN = gen_spline, x = y, elements = 1:4, MoreArgs = list(ctrl = ctrl), SIMPLIFY = FALSE, USE.NAMES = TRUE)
