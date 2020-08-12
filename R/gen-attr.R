#================#
#                #
#### GEN ATTR ####
#                #
#================#


### gen_attr() ###
gen_attr <- function(col, elements = length(col), index = 1L, ctrl = list(), ...){
  
  ## Set Control Parameters ##
  ctrl <- gen_stubble_ctrl(..., old_ctrl = ctrl, index = index)
  
  ## Class ##
  dtype <- dtype0(col)
  
  ## Method ##
  method <- ecdf_method(col = col, ctrl = ctrl)
  
  ### Generate sim ##
  sim <- switch(method,
                spline = ecdf_spline(col = col, ctrl = ctrl),
                sample = ecdf_sample(col = col, ctrl = ctrl))
  sim <- append(list(method = method), sim)
  
  ## Missing Values ##
  p_na <- if (is.na(ctrl[["p_na"]])) sum(is.na(col))/length(col) else ctrl[["p_na"]]
  
  ## Form Output ##
  out <- list(
    dtype = dtype,
    n = elements,
    p_na = p_na,
    sim = sim
  )
  
  ## Output ##
  return(out)
  
}


# ### Testing ###
# ctrl <- list(
#   dttm_tz = "UTC",
#   emp_sw = 0.5,
#   tail_exc = 0.025,
#   fuzz_ecdf = FALSE,
#   n_exc = 10,
#   p_exc = 0.05,
#   fuzz_samp = FALSE,
#   p_na = NA_real_
# )
# 
# n <- 100L
# list_test <- list(
#   vec_int = rpois(n, 10),
#   vec_doub = rnorm(n, 100, 10),
#   vec_int64 = bit64::as.integer64(rpois(n, 1e3)),
#   vec_fac = factor(sample(1:10, n, replace = T)),
#   vec_ord = ordered(sample(1:10, n, replace = T)),
#   vec_Date = as.Date("1970-01-01") + rpois(n, 1e3),
#   vec_IDate = data.table::as.IDate("1970-01-01") + + rpois(n, 1e3),
#   vec_POSIXct = as.POSIXct("1970-01-01") + runif(n, 0, 1e9),
#   vec_POSIXlt = as.POSIXlt(as.POSIXct("1970-01-01") + runif(n, 0, 1e9))
# ); rm(n)
# 
# lapply(list_test, gen_attr, ctrl = ctrl)
