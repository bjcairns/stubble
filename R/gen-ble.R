#===============#
#               #
#### GEN BLE ####
#               #
#===============#


### gen_ble() ###
gen_ble <- function(x, elements, index = 1L, ctrl = list(), ...){
  
  ## Set Control Parameters ##
  ctrl <- gen_stubble_ctrl(..., old_ctrl = ctrl, index = index)
  
  ## Extract Params ##
  method <- x[["sim"]][["method"]]
  elements <- if (missing(elements)) x[["n"]] else elements
  
  ## Generate syn_col ##
  syn_col <- switch(method,
                    sample = gen_sample(x = x, elements = elements, ctrl = ctrl),
                    spline = gen_spline(x = x, elements = elements, ctrl = ctrl))
  
  ## Impute NA Values ##
  if (!is.na(ctrl[["p_na"]])) {
    
    syn_col <- impute_na(syn_col, p_na = ctrl[["p_na"]])
    
  }
  
  ## Output ##
  return(syn_col)
  
}


# ### Testing ###
# ctrl <- list(p_na = 0.5)
# moo <- stub(iris)
# gen_ble(moo[[1]])
# 
# mapply(FUN = gen_ble,
#        x = moo,
#        MoreArgs = list(ctrl = ctrl),
#        SIMPLIFY = FALSE,
#        USE.NAMES = TRUE)
