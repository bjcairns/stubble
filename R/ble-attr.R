#================#
#                #
#### BLE ATTR ####
#                #
#================#


### ble_attr() ###
#' @noRd
ble_attr <- function(x, elements, index = 1L, ctrl = list(), ...){
  
  ## Set Control Parameters ##
  ctrl <- stubble_ctrl(..., old_ctrl = ctrl, index = index)
  
  ## Extract Params ##
  method <- x[["sim"]][["method"]]
  elements <- if (missing(elements)) x[["n"]] else elements
  
  ## Generate syn_col ##
  syn_col <- switch(method,
                    sample = ble_sample(x = x, elements = elements, ctrl = ctrl),
                    spline = ble_spline(x = x, elements = elements, ctrl = ctrl))
  
  ## Impute NA Values ##
  if (!is.na(ctrl[["p_na"]])) {
    
    syn_col <- impute_na(syn_col, p_na = ctrl[["p_na"]])
    
  }
  
  ## Output ##
  return(syn_col)
  
}
