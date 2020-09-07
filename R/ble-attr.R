#' @title
#' Decode vector attributes
#' 
#' @description
#' Internal function to decode list attributes into a vector.
#' 
#' @concept empirical
#' @concept ecdf
#' @concept resample
#' @concept simulate
#' @concept simulated
#' @concept simulation
#' 
#' @keywords datagen


### ble_attr() ###
#' @noRd
ble_attr <- function(x, elements, index = 1L, ctrl = list(), ...){
  
  ## Set Control Parameters ##
  ctrl <- stubble_ctrl(..., old_ctrl = ctrl, index = index)
  
  ## Extract Params ##
  method <- x[["sim"]][["method"]]
  elements <- if (missing(elements)) x[["n"]] else elements
  
  ## Set RNG ##
  rng_kind <- ctrl[["rng_kind"]]
  old_kind <- RNGkind()[1]
  on.exit(RNGkind(kind = old_kind))
  RNGkind(kind = rng_kind)
  
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
