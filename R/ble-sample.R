#' @title
#' Generate synthetic data using resampling methods
#' 
#' @description
#' Internal function for generating synthetic data using resampling methods.
#' 
#' @concept empirical
#' @concept ecdf
#' @concept resample
#' @concept simulate
#' @concept simulated
#' @concept simulation
#' 
#' @keywords datagen


### ble_sample() ###
#' @noRd
ble_sample <- function(x, elements, ctrl){
  
  ## Extract Values ##
  val <- x[["sim"]][["values"]]
  wt <- x[["sim"]][["wt"]]
  
  ## Re-Sample ##
  syn_col <- sample(x = val, size = elements, replace = TRUE, prob = wt)
  
  ## Output ##
  return(syn_col)
  
}
