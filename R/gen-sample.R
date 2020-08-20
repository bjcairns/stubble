#==================#
#                  #
#### GEN SAMPLE ####
#                  #
#==================#


### gen_sample() ###
gen_sample <- function(x, elements, ctrl){
  
  ## Extract Values ##
  val <- x[["sim"]][["values"]]
  wt <- x[["sim"]][["wt"]]
  
  ## Re-Sample ##
  syn_col <- sample(x = val, size = elements, replace = TRUE, prob = wt)
  
  ## Output ##
  return(syn_col)
  
}
