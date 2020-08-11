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


# ### Testing ###
# old_ctrl <- list(
#   dttm_tz = "UTC",
#   n_exc = 0,
#   p_exc = 0,
#   fuzz_samp = FALSE,
#   p_na = 0.1
# )
# ctrl <- gen_stubble_ctrl(old_ctrl = old_ctrl, index = 1L)
# 
# moo <- stub(iris, ctrl = ctrl)
# x <- moo[["Species"]]
