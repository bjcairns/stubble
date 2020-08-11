#==================#
#                  #
#### GEN SPLINE ####
#                  #
#==================#


### ToDo ###
# - Implement fuzz().


### gen_spline() ###
gen_spline <- function(x, elements, ctrl){
  
  ## Extract stub Params ##
  cl <- x[["class"]]
  f <- x[["sim"]][["fun"]]
  
  ## Generate Data ##
  v <- runif(n = elements, min = 0 + ctrl[["tail_exc"]], max = 1 - ctrl[["tail_exc"]])
  syn_col <- f(v = v)
  
  ## Coerce Output ##
  syn_col <- switch(cl,
                    Date = as.Date(syn_col, tz = ctrl[["dttm_tz"]]),
                    double = as.double(syn_col),
                    IDate = {
                      if("data.table" %in% rownames(installed.packages())){
                        data.table::as.IDate(syn_col, tz = ctrl[["dttm_tz"]])
                      } else {
                        warning("Package 'data.table' not found. IDate will be converted to Date.")
                        as.Date(syn_col, tz = ctrl[["dttm_tz"]])
                      }
                    },
                    integer = as.integer(round(syn_col)),
                    POSIXct = as.POSIXct(syn_col, tz = ctrl[["dttm_tz"]]),
                    POSIXlt = as.POSIXlt(as.POSIXct(syn_col, tz = ctrl[["dttm_tz"]])))
  
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
