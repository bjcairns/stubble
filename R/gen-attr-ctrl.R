#=====================#
#                     #
#### GEN ATTR CTRL ####
#                     #
#=====================#


### gen_attr_ctrl() ###
gen_attr_ctrl <- function(
  p_na = NA_real_, emp_sw = 0.5,
  tail_exc = 0.025, fuzz_ecdf = TRUE,
  n_exc = 10, p_exc = 0.05, fuzz_samp = TRUE, drop_lev = TRUE,
  dttm_tz = "UTC",
  old_ctrl = as.list(NULL),
  index = NA_integer_,
  ...
){
  
  ## Checks ##
  if (tail_exc < 0 | tail_exc >= 0.5) stop("'tail_exc' must be a positive value < 0.5.")
  
  args <- as.list(sys.frame(sys.nframe()))
  args <- lapply(args, eval, parent.frame())
  args <- lapply(args, as.list)
  
  cargs <- as.list(match.call())[-1L]
  cargs <- lapply(cargs, eval, parent.frame())
  cargs <- lapply(cargs, as.list)
  
  args[["old_ctrl"]] <- NULL
  cargs[["old_ctrl"]] <- NULL
  
  all_args <- append(old_ctrl, args[!(names(args) %in% names(old_ctrl))])
  all_args <- append(cargs, all_args[!(names(all_args) %in% names(cargs))])
  
  # Return control parameters for a single column if required
  if(!is.na(index)){
    all_args <- lapply(all_args, get_ctrl_element, index = index)
  }
  
  return(invisible(all_args))
  
}


### get_ctrl_element() ###
get_ctrl_element <- function(item, index){
  item_base <- length(item)
  item_idx <- index %% item_base
  item_idx <- ifelse(item_idx == 0, item_base, item_idx)
  elem <- item[[item_idx]]
  return(elem)
}
