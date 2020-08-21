#' @title
#' Control parameters for stubble
#' 
#' @description
#' This help file describes the available parameters to control the output of
#' [stubble()]. The function is accessible to users but often will not need to
#' be called directly.
#' 
#' @param p_na Proportion of values set to `NA`; defaults to `NA`, meaning that
#' the proportions present in any generated data will roughly match those of the
#' source data.
#' @param emp_sw Value determining whether spline or resampling methods are used
#' in the the generation of synthetic data. When the unique fraction of a column
#' is above this value spline-based methods will be used. Conversely, when it is
#' below this value resampling methods will be used. Hence, setting it to 1 will
#' ensure that resampling methods will always be used, while setting it to 0
#' will ensure that spline-based methods will always be used. Defaults to 50%
#' (`0.5`). will always be used. When set to 0 ECDF-based methods will always be
#' used.
#' @param tail_exc Quantile tail size to be omitted from sampling at each end
#' of the empirical cumulative distribution function. Defaults to 2.5% (`0.025`)
#' at each end (tail) of the distribution.
#' @param fuzz_spl Should the values sampled from the ECDF by [`ble_spline`]
#' be 'fuzzed' through the addition of random normal noise? Defaults to `TRUE`.
#' @param fuzz_spl_sca The scaling factor for the standard deviation of the
#' random noise applied when `fuzz_spl` is set to `TRUE`. Defaults to `0.05`,
#' i.e. 5% of the standard deviation of the source data.
#' @param n_exc Observation prevalence below which values will be excluded from
#' simulations. Defaults to 10.
#' @param p_exc Observation prevalence below which values will be excluded from
#' simulations. Defaults to 1% (`0.01`)
#' @param fuzz_samp Should the probability weights sampled from the distribution
#' of values by [`stub_sample`] be 'fuzzed' through the addition of random
#' normal noise? Defaults to `TRUE`.
#' @param drop_lev Parameter indicating whether empty factor levels should be
#' dropped from simlated factors and ordered factors. Defauls to `TRUE`
#' @param dttm_tz Timezone for generated date-times. Defaults to `"UTC"`, but
#' [Sys.timezone()] may be more appropriate for some users.
#' @param old_ctrl A set of control parameters to inherit unless explicitly
#' overwritten in the current call.
#' @param index Default `NA`. If not `NA`, the function will return list in
#' which elements apply to a single column (i.e. elements are not necessarily
#' lists). Mostly for internal use to handle passing control parameters between
#' `emperor_` S3 methods.
#' @param ... Further control parameters permitting extension of the `stubble`
#' S3 methods.
#' 
#' @seealso
#' [stubble()]


### stubble_ctrl() ###
#' @export
stubble_ctrl <- function(
  p_na = NA_real_, emp_sw = 0.1,
  tail_exc = 0.025, fuzz_spl = TRUE, fuzz_spl_sca = 0.05,
  n_exc = 10, p_exc = 0.05, fuzz_samp = TRUE, drop_lev = TRUE,
  dttm_tz = "UTC",
  old_ctrl = list(),
  index = NA_integer_,
  ...
){
  
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
#' @noRd
get_ctrl_element <- function(item, index){
  item_base <- length(item)
  item_idx <- index %% item_base
  item_idx <- ifelse(item_idx == 0, item_base, item_idx)
  elem <- item[[item_idx]]
  return(elem)
}
