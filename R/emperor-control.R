#' @title
#' Control parameters for emperor()
#'
#' @description
#' This help file describes the available parameters to control the output of
#' [stubblise()]. The function which assembles these parameters into a list,
#' `emperor_control()`, is accessible to users but often will not need to be
#' called directly.
#'
#' @param p_na Proportion of values set to `NA`; defaults to `NA`.
#' @param tail_exc Quantile tail size to be omitted from sampling at each end
#' of the empirical cumulative distribution function. Defaults to 5% (`0.05`)
#' at each end (tail) of the distribution.
#' @param fuzz_ecdf Should the values sampled from the ECDF be 'fuzzed'
#' through the addition of random normal noise? Defaults to `TRUE`.
#' @param fuzz_sca Scaling factor for the random noise added to the ECDF data.
#' Defaults to `0.01`, i.e. 100th the range of the original data.
#' of the original data.
#' @param fuzz_ht Should all fuzzed parameters be kept within the bounds set
#' by `tail_exc`? Defaults to `TRUE`.
#' @param cat_exc Categorical observation prevalence below which values will
#' not be excluded from simulations. Defaults to 5% (`0.05`)
#' @param drop_lev Parameter indicating whether empty factor levels should be
#' dropped from simlated factors and ordered factors. Defauls to `TRUE`
#' @param dbl_round Number of decimal places to round to. [round()] and then
#' [signif()] are applied in sequence (see `dbl_signif`, below). If `NA`
#' (default), no rounding is applied.
#' @param dbl_signif Number of significant digits to round to. [round()] and
#' then [signif()] are applied in sequence (see `dbl_round`). If `NA`
#' (default), no rounding is applied.
#' @param dttm_tz Timezone for generated date-times. Defaults to `"UTC"`, but
#' [Sys.timezone()] may be more appropriate for some users.
#' @param old_ctrl A set of control parameters to inherit unless explicitly
#' overwritten in the current call.
#' @param index Default `NA`. If not `NA`, the function will return list in
#' which elements apply to a single column (i.e. elements are not necessarily
#' lists). Mostly for internal use to handle passing control parameters between
#' `emperor_` S3 methods.
#' @param ... Further control parameters permitting extension of the `emperor_`
#' S3 methods.
#'
#' @details
#' Generation of synthetic data in stubble is very simple. Numbers,
#' dates and times are sampled uniformly within a range, while strings and
#' factors are constructed from lists of symbols or levels sampled with equal
#' probability.
#'
#' Various control parameters allow some user influence over the sets of
#' allowed values. Available parameters are listed and described above. In
#' addition to `emperor_control()`, these parameters are exposed directly via
#' [stubblise()] and [emperor()], and may be passed as arguments with the names
#' given above to either function.
#'
#' Control parameters may be common to all or may be specified per-column.
#' Arguments passed to `emperor_control()` or directly via `stubblise()` or
#' `emperor()` are interpreted according to type:
#' \itemize{
#'   \item *Single-element vectors* mean that the same parameter value applies
#'   to each column;
#'   \item *Multi-element vectors* mean that each element is matched (with
#'   recycling) as the parameter for the corresponding column;
#' }
#' To pass a vector parameter for a column, such as a vector of allowed factor
#' levels, wrap the vector in a list, e.g. `list(letters[1:4])`. (Internally,
#' all arguments passed to `emperor_control()`, whether directly or indirectly,
#' are sanitised with `as.list()`.)
#' 
#' @seealso
#' [emperor()]

#' @export
emperor_control <- function(
  p_na = NA_real_,
  tail_exc = 0.025, fuzz_ecdf = TRUE, fuzz_sca = 0.01, fuzz_ht = TRUE,
  cat_exc = 0.05, drop_lev = TRUE,
  dbl_round = NA_integer_, dbl_signif = NA_integer_,
  dttm_tz = "UTC",
  old_ctrl = as.list(NULL),
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


#' @rdname emperor_control
#' @name control
NULL


get_ctrl_element <- function(item, index){
  item_base <- length(item)
  item_idx <- index %% item_base
  item_idx <- ifelse(item_idx == 0, item_base, item_idx)
  elem <- item[[item_idx]]
  return(elem)
}
