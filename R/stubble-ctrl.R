#' @title
#' Control parameters for stubble
#' 
#' @description
#' This help file describes the available parameters to control the output of
#' [stubble()]. The function is accessible to users but often will not need to
#' be called directly.
#' 
#' @param rng_kind The random number generation algorithm to use. Takes values
#' allowed by [RNGkind()]. Defaults to `"Wichmann-Hill"`, which is slower than
#' other generators but is much less likely to produce duplicate values.
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
#' @param fuzz_spl Should the values sampled from the ECDF by `ble_spline`
#' be 'fuzzed' through the addition of random normal noise? Defaults to `TRUE`.
#' @param fuzz_spl_sca The scaling factor for the standard deviation of the
#' random noise applied when `fuzz_spl` is set to `TRUE`. Defaults to `0.05`,
#' i.e. 5% of the standard deviation of the source data.
#' @param n_exc Observation prevalence below which values will be excluded from
#' simulations. Defaults to 10.
#' @param p_exc Observation prevalence below which values will be excluded from
#' simulations. Defaults to 1% (`0.01`)
#' @param fuzz_samp Should the probability weights sampled from the distribution
#' of values by `stub_sample` be 'fuzzed' through the addition of random normal
#' noise? Defaults to `TRUE`.
#' @param drop_lev Parameter indicating whether empty factor levels should be
#' dropped from simlated factors and ordered factors. Defauls to `TRUE`
#' @param unique Single logical value or logical vector indicating whether
#' synthetic values should be unique within the column. When a vector, the
#' relevant element is chosen by the `index` argument to `ble_agnostic`.
#' @param int_min Minmium values for integer generation.
#' @param int_max Maximum values for integer generation.
#' @param int_list An integer vector of allowed values for integer generation.
#' If `NA` (the default), this is ignored. If non-`NA`, `int_list`
#' overrides `int_min`/`int_max`.
#' @param dbl_min Minimum values for real/numeric/double generation
#' @param dbl_max Maximum values for real/numeric/double generation
#' @param dbl_round Number of decimal places to round to. [round()] and then
#' [signif()] are applied in sequence (see `dbl_signif`, below). If `NA`
#' (default), no rounding is applied.
#' @param dbl_signif Number of significant digits to round to. [round()] and
#' then [signif()] are applied in sequence (see `dbl_round`). If `NA`
#' (default), no rounding is applied.
#' @param chr_min The minimum number of characters in a generated string.
#' @param chr_max The maximum number of characters in a generated string.
#' @param chr_sym A character vector of allowed symbols for generated strings.
#' @param chr_sep A separator for symbols in generated strings; defaults to
#' `""` (an empty string).
#' @param chr_try_unique Logical value indicating whether, after a failure to
#' generate a unique synthetic character vector, the algorithm should attempt
#' to regenerate duplicates. If `TRUE`, there will be `chr_try_unique_attempts`
#' attempts.
#' @param chr_try_unique_attempts Number of attempts to make to generate a
#' unique synthetic character vector.
#' @param chr_duplicated_nmax Value (greater than one) for the `nmax` parameter
#' of [duplicated()] when enforcing uniqueness. Defaults to `NA`, the default
#' for `duplicated()`.
#' @param fct_lvls Levels attribute for synthetic factors. Should always be a
#' list with character vectors as elements.
#' @param fct_use_lvls Allowed levels for synthetic factor data. Should always
#' be a list with character vectors as elements. Each element of `fct_use_lvls`
#' should be a subset of the corresponding element of `fct_lvls`.
#' @param fct_force_unique Force `gen_col()` to attempt to return a factor
#' vector with unique elements. Will return an error unless
#' `length(fct_use_lvls)` is greater than or equal to the `elements` argument
#' of `gen_col()` (equivalently, the `nrows` argument of `stubblise()`).
#' @param lgl_force_unique Force `gen_col()` to attempt to return a logical
#' vector with unique elements. Will return an error unless `length(lgl_lvls)`
#' (usually equal to 2) is greater than or equal to the `elements` argument
#' of `gen_col()` (equivalently, the `nrows` argument of `stubblise()`).
#' @param date_origin The reference date for generated dates and times.
#' @param date_min Min value for generated dates.
#' @param date_max Max value for generated dates.
#' @param dttm_min Min value for generated dates.
#' @param dttm_max Max value for generated date-times.
#' @param dttm_tz Timezone for generated date-times. Defaults to `"UTC"`, but
#' [Sys.timezone()] may be more appropriate for some users.
#' @param old_ctrl A set of control parameters to inherit unless explicitly
#' overwritten in the current call.
#' @param index Default `NA`. If not `NA`, the function will return list in
#' which elements apply to a single column (i.e. elements are not necessarily
#' lists). Mostly for internal use to handle passing control parameters between
#' S3 methods.
#' @param ... Further control parameters permitting extension of the `stubble`
#' S3 methods.
#' 
#' @seealso
#' [stubble()]


### stubble_ctrl() ###
#' @export
stubble_ctrl <- function(
  rng_kind = "Wichmann-Hill",
  p_na = NA_real_, emp_sw = 0.1,
  tail_exc = 0.025, fuzz_spl = TRUE, fuzz_spl_sca = 0.05,
  n_exc = 10, p_exc = 0.05, fuzz_samp = TRUE, drop_lev = TRUE,
  unique = FALSE,
  int_min = 0L, int_max = 100L, int_list = NA,
  dbl_min = 0, dbl_max = 100, dbl_round = NA, dbl_signif = NA,
  chr_min = 0L, chr_max = 10L,
  chr_sym = list(c(
    letters, LETTERS, 0:9,
    strsplit("!\"#$%&'()*+, -./:;<=>?@[]^_`{|}~", "")[[1]]
  )),
  chr_sep = "", chr_try_unique = FALSE, chr_try_unique_attempts = 10L, chr_duplicated_nmax = NA,
  fct_lvls = list(letters[1:4]), fct_use_lvls = NULL, fct_force_unique = FALSE,
  lgl_force_unique = FALSE,
  date_origin = "1970-01-01",
  date_min = date_origin, date_max = Sys.Date(),
  dttm_min = date_origin, dttm_max = Sys.time(),
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
