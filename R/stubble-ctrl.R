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
#' @param dttm_tz Timezone for generated date-times (`POSIXt` class variables).
#' Defaults to `"UTC"`, but [Sys.timezone()] may be more appropriate for some
#' users.
#' @param agn_unique Single logical value or logical vector indicating whether
#' synthetic values should be unique within the column. When a vector, the
#' relevant element is chosen by the `index` argument to `ble_agnostic`.
#' @param agn_int_min Minmium values for integer generation.
#' @param agn_int_max Maximum values for integer generation.
#' @param agn_int_list An integer vector of allowed values for integer
#' generation. If `NA` (the default), this is ignored. If non-`NA`,
#' @param agn_int64_min Minimum values for integer64 generation (requires
#' _bit64_ package).
#' @param agn_int64_max Maximum values for integer64 generation (requires
#' _bit64_ package).
#' `agn_int_list` overrides `agn_int_min`/`agn_int_max`.
#' @param agn_dbl_min Minimum values for real/numeric/double generation.
#' @param agn_dbl_max Maximum values for real/numeric/double generation.
#' @param agn_dbl_round Number of decimal places to round to. [round()] and then
#' [signif()] are applied in sequence (see `agn_dbl_signif`, below). If `NA`
#' (default), no rounding is applied.
#' @param agn_dbl_signif Number of significant digits to round to. [round()] and
#' then [signif()] are applied in sequence (see `agn_dbl_round`). If `NA`
#' (default), no rounding is applied.
#' @param agn_chr_min The minimum number of characters in a generated string.
#' @param agn_chr_max The maximum number of characters in a generated string.
#' @param agn_chr_sym A character vector of allowed symbols for generated
#' strings.
#' @param agn_chr_sep A separator for symbols in generated strings; defaults to
#' `""` (an empty string).
#' @param agn_chr_try_unique Logical value indicating whether, after a failure
#' to generate a unique synthetic character vector, the algorithm should attempt
#' to regenerate duplicates. If `TRUE`, there will be
#' `agn_chr_try_unique_attempts` attempts.
#' @param agn_chr_try_unique_attempts Number of attempts to make to generate a
#' unique synthetic character vector.
#' @param agn_chr_duplicated_nmax Value (greater than one) for the `nmax`
#' parameter of [duplicated()] when enforcing uniqueness. Defaults to `NA`, the
#' default for `duplicated()`.
#' @param agn_fct_lvls Levels attribute for synthetic factors. Should always be
#' a list with character vectors as elements.
#' @param agn_fct_use_lvls Allowed levels for synthetic factor data. Should
#' always be a list with character vectors as elements. Each element of
#' `agn_fct_use_lvls` should be a subset of the corresponding element of
#' `agn_fct_lvls`.
#' @param agn_fct_force_unique Force `gen_col()` to attempt to return a factor
#' vector with unique elements. Will return an error unless
#' `length(agn_fct_use_lvls)` is greater than or equal to the `elements`
#' argument of `gen_col()` (equivalently, the `nrows` argument of
#' `stubblise()`).
#' @param agn_lgl_force_unique Force `gen_col()` to attempt to return a logical
#' vector with unique elements. Will return an error unless
#' `length(agn_lgl_lvls)` (usually equal to 2) is greater than or equal to the
#' `elements` argument of `gen_col()` (equivalently, the `nrows` argument of
#' `stubblise()`).
#' @param agn_date_origin The reference date for generated dates and times (`Date`,
#' `IDate` and `POSIXt` class variables).
#' @param agn_date_min Minimum value for generated dates (`Date` and `IDate`
#' class variables).
#' @param agn_date_max Maximum value for generated dates (`Date` and `IDate`
#' class variables).
#' @param agn_dttm_min Minimum value for generated date-times (`POSIXt` class
#' variables).
#' @param agn_dttm_max Maximum value for generated date-times (`POSIXt` class
#' variables).
#' @param agn_time_min Minimum value for generated times (`ITime` class
#' variables).
#' @param agn_time_max Maximum value for generated times (`ITime` class
#' variables).
#' @param emp_sw Value determining whether spline or sampling methods are used
#' in the the generation of synthetic data. When the unique fraction of a column
#' is above this value spline-based methods will be used. Conversely, when it is
#' below this value sampling methods will be used. Hence, setting it to `1` will
#' ensure that sampling methods will always be used, while setting it to `0`
#' will ensure that spline-based methods will always be used. Defaults to 10%
#' (`0.1`).
#' @param emp_tail_exc Quantile tail size to be omitted from sampling at each
#' end of the empirical cumulative distribution function. Defaults to 2.5%
#' (`0.025`).
#' @param emp_fuzz_spl Scaling factor for the standard deviation of the
#' random noise applied to continuous variables prior to sampling the empirical
#' cumulative distribution function by `stub_spline`. Defaults to `0.05`, i.e.
#' 5% of the standard deviation of the source data. Setting this value to `0`
#' switches off obfuscation of the values evaluated by `stub_spline`.
#' @param emp_n_exc Observation prevalence below which values will be excluded
#' by `stub_sample`. Defaults to `10`.
#' @param emp_p_exc Observation prevalence below which values will be excluded
#' by `stub_sample`. Defaults to 5% (`0.05`).
#' @param emp_fuzz_samp Scaling factor for the obfuscation of the sample
#' proportions measured by `stub_sample`. Defaults to `0.05`. Larger values add
#' more noise. Setting this value to `0` switches off obfuscation of the sample
#' proportions recorded by `stub_sample`.
#' @param emp_drop_lev Parameter indicating whether empty factor levels should
#' be dropped from simulated factors and ordered factors. Defaults to `TRUE`.
#' @param old_ctrl A set of control parameters to inherit unless explicitly
#' overwritten in the current call.
#' @param assert_class Logical value indicating whether control parameter
#' classes should be checked. Mostly intended for internal use. Defaults to
#' `TRUE`.
#' @param index Default `NA`. If not `NA`, the function will return list in
#' which elements apply to a single column, i.e. elements are not necessarily
#' lists. Mostly for internal use to handle passing control parameters between
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
  p_na = NA_real_,
  dttm_tz = "UTC",
  agn_unique = FALSE,
  agn_int_min = 0L, agn_int_max = 100L, agn_int_list = NA_integer_,
  agn_int64_min = 0L, agn_int64_max = 100L,
  agn_dbl_min = 0, agn_dbl_max = 100, agn_dbl_round = NA_integer_, agn_dbl_signif = NA_integer_,
  agn_chr_min = 0L, agn_chr_max = 10L,
  agn_chr_sym = list(c(
    letters, LETTERS, 0:9,
    strsplit("!\"#$%&'()*+, -./:;<=>?@[]^_`{|}~", "")[[1]]
  )),
  agn_chr_sep = "", agn_chr_try_unique = FALSE, agn_chr_try_unique_attempts = 10L, agn_chr_duplicated_nmax = NA_integer_,
  agn_fct_lvls = list(letters[1:4]), agn_fct_use_lvls = list(), agn_fct_force_unique = FALSE,
  agn_lgl_force_unique = FALSE,
  agn_date_origin = "1970-01-01",
  agn_date_min = agn_date_origin, agn_date_max = Sys.Date(),
  agn_dttm_min = agn_date_origin, agn_dttm_max = format(Sys.time(), tz = dttm_tz),
  agn_time_min = "00:00:00", agn_time_max = "23:59:59",
  emp_sw = 0.1,
  emp_tail_exc = 0.025, emp_fuzz_spl = 0.05,
  emp_n_exc = 10L, emp_p_exc = 0.05, emp_fuzz_samp = 0.05, emp_drop_lev = TRUE,
  old_ctrl = list(),
  assert_class = TRUE,
  index = NA_integer_,
  ...
){
  
  args <- as.list(sys.frame(sys.nframe()))
  args <- lapply(args, eval, parent.frame())
  args <- lapply(args, as.list)
  
  cargs <- as.list(match.call())[-1L]
  cargs <- lapply(cargs, eval, parent.frame())
  cargs <- lapply(cargs, as.list)
  
  args[c("old_ctrl", "assert_class")] <- NULL
  cargs[c("old_ctrl", "assert_class")] <- NULL
  
  all_args <- append(old_ctrl, args[!(names(args) %in% names(old_ctrl))])
  all_args <- append(cargs, all_args[!(names(all_args) %in% names(cargs))])
  
  ## Assert Correct Input Classes ##
  if (assert_class) all_args <- assert_ctrl_class(args = all_args, ctrl_classes = CTRL_CLASS)
  
  ## Return control parameters for a single column if required ##
  if (!is.na(index)) {
    all_args <- lapply(all_args, get_ctrl_element, index = index)
  }
  
  ## Output Class ##
  class(all_args) <- "stubbleCtrl"
  
  ## Output ##
  return(all_args)
  
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


### assert_ctrl_class() ###
#' @noRd
assert_ctrl_class <- function(
  args, ctrl_classes
){
  
  ## Extract Params ##
  dttm_tz = unlist(args[["dttm_tz"]])
  agn_date_origin = unlist(args[["agn_date_origin"]])
  
  ## base ##
  args[ctrl_classes[["character"]]] <- assert_ctrl_class_character(args = args[ctrl_classes[["character"]]])
  args[ctrl_classes[["Date"]]] <- assert_ctrl_class_Date(args = args[ctrl_classes[["Date"]]])
  args[ctrl_classes[["double"]]] <- assert_ctrl_class_double(args = args[ctrl_classes[["double"]]])
  args[ctrl_classes[["integer"]]] <- assert_ctrl_class_integer(args = args[ctrl_classes[["integer"]]])
  args[ctrl_classes[["logical"]]] <- assert_ctrl_class_logical(args = args[ctrl_classes[["logical"]]])
  args[ctrl_classes[["POSIXct"]]] <- assert_ctrl_class_POSIXct(args = args[ctrl_classes[["POSIXct"]]], dttm_tz = dttm_tz)
  
  ## Optional Deps ##
  # bit64#
  args[ctrl_classes[["integer64"]]] <- if (getOption("stubble_has_bit64")) {
    assert_ctrl_class_integer64(args = args[ctrl_classes[["integer64"]]])
  } else{
    assert_ctrl_class_double(args = args[ctrl_classes[["integer64"]]])
  }
  # data.table #
  if (getOption("stubble_has_data.table")) {
    args[ctrl_classes[["ITime"]]] <- assert_ctrl_class_ITime(args = args[ctrl_classes[["ITime"]]])
  } else {
    args[ctrl_classes[["ITime"]]] <- format_ctrl_class_POSIXct(args = args[ctrl_classes[["ITime"]]], agn_date_origin = agn_date_origin)
    args[ctrl_classes[["ITime"]]] <- assert_ctrl_class_POSIXct(args = args[ctrl_classes[["ITime"]]], dttm_tz = dttm_tz)
  }
  
  ## Output ##
  return(args)
  
}


### assert_ctrl_class_character() ###
#' @noRd
assert_ctrl_class_character <- function(args){
  
  lapply(
    X = args,
    FUN = assert_ctrl_class_character_
  )
  
}


### assert_ctrl_class_character_() ###
#' @noRd
assert_ctrl_class_character_ <- function(arg){
  
  lapply(
    X = arg,
    FUN = as.character
  )
  
}


### assert_ctrl_class_Date() ###
#' @noRd
assert_ctrl_class_Date <- function(args){
  
  lapply(
    X = args,
    FUN = assert_ctrl_class_Date_
  )
  
}


### assert_ctrl_class_Date_() ###
#' @noRd
assert_ctrl_class_Date_ <- function(arg){
  
  lapply(
    X = arg,
    FUN = as.Date
  )
  
}


### assert_ctrl_class_double() ###
#' @noRd
assert_ctrl_class_double <- function(args){
  
  lapply(
    X = args,
    FUN = assert_ctrl_class_double_
  )
  
}


### assert_ctrl_class_double_() ###
#' @noRd
assert_ctrl_class_double_ <- function(arg){
  
  lapply(
    X = arg,
    FUN = as.double
  )
  
}


### assert_ctrl_class_integer() ###
#' @noRd
assert_ctrl_class_integer <- function(args){
  
  lapply(
    X = args,
    FUN = assert_ctrl_class_integer_
  )
  
}


### assert_ctrl_class_integer_() ###
#' @noRd
assert_ctrl_class_integer_ <- function(arg){
  
  lapply(
    X = arg,
    FUN = as.integer
  )
  
}


### assert_ctrl_class_integer64() ###
#' @noRd
assert_ctrl_class_integer64 <- function(args){
  
  lapply(
    X = args,
    FUN = assert_ctrl_class_integer64_
  )
  
}


### assert_ctrl_class_integer64_() ###
#' @noRd
assert_ctrl_class_integer64_ <- function(arg){
  
  lapply(
    X = arg,
    FUN = bit64::as.integer64
  )
  
}


### assert_ctrl_class_ITime() ###
#' @noRd
assert_ctrl_class_ITime <- function(args, agn_date_origin = agn_date_origin, dttm_tz = dttm_tz){
  
  lapply(
    X = args,
    FUN = assert_ctrl_class_ITime_
  )
  
}


### assert_ctrl_class_ITime_() ###
#' @noRd
assert_ctrl_class_ITime_ <- function(arg){
  
  lapply(
    X = arg,
    FUN = data.table::as.ITime
  )
  
}


### assert_ctrl_class_logical() ###
#' @noRd
assert_ctrl_class_logical <- function(args){
  
  lapply(
    X = args,
    FUN = assert_ctrl_class_logical_
  )
  
}


### assert_ctrl_class_logical_() ###
#' @noRd
assert_ctrl_class_logical_ <- function(arg){
  
  lapply(
    X = arg,
    FUN = as.logical
  )
  
}


### assert_ctrl_class_POSIXct() ###
#' @noRd
assert_ctrl_class_POSIXct <- function(args, dttm_tz){
  
  lapply(
    X = args,
    FUN = assert_ctrl_class_POSIXct_,
    dttm_tz = dttm_tz
  )
  
}


### assert_ctrl_class_POSIXct_() ###
#' @noRd
assert_ctrl_class_POSIXct_ <- function(arg, dttm_tz){
  
  lapply(
    X = arg,
    FUN = as.POSIXct,
    tz = dttm_tz
  )
  
}


### format_ctrl_class_POSIXct() ###
#' @noRd
format_ctrl_class_POSIXct <- function(args, agn_date_origin){
  
  lapply(
    X = args,
    FUN = format_ctrl_class_POSIXct_,
    agn_date_origin = agn_date_origin
  )
  
}


### format_ctrl_class_POSIXct_() ###
#' @noRd
format_ctrl_class_POSIXct_ <- function(arg, agn_date_origin){
  
  lapply(
    X = arg,
    FUN = format_ctrl_class_POSIXct__,
    agn_date_origin = agn_date_origin
  )
  
}


### format_ctrl_class_POSIXct__() ###
format_ctrl_class_POSIXct__ <- function(arg, agn_date_origin){
  
  paste(agn_date_origin, arg)
  
}
