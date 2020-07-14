#' Control parameters for gen_col()
#'
#' This help file describes the available parameters to control the output of
#' [stubblise()]. The function which assembles these parameters into a list,
#' `gen_col_control()`, is accessible to users but often will not need to be
#' called directly.
#'
#' @param unique Single logical value or logical vector indicating whether
#' synthetic values should be unique within the column. When a vector, the
#' relevant element is chosen by the `index` argument to [gen_col].
#' @param p_na Proportion of values set to `NA`; defaults to `0`.
#' @param int_min Minmium values for integer generation.
#' @param int_max Maximum values for integer generation.
#' @param int_list An integer vector of allowed values for integer generation.
#' If `NA` (the default), this is ignored. If non-`NA`, `int_list`
#' overrides `int_min`/`int_max`.
#' @param dbl_min Minimum values for real/numeric/double generation
#' @param dbl_max Maximum values for real/numeric/double generation
#' @param dbl_rng_kind The random number generation algorithm to use for
#' real/numeric/double values. Takes values allowed by [RNGkind()]. Defaults to
#' `"Wichmann-Hill"`, which is slower than other generators but is much less
#' likely to produce duplicate values.
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
#' @param date_max Max value for generated dates.
#' @param dttm_max Max value for generated date-times.
#' @param dttm_tz Timezone for generated date-times. Defaults to `"UTC"`, but
#' [Sys.timezone()] may be more appropriate for some users.
#' @param def_class A default vector class to return when all else fails.
#' Currently not used.
#' @param old_ctrl A set of control parameters to inherit unless explicitly
#' overwritten in the current call.
#' @param index Default `NA`. If not `NA`, the function will return list in
#' which elements apply to a single column (i.e. elements are not necessarily
#' lists). Mostly for internal use to handle passing control parameters between
#' `gen_col_` S3 methods.
#' @param ... Further control parameters permitting extension of the `gen_col_`
#' S3 methods.
#'
#' @details Generation of synthetic data in stubble is very simple. Numbers,
#' dates and times are sampled uniformly within a range, while strings and
#' factors are constructed from lists of symbols or levels sampled with equal
#' probability.
#'
#' Various control parameters allow some user influence over the sets of
#' allowed values. Available parameters are listed and described above. In
#' addition to `gen_col_control()`, these parameters are exposed directly via
#' [stubblise()] and [gen_col()], and may be passed as arguments with the names
#' given above to either function.
#'
#' Control parameters may be common to all or may be specified per-column.
#' Arguments passed to `gen_col_control()` or directly via `stubblise()` or
#' `gen_col()` are interpreted according to type:
#' \itemize{
#'   \item *Single-element vectors* mean that the same parameter value applies
#'   to each column;
#'   \item *Multi-element vectors* mean that each element is matched (with
#'   recycling) as the parameter for the corresponding column;
#' }
#' To pass a vector parameter for a column, such as a vector of allowed factor
#' levels, wrap the vector in a list, e.g. `list(letters[1:4])`. (Internally,
#' all arguments passed to `gen_col_control()`, whether directly or indirectly,
#' are sanitised with `as.list()`.)

#' @export
gen_col_control <- function(
  unique = FALSE,
  p_na = 0,
  int_min = 0L, int_max = 100L, int_list = NA,
  dbl_min = 0, dbl_max = 100,
  dbl_rng_kind = "Wichmann-Hill",
  dbl_round = NA, dbl_signif = NA,
  chr_min = 0L, chr_max = 10L,
  chr_sym = list(c(
    letters, LETTERS, as.character(0:9),
    unlist(strsplit("!\"#$%&'()*+, -./:;<=>?@[]^_`{|}~", ""))
  )),
  chr_sep = "",
  chr_try_unique = FALSE,
  chr_try_unique_attempts = 10L,
  chr_duplicated_nmax = NA,
  fct_lvls = list(letters[1:4]),
  fct_use_lvls = NULL,
  fct_force_unique = FALSE,
  lgl_force_unique = FALSE,
  date_origin = "1970-01-01",
  date_max = Sys.Date(),
  dttm_max = Sys.time(),
  dttm_tz = "UTC",
  def_class = "numeric",
  old_ctrl = as.list(NULL),
  index = NA,
  ...
) {

  if (!is.list(old_ctrl)) stop("Argument `old_ctrl` must be a list")

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
  if (!is.na(index)) {
    all_args <- lapply(all_args, get_ctrl_element, index = index)
  }

  invisible(all_args)

}


#' @rdname gen_col_control
#' @name control
NULL


get_ctrl_element <- function(item, index) {
  item_base <- length(item)
  item_idx <- index %% item_base
  item_idx <- ifelse(item_idx == 0, item_base, item_idx)
  elem <- item[[item_idx]]
  return(elem)
}

