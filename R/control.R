#' Control parameters for gen_col()
#'
#' This help file describes the available parameters to control the output of
#' [stubblise()].
#'
#' @details Generation of synthetic data in stubble is very simple. Numbers,
#' dates and times are sampled uniformly within a range, while strings and
#' factors are constructed from lists of symbols or levels sampled with equal
#' probability.
#'
#' Various control parameters allow some user influence over the sets of
#' allowed values. The parameters are:
#'
#' \describe{
#'   \item{\code{unique}}{Single logical value or logical vector indicating whether synthetic values should be unique within the column. When a vector, the relevant element is chosen by the `index` argument to [gen_col].}
#'   \item{\code{int_min, int_max}}{Minmium and maximum values for integer generation.}
#'   \item{\code{dbl_min, dbl_max}}{Min/max values for real/numeric/double generation}
#'   \item{\code{dbl_rng_kind}}{The random number generation algorithm to use for real/numeric/double values. Takes values allowed by [RNGkind()]. Defaults to `"Wichmann-Hill"`, which is slower than other generators but is much less likely to produce duplicate values.}
#'   \item{\code{dbl_round, dbl_signif}}{Number of decimal places and number of significant digits to round to. These are applied in sequence with [round()] and then [signif()], respectively. If `NA` (default) the corresponding rounding is not applied.}
#'   \item{\code{chr_min, chr_max}}{The min/max number of characters in a generated string.}
#'   \item{\code{chr_sym}}{A character vector of allowed symbols for generated strings.}
#'   \item{\code{fct_lvls}}{Levels attribute for synthetic factors. Should always be a list with character vectors as elements.}
#'   \item{\code{fct_use_lvls}}{Allowed levels for synthetic factor data. Should always be a list with character vectors as elements. Each element of `fct_use_lvls` should be a subset of the corresponding element of `fct_lvls`.}
#'   \item{\code{lgl_vals}}{Allowed values of generated logicals.}
#'   \item{\code{date_origin}}{The reference data for generated dates and times.}
#'   \item{\code{date_max}}{Max value for generated dates.}
#'   \item{\code{dttm_max}}{Max value for generated date-times.}
#'   \item{\code{dttm_tz}}{Timezone for generated date-times.}
#'   \item{\code{def_class}}{A default vector class to return when all else fails. Currently not used.}
#'   \item{\code{...}}{Further control parameters permitting extension of the `gen_col_` S3 methods.}
#' }
#'
#' These parameters are exposed directly via [stubblise()] and [gen_col()], and
#' may be passed as arguments with the names given above to either function.
#'
#' @name control
NULL

#' @rdname control
#' @export
gen_col_control <- function(
  unique = FALSE,
  int_min = 0L, int_max = 100L,
  dbl_min = 0, dbl_max = 100,
  dbl_rng_kind = "Wichmann-Hill",
  dbl_round = NA, dbl_signif = NA,
  chr_min = 0L, chr_max = 10L,
  chr_sym = c(
    letters, LETTERS, as.character(0:9),
    unlist(strsplit("!\"#$%&'()*+, -./:;<=>?@[]^_`{|}~", ""))
  ),
  fct_lvls = list(letters[1:4]),
  fct_use_lvls = list(NULL),
  lgl_vals = c(TRUE, FALSE),
  date_origin = "1970-01-01",
  date_max = Sys.Date(),
  dttm_max = Sys.time(),
  dttm_tz = "UTC",
  def_class = "numeric",
  old_ctrl = as.list(NULL),
  ...
) {

  if (!is.list(old_ctrl)) stop("Argument `old_ctrl` must be a list")

  args <- as.list(sys.frame(sys.nframe()))
  args <- lapply(args, eval, parent.frame())

  cargs <- as.list(match.call())[-1L]
  cargs <- lapply(cargs, eval, parent.frame())

  args[["old_ctrl"]] <- NULL
  cargs[["old_ctrl"]] <- NULL

  all_args <- append(old_ctrl, args[!(names(args) %in% names(old_ctrl))])
  all_args <- append(cargs, all_args[!(names(all_args) %in% names(cargs))])

  invisible(all_args)

}
