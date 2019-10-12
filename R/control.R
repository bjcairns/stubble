#' Control parameters for gen_col()
#'
#' This help file describes the available parameters to control the output of
#' [stubblise()].
#'
#' @details Generation of synthetic data in stubble is very simple. Numbers, dates and
#' times are sampled uniformly within a range, while strings and factors are
#' constructed from lists of symbols or levels sampled with equal probability.
#'
#' Various control parameters allow some user influence over the sets of
#' allowed values. The parameters are:
#'
#' \describe{
#'   \item{\code{int_min, int_max}}{Minmium and maximum values for integer generation.}
#'   \item{\code{dbl_min, dbl_max}}{Min/max values for real/numeric/double generation}
#'   \item{\code{chr_min, chr_max}}{The min/max number of characters in a generated string.}
#'   \item{\code{chr_sym}}{A character vector of allowed symbols for generated strings.}
#'   \item{\code{fct_lvls}}{Allowed levels for generated factors.}
#'   \item{\code{lgl_vals}}{Allowed values of generated logicals.}
#'   \item{\code{date_origin}}{The reference data for generated dates and times.}
#'   \item{\code{date_max}}{Max value for generated dates.}
#'   \item{\code{dttm_max}}{Max value for generated date-times.}
#'   \item{\code{dttm_tz}}{Timezone for generated date-times.}
#'   \item{\code{def_class}}{A default vector class to return when all else fails. Currently not used.}
#'   \item{\code{...}}{Further control parameters permitting extension of the `gen_col_` S3 class.}
#' }
#'
#' These parameters are exposed directly via [stubblise()] and [gen_col()], and
#' may be passed as arguments with the names given above to either function.
#'
#' @name control
NULL

# Do not export
gen_col_control <- function(
  int_min = 0L, int_max = 100L,
  dbl_min = 0, dbl_max = 100,
  chr_min = 0L, chr_max = 10L,
  chr_sym = c(
    letters, LETTERS, as.character(0:9),
    unlist(strsplit("!\"#$%&'()*+, -./:;<=>?@[]^_`{|}~", ""))
  ),
  fct_lvls = letters[1:4],
  lgl_vals = c(TRUE, FALSE),
  date_origin = "1970-01-01",
  date_max = Sys.Date(),
  dttm_max = Sys.time(),
  dttm_tz = "UTC",
  def_class = "numeric",
  ...
) {

  args <- as.list(sys.frame(sys.nframe()))
  cargs <- as.list(match.call())[-1L]

  all_args <- c(args, cargs[!(names(cargs) %in% names(args))])

  # Sort so the defaults are last
  all_args <- all_args[
    c(
      names(cargs)[!(names(cargs) %in% names(args))],
      names(args)
    )
    ]

  invisible(all_args)

}
