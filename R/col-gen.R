#' Generate simple synthetic data from an R vector
#'
#' `col_gen()` does the real work for [stubblise()]. Currently supported column
#' classes are `numeric`, `integer`, `character`, `factor`, `logical`,
#' `POSIXct` and `Date`. There is limited support for `list` (returns a list
#' with all `NA`s).
#'
#' @param col the vector from which the class of the synthetic data is taken.
#' @param nrows the number of elements to generate.
#' @param ... control parameters for ranges and valid levels/characters in the
#' synthetic data. See [stubble::control].
#'
#' @return Returns a vector of the same class as `col` with `nrows` elements.
#'
#' @examples
#' \dontrun{
#' library(stubble)
#' }
#' col_gen(iris$Sepal.length)
#' col_gen(iris$Species)
#'
#' @export
col_gen <- function(col, nrows, ...) {

  tryCatch(
    syn_col <- col_gen_(col, nrows, ctrl = col_gen_control(...)),
    error = function(err) {
      warning("Could not generate data for ", class(col), "; returning NAs" )
    }
  )

  if (!exists("syn_col")) {
    syn_col <- col_gen_.default(col, nrows, ctrl = col_gen_control(...))
  }

  invisible(syn_col)

}


col_gen_ <- function(col, nrows, ctrl) {
  UseMethod("col_gen_", col)
}


col_gen_.default <- function(col, nrows, ctrl) {

  # Note, no user control over class of vector in this case
  as.numeric(rep(NA, nrows))

}


col_gen_.numeric <- function(col, nrows, ctrl) {

  stats::runif(
    nrows,
    ctrl$dbl_min[1],
    ctrl$dbl_max[1]
  )

}


col_gen_.integer <- function(col, nrows, ctrl) {

  ceiling(stats::runif(
    nrows,
    as.integer(ctrl$int_min[1]),
    as.integer(ctrl$int_max[1])
  ))

}


col_gen_.character <- function(col, nrows, ctrl) {

  char_lengths <- col_gen_.integer(
    col, nrows,
    col_gen_control(int_min = ctrl$chr_min, int_max = ctrl$chr_max)
  )
  sapply(
    sapply(char_lengths, resample, x = ctrl$chr_sym, replace = TRUE),
    paste0, collapse = "", simplify = TRUE
  )

}


col_gen_.factor <- function(col, nrows, ctrl) {

  as.factor(
    col_gen_.character(
      col, nrows,
      col_gen_control(
        chr_min = 1L, chr_max = 1L,
        chr_sym = as.character(ctrl$fct_lvls)
      )
    )
  )

}


col_gen_.logical <- function(col, nrows, ctrl) {

  as.logical(
    col_gen_.integer(
      col, nrows,
      col_gen_control(int_min = 0L, int_max = 1L)
    )
  )

}


col_gen_.POSIXct <- function(col, nrows, ctrl) {

  as.POSIXct(
    col_gen_.numeric(
      col, nrows,
      col_gen_control(dbl_min = 0, dbl_max = as.numeric(ctrl$dttm_max))
    ),
    tz = ctrl$dttm_tz,
    origin = ctrl$date_origin
  )

}


col_gen_.Date <- function(col, nrows, ctrl) {

  as.Date(
    col_gen_.integer(
      col, nrows,
      col_gen_control(dbl_min = 0L, dbl_max = as.integer(ctrl$date_max))
    ),
    tz = ctrl$dttm_tz,
    origin = ctrl$date_origin
  )

}


col_gen_.list <- function(col, nrows, ctrl) {

  as.list(rep(NA, nrows))

}


resample <- function(size, x, replace = FALSE, prob = NULL) {
  base::sample(x, size, replace, prob)
}

