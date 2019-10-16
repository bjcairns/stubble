#' Generate simple synthetic data from an R vector
#'
#' `gen_col()` does the real work for [stubblise()]. Currently supported column
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
#' gen_col(iris$Sepal.length)
#' gen_col(iris$Species)
#'
#' @export
gen_col <- function(col, nrows, ...) {

  tryCatch(
    syn_col <- gen_col_(col, nrows, ctrl = gen_col_control(...)),
    error = function(err) {
      warning("Could not generate data for ", class(col), "; returning NAs" )
    }
  )

  if (!exists("syn_col")) {
    syn_col <- gen_col_.default(col, nrows, ctrl = gen_col_control(...))
  }

  invisible(syn_col)

}


gen_col_ <- function(col, nrows, ctrl) {
  UseMethod("gen_col_", col)
}


gen_col_.default <- function(col, nrows, ctrl) {

  # Note, no user control over class of vector in this case
  as.numeric(rep(NA, nrows))

}


gen_col_.numeric <- function(col, nrows, ctrl) {

  stats::runif(
    nrows,
    ctrl$dbl_min[1],
    ctrl$dbl_max[1]
  )

}


gen_col_.integer <- function(col, nrows, ctrl) {

  as.integer(
    ceiling(stats::runif(
      nrows,
      as.integer(ctrl$int_min[1]),
      as.integer(ctrl$int_max[1])
    ))
  )

}


gen_col_.character <- function(col, nrows, ctrl) {

  char_lengths <- gen_col_.integer(
    col, nrows,
    gen_col_control(int_min = ctrl$chr_min, int_max = ctrl$chr_max)
  )
  sapply(
    sapply(char_lengths, resample, x = ctrl$chr_sym, replace = TRUE),
    paste0, collapse = "", simplify = TRUE
  )

}


gen_col_.factor <- function(col, nrows, ctrl) {

  as.factor(
    gen_col_.character(
      col, nrows,
      gen_col_control(
        chr_min = 1L, chr_max = 1L,
        chr_sym = as.character(ctrl$fct_lvls)
      )
    )
  )

}


gen_col_.logical <- function(col, nrows, ctrl) {

  as.logical(
    gen_col_.integer(
      col, nrows,
      gen_col_control(int_min = 0L, int_max = 1L)
    )
  )

}


gen_col_.POSIXct <- function(col, nrows, ctrl) {

  as.POSIXct(
    gen_col_.numeric(
      col, nrows,
      gen_col_control(dbl_min = 0, dbl_max = as.numeric(ctrl$dttm_max))
    ),
    tz = ctrl$dttm_tz,
    origin = ctrl$date_origin
  )

}


gen_col_.Date <- function(col, nrows, ctrl) {

  as.Date(
    gen_col_.integer(
      col, nrows,
      gen_col_control(dbl_min = 0L, dbl_max = as.integer(ctrl$date_max))
    ),
    tz = ctrl$dttm_tz,
    origin = ctrl$date_origin
  )

}


gen_col_.list <- function(col, nrows, ctrl) {

  as.list(rep(NA, nrows))

}


resample <- function(size, x, replace = FALSE, prob = NULL) {
  base::sample(x, size, replace, prob)
}

