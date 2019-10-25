#' Generate simple synthetic data from an R vector
#'
#' `gen_col()` does the real work for [stubblise()]. Currently supported column
#' classes are `numeric`, `integer`, `character`, `factor`, `logical`,
#' `POSIXct` and `Date`. There is limited support for `list` (returns a list
#' with all `NA`s).
#'
#' @param col the vector from which the type of the synthetic data is taken.
#' @param elements the number of elements to generate.
#' @param index the index of the column for the purposes of extracting control
#' parameters.
#' @param ... control parameters for ranges and valid levels/characters in the
#' synthetic data. See [control].
#'
#' @return Returns a vector of the same class as `col` with `elements` elements.
#'
#' @examples
#' \dontrun{
#' library(stubble)
#' }
#' gen_col(iris$Sepal.length)
#' gen_col(iris$Species)
#'
#' @export
gen_col <- function(col, elements = 10L, index = 1L, ...) {

  ctrl <- gen_col_control(...)
  this_ctrl <- lapply(ctrl, get_ctrl_element, index = index)

  tryCatch(
    syn_col <- gen_col_(col, elements = elements, ctrl = this_ctrl),
    error = function(err) {
      warning("Could not generate data for ", class(col), "; returning NAs" )
    }
  )

  if (!exists("syn_col")) {
    syn_col <- gen_col_.default(
      col, elements = elements, ctrl = this_ctrl
    )
  }

  invisible(syn_col)

}


gen_col_ <- function(col, elements, index, ctrl) {
  UseMethod("gen_col_", col)
}


gen_col_.default <- function(col, elements, index, ctrl) {

  # Note, no user control over class of vector in this case
  as.numeric(rep(NA, elements))

}


gen_col_.numeric <- function(col, elements, ctrl) {

  old_kind = RNGkind()[1]
  RNGkind(kind = ctrl$dbl_rng_kind)

  syn_col <- stats::runif(elements, ctrl$dbl_min, ctrl$dbl_max)

  RNGkind(kind = old_kind)

  if (!is.na(ctrl$dbl_round))
    syn_col <- round(syn_col, digits = ctrl$dbl_round)
  if (!is.na(ctrl$dbl_signif))
    syn_col <- signif(syn_col, digits = ctrl$dbl_signif)

  syn_col

}


gen_col_.integer <- function(col, elements, ctrl) {

  # Enforce types
  int_min <- as.integer(ctrl$int_min)
  int_max <- as.integer(ctrl$int_max)
  unique <- as.logical(ctrl$unique)

  int_min - 1L + sample.int(
    int_max - int_min + 1L,
    size = as.integer(elements),
    replace = !unique
  )

}


gen_col_.character <- function(col, elements, ctrl) {

  # Enforce types
  chr_min <- as.integer(ctrl$chr_min)
  chr_max <- as.integer(ctrl$chr_max)
  unique <- as.logical(ctrl$unique)

  char_lengths <- gen_col_.integer(
    col, elements,
    gen_col_control(int_min = chr_min, int_max = chr_max, old_ctrl = ctrl)
  )
  sapply(
    sapply(char_lengths, resample, x = ctrl$chr_sym, replace = !unique),
    paste0, collapse = "", simplify = TRUE
  )

}


gen_col_.factor <- function(col, elements, ctrl) {

  # Enforce types
  fct_lvls <- as.character(unlist(ctrl$fct_lvls))
  fct_use_lvls <- unlist(ctrl$fct_use_lvls)
  if (is.null(fct_use_lvls)) fct_use_lvls <- fct_lvls
  fct_use_lvls <- as.character(fct_use_lvls)
  if (!all(fct_use_lvls %in% fct_lvls))
    warning("`fct_use_lvls` is not a subset of `fct_lvls`; see ?control")

  fct_as_chr <- gen_col_.character(
    col, elements,
    gen_col_control(
      chr_min = 1L, chr_max = 1L,
      chr_sym = fct_use_lvls,
      old_ctrl = ctrl
    )
  )

  factor(fct_as_chr, levels = fct_lvls)

}


gen_col_.logical <- function(col, elements, ctrl) {

  as.logical(
    gen_col_.integer(
      col, elements,
      gen_col_control(int_min = 0L, int_max = 1L, old_ctrl = ctrl)
    )
  )

}


gen_col_.POSIXct <- function(col, elements, ctrl) {

  dbl_date = gen_col_.numeric(
    col, elements,
    gen_col_control(
      dbl_min = 0, dbl_max = as.numeric(ctrl$dttm_max),
      old_ctrl = ctrl
    )
  )

  as.POSIXct(
    dbl_date,
    tz = ctrl$dttm_tz,
    origin = ctrl$date_origin
  )

}


gen_col_.Date <- function(col, elements, ctrl) {

  as.Date(
    gen_col_.integer(
      col, elements,
      gen_col_control(
        dbl_min = 0L, dbl_max = as.integer(ctrl$date_max),
        old_ctrl = ctrl
      )
    ),
    tz = ctrl$dttm_tz,
    origin = ctrl$date_origin
  )

}


gen_col_.list <- function(col, elements, ctrl) {

  as.list(rep(NA, elements))

}


resample <- function(size, x, replace = FALSE, prob = NULL) {
  base::sample(x, size, replace, prob)
}


get_ctrl_element <- function(item, index) {
  ifelse(length(item) >= index, item[index], rep_len(item, index)[index])
}
