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
#' @details `gen_col()` calls an internal S3 generic function, `gen_col_()`,
#' with built-in methods for base R vector types.
#'
#' All methods return values sampled uniformly at random, with the exception of
#' `gen_col_.character()`, which samples string lengths uniformly at random and
#' then populates those strings with symbols (strings of one or more
#' characters) chosen uniformly at random from the values in the `chr_sym`
#' control parameter (see [gen_col_control()]). Character results of
#' `gen_col()` are therefore not chosen uniformly at random from the set of all
#' strings which are valid according to the control parameters; short strings
#' are overrepresented.
#'
#' Uniqueness can be demanded (using the `unique` control parameter) for any
#' vector type, but is likely to result in an error for logical and factor
#' vectors. To even attempt to enforce uniqueness for either of these, set
#' the respective control parameters, `lgl_force_unique` and
#' `fct_force_unique`, to `TRUE`.
#'
#' @return Returns a vector of the same class as `col` with `elements` elements.
#'
#' @examples
#' gen_col(iris$Sepal.length)
#' gen_col(iris$Species)
#'
#' @export
gen_col <- function(col, elements = 10L, index = 1L, ...) {

  this_ctrl <- gen_col_control(..., index = index)

  syn_col <- gen_col_(col, elements = elements, ctrl = this_ctrl)

  if (!exists("syn_col")) {
    syn_col <- gen_col_.default(
      col, elements = elements, ctrl = this_ctrl
    )
  }

  invisible(syn_col)

}


gen_col_ <- function(col, ...) {
  UseMethod("gen_col_", col)
}


#' @export
gen_col_.default <- function(col, elements, index, ctrl) {

  # Note, no user control over class of vector in this case
  warning("Could not generate data for ", class(col), "; returning NAs")
  as.numeric(rep(NA, elements))

}


#' @export
gen_col_.numeric <- function(col, elements, ctrl) {

  uniq <- as.logical(ctrl$unique)
  rng_kind <- as.character(ctrl$dbl_rng_kind)

  old_kind = RNGkind()[1]
  on.exit(RNGkind(kind = old_kind))
  RNGkind(kind = rng_kind)

  syn_col <- stats::runif(elements, ctrl$dbl_min, ctrl$dbl_max)

  if (rng_kind != "Wichmann-Hill" & !uniq) {
    warning(
      "Random number generators other than 'Wichmann-Hill' may return ",
      "non-unique results in \nlarge columns. ",
      "See ?control for `unique = TRUE` to enforce uniqueness."
    )
  } else if (uniq) {
    if (anyDuplicated(syn_col)) {
      stop(
        "Duplicate values in column but `unique = TRUE`. ",
        "See ?control and \ntry `dbl_rng_kind = \"Wichmann-Hill\"`.\n"
      )
      stop()
    }
  }

  if (!is.na(ctrl$dbl_round))
    syn_col <- round(syn_col, digits = ctrl$dbl_round)
  if (!is.na(ctrl$dbl_signif))
    syn_col <- signif(syn_col, digits = ctrl$dbl_signif)

  return(syn_col)

}


#' @export
gen_col_.integer <- function(col, elements, ctrl) {

  # Enforce types
  int_min <- as.integer(ctrl$int_min)
  int_max <- as.integer(ctrl$int_max)
  uniq <- as.logical(ctrl$unique)

  if ((int_max - int_min + 1 < elements) & uniq) {
    stop(
      "Number of possible values must be at least `elements` for uniqueness. ",
      "See ?control for \n`int_max` and `int_min`.\n"
    )
  }

  int_min - 1L + sample.int(
    int_max - int_min + 1L,
    size = as.integer(elements),
    replace = !uniq
  )

}


#' @export
gen_col_.character <- function(col, elements, ctrl) {

  # Enforce types
  chr_min <- as.integer(ctrl$chr_min)
  chr_max <- as.integer(ctrl$chr_max)
  chr_sym <- sapply(ctrl$chr_sym, as.character)
  uniq <- as.logical(ctrl$unique)
  try_uniq <- as.logical(ctrl$chr_try_unique)
  try_attempts <- as.integer(ctrl$chr_try_unique_attempts)
  dups_nmax <- as.integer(ctrl$chr_duplicates_nmax)

  # Lengths of each string in the synthetic character vector
  char_lengths <- gen_col_.integer(
    as.integer(), elements,
    gen_col_control(
      int_min = chr_min, int_max = chr_max,
      old_ctrl = lapply(ctrl, list),
      unique = FALSE,
      index = 1L
    )
  )

  # Sample symbols the number of times given by char_lengths
  syn_col <- sapply(
    lapply(char_lengths, resample, x = chr_sym, replace = TRUE),
    paste0, collapse = "", simplify = TRUE
  )

  # duplicates and their count
  dups <- duplicated(syn_col, nmax = dups_nmax)
  ndups <- sum(dups)

  while (ndups > 0 & try_attempts > 0 & uniq & try_uniq) {

    try_attempts <- try_attempts - 1

    syn_col_repl <- gen_col_.character(
      as.character(),
      ndups,
      ctrl = gen_col_control(
        unique = FALSE,
        old_ctrl = lapply(ctrl, list),
        index = 1L
      )
    )
    syn_col[dups] <- syn_col_repl

    # duplicates and their count
    dups <- duplicated(syn_col, nmax = dups_nmax)
    ndups <- sum(dups)

  }

  # Stop if there are still duplicates, and uniqueness is required, and either
  # no attempts remain or uniqueness should not be forced
  if ((ndups > 0) & uniq & (try_attempts <= 0 | !try_uniq)) {

    stop(
      "Duplicate values required but not generated. ",
      "See ?control for `chr_try_unique` and `chr_try_unique_attempts`."
    )

  }

  return(syn_col)

}


#' @export
gen_col_.factor <- function(col, elements, ctrl) {

  # Enforce types
  fct_lvls <- as.character(unlist(ctrl$fct_lvls))
  fct_use_lvls <- unlist(ctrl$fct_use_lvls)
  uniq <- as.logical(ctrl$fct_force_unique)

  if (is.null(fct_use_lvls)) fct_use_lvls <- fct_lvls
  fct_use_lvls <- as.character(fct_use_lvls)
  if (!all(fct_use_lvls %in% fct_lvls))
    warning("`fct_use_lvls` is not a subset of `fct_lvls`; see ?control")

  fct_as_chr <- gen_col_.character(
    col, elements,
    gen_col_control(
      unique = uniq,
      chr_min = 1L, chr_max = 1L,
      chr_sym = list(fct_use_lvls),
      old_ctrl = lapply(ctrl, list),
      index = 1L
    )
  )

  factor(fct_as_chr, levels = fct_lvls)

}


#' @export
gen_col_.logical <- function(col, elements, ctrl) {

  uniq <- as.logical(ctrl$lgl_force_unique)

  as.logical(
    gen_col_.integer(
      col, elements,
      gen_col_control(
        unique = uniq,
        int_min = 0L, int_max = 1L,
        old_ctrl = lapply(ctrl, list),
        index = 1L
      )
    )
  )

}


#' @export
gen_col_.POSIXct <- function(col, elements, ctrl) {

  dbl_date = gen_col_.numeric(
    col, elements,
    gen_col_control(
      dbl_min = 0, dbl_max = as.numeric(ctrl$dttm_max),
      old_ctrl = lapply(ctrl, list),
      index = 1L
    )
  )

  as.POSIXct(
    dbl_date,
    tz = ctrl$dttm_tz,
    origin = ctrl$date_origin
  )

}


#' @export
gen_col_.Date <- function(col, elements, ctrl) {

  as.Date(
    gen_col_.integer(
      col, elements,
      gen_col_control(
        dbl_min = 0L, dbl_max = as.integer(ctrl$date_max),
        old_ctrl = lapply(ctrl, list),
        index = 1L
      )
    ),
    tz = ctrl$dttm_tz,
    origin = ctrl$date_origin
  )

}


#' @export
gen_col_.list <- function(col, elements, ctrl) {

  as.list(rep(NA, elements))

}


resample <- function(size, x, replace = FALSE, prob = NULL) {
  base::sample(x, size, replace, prob)
}
