#====================#
#                    #
#### BLE AGNOSTIC ####
#                    #
#====================#


### Notes ###
# - Use a combination of sample() and runif() to derive agnostic data from
#   parameters embedded in stub list element. All values should be able to be
#   overridden by user when passing ctrl parameters to ble().
#   specified values in stubble_ctrl().


### ble_agnostic() ###
#' @noRd
ble_agnostic <- function(dtype, ...){
  
  ## Define S3 Method ##
  UseMethod("ble_agnostic", dtype)
  
}


### ble_agnostic.default() ###
#' @noRd
ble_agnostic.default <- function(dtype, elements, ...){
  
  ## Warning ##
  .warn_no_method(dtype)
  
  ## Generate NA Data ##
  syn_col <- rep(NA_integer_, elements)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.character() ###
#' @noRd
ble_agnostic.character <- function(dtype, elements, ctrl){
  
  ## Checks ##
  uniq <- as.logical(ctrl[["unique"]])
  chr_min <- as.integer(ctrl[["chr_min"]])
  chr_max <- as.integer(ctrl[["chr_max"]])
  chr_sym <- sapply(ctrl[["chr_sym"]], as.character)
  chr_sep <- as.character(ctrl[["chr_sep"]])
  try_uniq <- as.logical(ctrl[["chr_try_unique"]])
  try_attempts <- as.integer(ctrl[["chr_try_unique_attempts"]])
  dups_nmax <- as.integer(ctrl[["chr_duplicates_nmax"]])
  
  # ## Sample Symbols ##
  # syn_col <- replicate(
  #   n = elements,
  #   expr = paste(
  #     sample(
  #       x = chr_sym,
  #       size = sample(
  #         x = chr_min:chr_max,
  #         size = 1,
  #         replace = TRUE
  #       ),
  #       replace = TRUE
  #     ),
  #     collapse = chr_sep)
  # )
  
  ## Lengths of Each String in the Synthetic Character Vector ##
  char_lengths <- sample(chr_min:chr_max, size = elements, replace = TRUE)
  
  ## Sample Symbols the Number of Times Given by char_lengths ##
  syn_col <- sapply(
    lapply(char_lengths,
           sample,
           x = chr_sym,
           replace = TRUE),
    paste0,
    collapse = chr_sep,
    simplify = TRUE
  )
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    unique = FALSE,
    old_ctrl = lapply(ctrl, list),
    index = 1L
  )
  
  ## Number of Duplicates ##
  dups <- duplicated(syn_col, nmax = dups_nmax)
  ndups <- sum(dups)
  
  while (ndups > 0 & try_attempts > 0 & uniq & try_uniq) {
    
    try_attempts <- try_attempts - 1
    
    syn_col_repl <- ble_agnostic.character(
      ble = ble,
      elements = ndups,
      ctrl = ctrl
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
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.Date() ###
#' @noRd
ble_agnostic.Date <- function(dtype, elements, ctrl){
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    int_min = 0L, int_max = as.integer(ctrl[["date_max"]]),
    old_ctrl = lapply(ctrl, list),
    index = 1L
  )
  
  ## Use integer Method ##
  syn_col <- ble_agnostic.integer(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Coerce to Date ##
  syn_col <- as.Date(syn_col, origin = ctrl[["date_origin"]], tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.double() ###
#' @noRd
ble_agnostic.double <- function(dtype, elements, ctrl){
  
  ## Draw From Uniform Distribution ##
  syn_col <- runif(elements, ctrl[["dbl_min"]], ctrl[["dbl_max"]])
  
  ## Rounding ##
  if (!is.na(ctrl[["dbl_round"]]))
    syn_col <- round(syn_col, digits = ctrl[["dbl_round"]])
  if (!is.na(ctrl[["dbl_signif"]]))
    syn_col <- signif(syn_col, digits = ctrl[["dbl_signif"]])
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.factor() ###
#' @noRd
ble_agnostic.factor <- function(dtype, elements, ctrl){
  
  ## Checks ##
  fct_lvls <- as.character(unlist(ctrl[["fct_lvls"]]))
  fct_use_lvls <- unlist(ctrl[["fct_use_lvls"]])
  uniq <- as.logical(ctrl[["fct_force_unique"]])
  
  if (is.null(fct_use_lvls)) fct_use_lvls <- fct_lvls
  fct_use_lvls <- as.character(fct_use_lvls)
  if (!all(fct_use_lvls %in% fct_lvls))
    warning("`fct_use_lvls` is not a subset of `fct_lvls`; see ?control")
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    unique = uniq,
    chr_min = 1L, chr_max = 1L,
    chr_sym = list(fct_use_lvls),
    chr_try_unique = uniq,
    old_ctrl = lapply(ctrl, list),
    index = 1L
  )
  
  ## Use character method ##
  syn_col <- ble_agnostic.character(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Coerce to factor ##
  syn_col <- factor(syn_col, levels = fct_lvls)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.IDate() ###
#' @noRd
ble_agnostic.IDate <- function(dtype, elements, ctrl){
  
  ## Use Date Method ##
  syn_col <- NextMethod(dtype)
  
  ## Coerce to IDate ##
  syn_col <- if (is.installed.package("data.table")){
    
    data.table::as.IDate(syn_col, origin = ctrl[["date_origin"]])
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.integer() ###
#' @noRd
ble_agnostic.integer <- function(dtype, elements, ctrl){
  
  ## Checks ##
  uniq <- as.logical(ctrl[["unique"]])
  int_list <- as.integer(unlist(ctrl[["int_list"]]))
  int_list <- int_list[!is.na(int_list)]
  
  if (length(int_list) == 0) {
    
    int_min <- as.integer(ctrl[["int_min"]])
    int_max <- as.integer(ctrl[["int_max"]])
    int_list <- int_min:int_max
    
  }
  
  if ((length(int_list) < elements) & uniq) {
    
    stop(
      "Number of possible values must be at least `elements` for uniqueness. ",
      "See ?control for \n`int_max`, `int_min`, and `int_list`.\n"
    )
    
  }
  
  ## Resample Allowed Values ##
  syn_col <- sample(int_list, size = elements, replace = !uniq)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.integer64() ###
#' @noRd
ble_agnostic.integer64 <- function(dtype, elements, ctrl){
  
  ## Use double Method ##
  syn_col <- ble_agnostic.double(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Coerce to integer64 ##
  syn_col <- if (is.installed.package("bit64")) {
    
    bit64::as.integer64(round(syn_col))
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.logical() ###
#' @noRd
ble_agnostic.logical <- function(dtype, elements, ctrl){
  
  ## Checks ##
  uniq <- as.logical(ctrl[["lgl_force_unique"]])
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    unique = uniq,
    int_min = 0L, int_max = 1L,
    old_ctrl = lapply(ctrl, list),
    index = 1L
  )
  
  ## Use integer Method ##
  syn_col <- ble_agnostic.integer(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Coerce to logical ##
  syn_col <- as.logical(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.ordered() ###
#' @noRd
ble_agnostic.ordered <- function(dtype, elements, ctrl){
  
  ## Use factor Method ##
  syn_col <- NextMethod(dtype)
  
  ## Coerce to ordered ##
  syn_col <- ordered(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.POSIXct() ###
#' @noRd
ble_agnostic.POSIXct <- function(dtype, elements, ctrl){
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    dbl_min = 0, dbl_max = as.double(ctrl[["dttm_max"]]),
    old_ctrl = lapply(ctrl, list),
    index = 1L
  )
  
  ## Use double Method ##
  syn_col <- ble_agnostic.double(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Coerce to POSIXct ##
  syn_col <- as.POSIXct(syn_col, origin = ctrl[["date_origin"]], tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.POSIXlt() ###
#' @noRd
ble_agnostic.POSIXlt <- function(dtype, elements, ctrl){
  
  ## Use POSIXct Method ##
  syn_col <- ble_agnostic.POSIXct(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Coerce to POSIXlt ##
  syn_col <- as.POSIXlt(syn_col, tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}
