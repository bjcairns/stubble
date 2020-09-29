#====================#
#                    #
#### BLE AGNOSTIC ####
#                    #
#====================#


### Notes ###
# - Should ctrl params exist for agn_dttm_min and agn_date_min?
# - IDates are currently passed to the integer method, whereas Dates are passed
#   to the double method. This is correct, but will it confuse people?


### ble_agnostic() ###
#' @noRd
ble_agnostic <- function(dtype, ...){
  
  ## Define S3 Method ##
  UseMethod("ble_agnostic", dtype)
  
}


### ble_agnostic.default() ###
#' @export
ble_agnostic.default <- function(dtype, elements, ...){
  
  ## Warning ##
  .warning_no_method(dtype)
  
  ## Generate NA Data ##
  syn_col <- rep(NA_integer_, elements)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.character() ###
#' @export
ble_agnostic.character <- function(dtype, elements, ctrl){
  
  ## Checks ##
  uniq <- as.logical(ctrl[["unique"]])
  agn_chr_min <- as.integer(ctrl[["agn_chr_min"]])
  agn_chr_max <- as.integer(ctrl[["agn_chr_max"]])
  agn_chr_sym <- sapply(ctrl[["agn_chr_sym"]], as.character)
  agn_chr_sep <- as.character(ctrl[["agn_chr_sep"]])
  try_uniq <- as.logical(ctrl[["agn_chr_try_unique"]])
  try_attempts <- as.integer(ctrl[["agn_chr_try_unique_attempts"]])
  dups_nmax <- as.integer(ctrl[["chr_duplicates_nmax"]])
  
  ## Sample Symbols ##
  syn_col <- sample_chars(x = agn_chr_sym,
                          size = elements,
                          nchar_min = agn_chr_min,
                          nchar_max = agn_chr_max,
                          agn_chr_sep = agn_chr_sep)
  
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
      dtype = dtype,
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
      "See ?control for `agn_chr_try_unique` and `agn_chr_try_unique_attempts`."
    )
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.Date() ###
#' @export
ble_agnostic.Date <- function(dtype, elements, ctrl){
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    agn_dbl_min = as.double(as.Date(ctrl[["agn_date_min"]])),
    agn_dbl_max = as.double(as.Date(ctrl[["agn_date_max"]])),
    old_ctrl = lapply(ctrl, list),
    index = 1L
  )
  
  ## Use double Method ##
  syn_col <- ble_agnostic.double(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Coerce to Date ##
  syn_col <- as.Date(syn_col, origin = ctrl[["date_origin"]], tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.double() ###
#' @export
ble_agnostic.double <- function(dtype, elements, ctrl){
  
  ## Draw From Uniform Distribution ##
  syn_col <- runif(elements, ctrl[["agn_dbl_min"]], ctrl[["agn_dbl_max"]])
  
  ## Rounding ##
  if (!is.na(ctrl[["agn_dbl_round"]]))
    syn_col <- round(syn_col, digits = ctrl[["agn_dbl_round"]])
  if (!is.na(ctrl[["agn_dbl_signif"]]))
    syn_col <- signif(syn_col, digits = ctrl[["agn_dbl_signif"]])
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.factor() ###
#' @export
ble_agnostic.factor <- function(dtype, elements, ctrl){
  
  ## Checks ##
  agn_fct_lvls <- as.character(unlist(ctrl[["agn_fct_lvls"]]))
  agn_fct_use_lvls <- unlist(ctrl[["agn_fct_use_lvls"]])
  uniq <- as.logical(ctrl[["agn_fct_force_unique"]])
  
  if (is.null(agn_fct_use_lvls)) agn_fct_use_lvls <- agn_fct_lvls
  agn_fct_use_lvls <- as.character(agn_fct_use_lvls)
  if (!all(agn_fct_use_lvls %in% agn_fct_lvls))
    warning("`agn_fct_use_lvls` is not a subset of `agn_fct_lvls`; see ?control")
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    unique = uniq,
    agn_chr_min = 1L, agn_chr_max = 1L,
    agn_chr_sym = list(agn_fct_use_lvls),
    agn_chr_try_unique = uniq,
    old_ctrl = lapply(ctrl, list),
    index = 1L
  )
  
  ## Use character method ##
  syn_col <- ble_agnostic.character(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Coerce to factor ##
  syn_col <- factor(syn_col, levels = agn_fct_lvls)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.IDate() ###
#' @export
ble_agnostic.IDate <- function(dtype, elements, ctrl){
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    agn_int_min = as.integer(as.Date(ctrl[["agn_date_min"]])),
    agn_int_max = as.integer(as.Date(ctrl[["agn_date_max"]])),
    old_ctrl = lapply(ctrl, list),
    index = 1L
  )
  
  ## Use integer Method ##
  syn_col <- ble_agnostic.integer(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Attempt Coercion to IDate ##
  syn_col <- if (is.installed.package("data.table")){
    
    # Coerce to IDate #
    data.table::as.IDate(syn_col, origin = ctrl[["date_origin"]])
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
    # Coerce to Date #
    as.Date(syn_col, origin = ctrl[["date_origin"]])
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.integer() ###
#' @export
ble_agnostic.integer <- function(dtype, elements, ctrl){
  
  ## Checks ##
  uniq <- as.logical(ctrl[["unique"]])
  agn_int_list <- as.integer(unlist(ctrl[["agn_int_list"]]))
  agn_int_list <- agn_int_list[!is.na(agn_int_list)]
  
  if (length(agn_int_list) == 0) {
    
    agn_int_min <- as.integer(ctrl[["agn_int_min"]])
    agn_int_max <- as.integer(ctrl[["agn_int_max"]])
    agn_int_list <- agn_int_min:agn_int_max
    
  }
  
  if ((length(agn_int_list) < elements) & uniq) {
    
    stop(
      "Number of possible values must be at least `elements` for uniqueness. ",
      "See ?control for \n`agn_int_max`, `agn_int_min`, and `agn_int_list`.\n"
    )
    
  }
  
  ## Resample Allowed Values ##
  syn_col <- sample(agn_int_list, size = elements, replace = !uniq)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.integer64() ###
#' @export
ble_agnostic.integer64 <- function(dtype, elements, ctrl){
  
  ## Use double Method ##
  syn_col <- ble_agnostic.double(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Attempt Coercion to integer64 ##
  syn_col <- if (is.installed.package("bit64")) {
    
    # Coerce to integer64 #
    bit64::as.integer64(syn_col)
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.ITime() ###
#' @export
ble_agnostic.ITime <- function(dtype, elements, ctrl){
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    agn_int_min = 0L,
    agn_int_max = 86399L,
    old_ctrl = lapply(ctrl, list),
    index = 1L
  )
  
  ## Use integer Method ##
  syn_col <- ble_agnostic.integer(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Attempt Coercion to ITime ##
  syn_col <- if (is.installed.package("data.table")) {
    
    # Coerce to ITime #
    data.table::as.ITime(syn_col)
    
  } else {
    
    # Coerce to POSIXct #
    as.POSIXct(syn_col, origin = ctrl[["date_origin"]], tz = ctrl[["dttm_tz"]])
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.logical() ###
#' @export
ble_agnostic.logical <- function(dtype, elements, ctrl){
  
  ## Checks ##
  uniq <- as.logical(ctrl[["agn_lgl_force_unique"]])
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    unique = uniq,
    agn_int_min = 0L, agn_int_max = 1L,
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
#' @export
ble_agnostic.ordered <- function(dtype, elements, ctrl){
  
  ## Use factor Method ##
  syn_col <- NextMethod(dtype)
  
  ## Coerce to ordered ##
  syn_col <- ordered(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic.POSIXct() ###
#' @export
ble_agnostic.POSIXct <- function(dtype, elements, ctrl){
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    agn_dbl_min = as.double(as.POSIXct(ctrl[["agn_dttm_min"]], tz = ctrl[["dttm_tz"]])),
    agn_dbl_max = as.double(as.POSIXct(ctrl[["agn_dttm_max"]], tz = ctrl[["dttm_tz"]])),
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
#' @export
ble_agnostic.POSIXlt <- function(dtype, elements, ctrl){
  
  ## Use POSIXct Method ##
  syn_col <- ble_agnostic.POSIXct(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Coerce to POSIXlt ##
  syn_col <- as.POSIXlt(syn_col, tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}
