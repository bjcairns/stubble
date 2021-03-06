#====================#
#                    #
#### BLE AGNOSTIC ####
#                    #
#====================#


### ble_agnostic() ###
#' @noRd
ble_agnostic <- function(x, elements, ctrl){
  
  ## Extract stub Params ##
  dtype <- x[["dtype"]]
  
  ## Call S3 Internals ##
  syn_col <- ble_agnostic_(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic_() ###
#' @noRd
ble_agnostic_ <- function(dtype, ...){
  
  ## Define S3 Method ##
  UseMethod("ble_agnostic_", dtype)
  
}


### ble_agnostic_.default() ###
#' @export
ble_agnostic_.default <- function(dtype, elements, ...){
  
  ## Warning ##
  .warning_no_method(dtype)
  
  ## Generate NA Data ##
  syn_col <- rep(NA_integer_, elements)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic_.character() ###
#' @export
ble_agnostic_.character <- function(dtype, elements, ctrl){
  
  ## Extract Control Params ##
  uniq <- ctrl[["agn_unique"]]
  agn_chr_min <- ctrl[["agn_chr_min"]]
  agn_chr_max <- ctrl[["agn_chr_max"]]
  agn_chr_sym <- ctrl[["agn_chr_sym"]]
  agn_chr_sep <- ctrl[["agn_chr_sep"]]
  try_uniq <- ctrl[["agn_chr_try_unique"]]
  try_attempts <- ctrl[["agn_chr_try_unique_attempts"]]
  dups_nmax <- ctrl[["chr_duplicates_nmax"]]
  
  ## Sample Symbols ##
  syn_col <- sample_chars(x = agn_chr_sym,
                          size = elements,
                          nchar_min = agn_chr_min,
                          nchar_max = agn_chr_max,
                          agn_chr_sep = agn_chr_sep)
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    agn_unique = FALSE,
    old_ctrl = lapply(ctrl, list),
    assert_class = FALSE,
    index = 1L
  )
  
  ## Number of Duplicates ##
  dups <- duplicated(syn_col, nmax = dups_nmax)
  ndups <- sum(dups)
  
  while (ndups > 0 & try_attempts > 0 & uniq & try_uniq) {
    
    try_attempts <- try_attempts - 1
    
    syn_col_repl <- ble_agnostic_.character(
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


### ble_agnostic_.Date() ###
#' @export
ble_agnostic_.Date <- function(dtype, elements, ctrl){
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    agn_dbl_min = as.double(ctrl[["agn_date_min"]]),
    agn_dbl_max = as.double(ctrl[["agn_date_max"]]),
    old_ctrl = lapply(ctrl, list),
    assert_class = FALSE,
    index = 1L
  )
  
  ## Use double Method ##
  syn_col <- ble_agnostic_.double(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Coerce to Date ##
  syn_col <- as.Date(syn_col, origin = ctrl[["agn_date_origin"]], tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic_.double() ###
#' @export
ble_agnostic_.double <- function(dtype, elements, ctrl){
  
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


### ble_agnostic_.factor() ###
#' @export
ble_agnostic_.factor <- function(dtype, elements, ctrl){
  
  ## Extract Control Params ##
  agn_fct_lvls <- unlist(ctrl[["agn_fct_lvls"]])
  agn_fct_use_lvls <- unlist(ctrl[["agn_fct_use_lvls"]])
  uniq <- ctrl[["agn_fct_force_unique"]]
  
  if (is.null(agn_fct_use_lvls)) agn_fct_use_lvls <- agn_fct_lvls
  if (!all(agn_fct_use_lvls %in% agn_fct_lvls))
    warning("`agn_fct_use_lvls` is not a subset of `agn_fct_lvls`; see ?control")
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    agn_unique = uniq,
    agn_chr_min = 1L,
    agn_chr_max = 1L,
    agn_chr_sym = list(agn_fct_use_lvls),
    agn_chr_try_unique = uniq,
    old_ctrl = lapply(ctrl, list),
    assert_class = FALSE,
    index = 1L
  )
  
  ## Use character method ##
  syn_col <- ble_agnostic_.character(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Coerce to factor ##
  syn_col <- factor(syn_col, levels = agn_fct_lvls)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic_.IDate() ###
#' @export
ble_agnostic_.IDate <- function(dtype, elements, ctrl){
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    agn_int_min = as.integer(ctrl[["agn_date_min"]]),
    agn_int_max = as.integer(ctrl[["agn_date_max"]]),
    agn_int_list = NA_integer_,
    old_ctrl = lapply(ctrl, list),
    assert_class = FALSE,
    index = 1L
  )
  
  ## Use integer Method ##
  syn_col <- ble_agnostic_.integer(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Attempt Coercion to IDate ##
  syn_col <- if (getOption("stubble_has_data.table")){
    
    # Coerce to IDate #
    data.table::as.IDate(syn_col, origin = ctrl[["agn_date_origin"]])
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
    # Coerce to Date #
    as.Date(syn_col, origin = ctrl[["agn_date_origin"]])
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic_.integer() ###
#' @export
ble_agnostic_.integer <- function(dtype, elements, ctrl){
  
  ## Extract Control Params ##
  uniq <- ctrl[["agn_unique"]]
  agn_int_list <- unlist(ctrl[["agn_int_list"]])
  
  ## Prespecified Integer List ##
  agn_int_list <- agn_int_list[!is.na(agn_int_list)]
  if (length(agn_int_list) == 0L) {
    
    agn_int_min <- ctrl[["agn_int_min"]]
    agn_int_max <- ctrl[["agn_int_max"]]
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


### ble_agnostic_.integer64() ###
#' @export
ble_agnostic_.integer64 <- function(dtype, elements, ctrl){
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    agn_dbl_min = as.double(ctrl[["agn_int64_min"]]),
    agn_dbl_max = as.double(ctrl[["agn_int64_max"]]),
    old_ctrl = lapply(ctrl, list),
    assert_class = FALSE,
    index = 1L
  )
  
  ## Use double Method ##
  syn_col <- ble_agnostic_.double(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Round ##
  syn_col <- round(syn_col)
  
  ## Attempt Coercion to integer64 ##
  syn_col <- if (getOption("stubble_has_bit64")) {
    
    # Coerce to integer64 #
    bit64::as.integer64(syn_col)
    
  } else {
    
    # Warning #
    .warning_coercion(dtype)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic_.ITime() ###
#' @export
ble_agnostic_.ITime <- function(dtype, elements, ctrl){
  
  ## Redefine Control Parameters ##
  stubble_ctrl(
    agn_int_min = as.integer(ctrl[["agn_time_min"]]),
    agn_int_max = as.integer(ctrl[["agn_time_max"]]),
    agn_int_list = NA_integer_,
    old_ctrl = lapply(ctrl, list),
    assert_class = FALSE,
    index = 1L
  )
  
  ## Use integer Method ##
  syn_col <- ble_agnostic_.integer(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Attempt Coercion to ITime ##
  syn_col <- if (getOption("stubble_has_data.table")) {
    
    # Coerce to ITime #
    data.table::as.ITime(syn_col)
    
  } else {
    
    # Coerce to POSIXct #
    as.POSIXct(syn_col, origin = ctrl[["agn_date_origin"]], tz = ctrl[["dttm_tz"]])
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic_.logical() ###
#' @export
ble_agnostic_.logical <- function(dtype, elements, ctrl){
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    agn_unique = ctrl[["agn_lgl_force_unique"]],
    agn_int_min = 0L,
    agn_int_max = 1L,
    agn_int_list = NA_integer_,
    old_ctrl = lapply(ctrl, list),
    assert_class = FALSE,
    index = 1L
  )
  
  ## Use integer Method ##
  syn_col <- ble_agnostic_.integer(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Coerce to logical ##
  syn_col <- as.logical(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic_.ordered() ###
#' @export
ble_agnostic_.ordered <- function(dtype, elements, ctrl){
  
  ## Use factor Method ##
  syn_col <- NextMethod(dtype)
  
  ## Coerce to ordered ##
  syn_col <- ordered(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic_.POSIXct() ###
#' @export
ble_agnostic_.POSIXct <- function(dtype, elements, ctrl){
  
  ## Redefine Control Parameters ##
  ctrl <- stubble_ctrl(
    agn_dbl_min = as.double(ctrl[["agn_dttm_min"]]),
    agn_dbl_max = as.double(ctrl[["agn_dttm_max"]]),
    old_ctrl = lapply(ctrl, list),
    assert_class = FALSE,
    index = 1L
  )
  
  ## Use double Method ##
  syn_col <- ble_agnostic_.double(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Coerce to POSIXct ##
  syn_col <- as.POSIXct(syn_col, origin = ctrl[["agn_date_origin"]], tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}


### ble_agnostic_.POSIXlt() ###
#' @export
ble_agnostic_.POSIXlt <- function(dtype, elements, ctrl){
  
  ## Use POSIXct Method ##
  syn_col <- ble_agnostic_.POSIXct(dtype = dtype, elements = elements, ctrl = ctrl)
  
  ## Coerce to POSIXlt ##
  syn_col <- as.POSIXlt(syn_col, tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}
