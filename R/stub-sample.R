#' @title
#' Derive parameters for use in sampling methods
#' 
#' @description
#' Internal function for producing parameters to feed into sampling methods.
#' 
#' @concept empirical
#' @concept ecdf
#' @concept sample
#' @concept simulate
#' @concept simulated
#' @concept simulation
#' 
#' @keywords datagen


### stub_sample() ###
#' @noRd
stub_sample <- function(col, ...){
  
  ## Define S3 Method ##
  UseMethod("stub_sample", col)
  
}


### stub_sample.default() ###
#' @export
stub_sample.default <- function(col, ctrl){
  
  ## Warning ##
  .warning_no_method(col)
  
  ## Form Output ##
  sim <- list(
    values = NA_integer_,
    wt = 1L
  )
  
  ## Output ##
  return(sim)
  
}


### stub_sample.character() ###
#' @export
stub_sample.character <- function(col, ctrl){
  
  ## Extract Parameters ##
  sim <- stub_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to Date ##
  sim[["values"]] <- as.character(sim[["values"]])
  
  ## Output ##
  return(sim)
  
}


### stub_sample.Date() ###
#' @export
stub_sample.Date <- function(col, ctrl){
  
  ## Extract Parameters ##
  sim <- stub_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to Date ##
  sim[["values"]] <- as.Date(sim[["values"]], tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(sim)
  
}


### stub_sample.double() ###
#' @export
stub_sample.double <- function(col, ctrl){
  
  ## Extract Parameters ##
  sim <- stub_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to Double ##
  sim[["values"]] <- as.double(sim[["values"]])
  
  ## Output ##
  return(sim)
  
}


### stub_sample.factor() ###
#' @export
stub_sample.factor <- function(col, ctrl){
  
  ## Extract Value Labels ##
  labels <- sort(unique(col))
  
  ## Extract Parameters ##
  sim <- stub_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to Factor ##
  sim[["values"]] <- factor(sim[["values"]], levels = labels)
  
  ## Drop Empty Levels ##
  if (ctrl[["emp_drop_lev"]]) sim[["values"]] <- droplevels(sim[["values"]])
  
  ## Output ##
  return(sim)
  
}


### stub_sample.IDate() ###
#' @export
stub_sample.IDate <- function(col, ctrl){
  
  ## Use Date Method ##
  sim <- NextMethod(col)
  
  ## Attempt Coercion to IDate ##
  if (is.installed.package("data.table")) {
    
    # Coerce to IDate #
    sim[["values"]] <- data.table::as.IDate(sim[["values"]])
    
  } else {
    
    # Warning #
    .warning_coercion(col)
    
  }
  
  ## Output ##
  return(sim)
  
}


### stub_sample.integer() ###
#' @export
stub_sample.integer <- function(col, ctrl){
  
  ## Extract Parameters ##
  sim <- stub_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to Integer ##
  sim[["values"]] <- as.integer(sim[["values"]])
  
  ## Output ##
  return(sim)
  
}


### stub_sample.integer64() ###
#' @export
stub_sample.integer64 <- function(col, ctrl){
  
  ## Extract Parameters ##
  sim <- stub_sample_(col = col, ctrl = ctrl)
  
  ## Attempt Coercion to integer64 ##
  if (is.installed.package("bit64")) {
    
    # Coerce to Integer64 #
    sim[["values"]] <- bit64::as.integer64(sim[["values"]])
    
  } else {
    
    # Warning #
    .warning_coercion(col)
    
  }
  
  ## Output ##
  return(sim)
  
}


### stub_sample.ITime() ###
#' @export
stub_sample.ITime <- function(col, ctrl){
  
  ## Extract Parameters ##
  sim <- stub_sample_(col = col, ctrl = ctrl)
  
  ## Attempt Coercion to ITime ##
  if (is.installed.package("data.table")) {
    
    # Coerce to ITime #
    sim[["values"]] <- data.table::as.ITime(sim[["values"]])
    
  } else {
    
    # Warning #
    .warning_coercion(col)
    
    # Coerce to POSIXct #
    sim[["values"]] <- as.POSIXct(sim[["values"]], origin = "1970-01-01", tz = ctrl[["dttm_tz"]])
    
  }
  
  ## Output ##
  return(sim)
  
}


### stub_sample.logical() ###
#' @export
stub_sample.logical <- function(col, ctrl){
  
  ## Extract Parameters ##
  sim <- stub_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to Logical ##
  sim[["values"]] <- as.logical(sim[["values"]])
  
  ## Output ##
  return(sim)
  
}


### stub_sample.ordered() ###
#' @export
stub_sample.ordered <- function(col, ctrl){
  
  ## Use factor Method ##
  sim <- NextMethod(col)
  
  ## Coerce to Ordered Factor ##
  sim[["values"]] <- ordered(sim[["values"]])
  
  ## Output ##
  return(sim)
  
}


### stub_sample.POSIXct() ###
#' @export
stub_sample.POSIXct <- function(col, ctrl){
  
  ## Extract Parameters ##
  sim <- stub_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to POSIXct ##
  sim[["values"]] <- as.POSIXct(sim[["values"]], tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(sim)
  
}


### stub_sample.POSIXlt() ###
#' @export
stub_sample.POSIXlt <- function(col, ctrl){
  
  ## Coerce to POSIXct ##
  col <- as.POSIXct(col, tz = ctrl[["dttm_tz"]])
  
  ## Extract Parameters ##
  sim <- stub_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to POSIXlt ##
  sim[["values"]] <- as.POSIXlt(sim[["values"]], tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(sim)
  
}


### stub_sample_() ###
#' @noRd
stub_sample_ <- function(col, ctrl){
  
  ## Checks ##
  if (any(!is.numeric(ctrl[["emp_n_exc"]]), !is.numeric(ctrl[["emp_p_exc"]])))
    stop("The 'emp_n_exc' and 'emp_p_exc' control parameters must be of class numeric.")
  if (ctrl[["emp_n_exc"]] %% 1 != 0 | ctrl[["emp_n_exc"]] < 0)
    stop("The 'emp_n_exc' control parameter must be a positive whole number.")
  if (ctrl[["emp_p_exc"]] < 0 | ctrl[["emp_p_exc"]] > 1)
    stop("The 'emp_p_exc' control parameter must be between 0 and 1.")
  if (!is.numeric(ctrl[["emp_fuzz_samp"]]))
    stop("The 'emp_fuzz_samp' control parameter must be of class numeric.")
  if (sign(ctrl[["emp_fuzz_samp"]]) == -1)
    stop("The 'emp_fuzz_samp' control parameter must be a positive value")
  
  ## Tabulate Values ##
  n_obs <- table(col)
  p_obs <- prop.table(n_obs)
  
  ## Omit Low Counts/Prop from Resimulation ##
  p_obs <- p_obs[n_obs >= ctrl[["emp_n_exc"]] & p_obs >= ctrl[["emp_p_exc"]]]
  
  ## Extract Params ##
  values <- names(p_obs)
  wt <- as.vector(p_obs)
  
  ## Fuzz ##
  if (ctrl[["emp_fuzz_samp"]] > 0) {
    
    wt <- wt + runif(n = length(wt), min = -ctrl[["emp_fuzz_samp"]], max = ctrl[["emp_fuzz_samp"]])
    
  }
  
  
  if (length(values) == 0) {
    
    if (length(col[!is.na(col)]) > 0)
      warning("no values to sample from non-empty data vector; is 'emp_n_exc' or 'emp_p_exc' too large?")
    
    values <- NA_character_
    
    wt <- 1L
    
  }
  
  ## Form Output ##
  out <- list(
    values = values,
    wt = wt
  )
  
  ## Output ##
  return(out)
  
}
