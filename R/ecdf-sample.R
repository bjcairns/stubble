#===================#
#                   #
#### ECDF SAMPLE ####
#                   #
#===================#


### ecdf_sample() ###
ecdf_sample <- function(col, ...){
  
  ## Define S3 Method ##
  UseMethod("ecdf_sample", col)
  
}


### ecdf_sample.default() ###
ecdf_sample.default <- function(col, ctrl = ctrl){
  
  ## Warning ##
  warning("No method exists for vector of class: ", class(col)[1])
  
  ## Form Output ##
  samp <- list(
    values = NA_integer_,
    wt = double(0)
  )
  
  ## Output ##
  return(samp)
  
}


# ### ecdf_sample.bit() ###
# ecdf_sample.bit <- function(col, ctrl){
#   
#   ## Coerce 0-Length Vectors to Logical ##
#   if (length(col) == 0) col <- logical(0)
#   
#   ## Use Logical Method ##
#   samp <- ecdf_sample.logical(col = col, ctrl = ctrl)
#   
#   ## Coerce to Bit ##
#   if("bit" %in% rownames(installed.packages())){
#     
#     samp[["values"]] <- if(length(samp[["values"]]) == 0){
#       
#       bit::bit(0)
#       
#     } else {
#       
#       bit::as.bit(samp[["values"]])
#       
#     }
#     
#   } else {
#     
#     warning("Package 'bit' not found. bit will be converted to logical.")
#     
#   }
#   
#   ## Output ##
#   return(samp)
#   
# }


### ecdf_sample.bit() ###
ecdf_sample.bit <- function(col, ctrl){
  
  ## Use Logical Method ##
  samp <- ecdf_sample.logical(col = col, ctrl = ctrl)
  
  ## Coerce to Bit ##
  if("bit" %in% rownames(installed.packages())){
    
    samp[["values"]] <- bit::as.bit(samp[["values"]])

  } else {
    
    warning("Package 'bit' not found. bit will be converted to logical.")
    
  }
  
  ## Output ##
  return(samp)
  
}


### ecdf_sample.character() ###
ecdf_sample.character <- function(col, ctrl){
  
  ## Extract Parameters ##
  samp <- ecdf_sample_(col = col, ctrl = ctrl)
  
  ## Output ##
  return(samp)
  
}


### sampler_Date() ###
ecdf_sample.Date <- function(col, ctrl){
  
  ## Extract Value Labels ##
  labels <- sort(unique(col))
  
  ## Extract Parameters ##
  samp <- ecdf_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to Date ##
  samp[["values"]] <- as.Date(samp[["values"]], tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(samp)
  
}


### ecdf_sample.double() ###
ecdf_sample.double <- function(col, ctrl){
  
  ## Extract Parameters ##
  samp <- ecdf_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to Double ##
  samp[["values"]] <- as.double(samp[["values"]])
  
  ## Output ##
  return(samp)
  
}


### ecdf_sample.factor() ###
ecdf_sample.factor <- function(col, ctrl){
  
  ## Extract Value Labels ##
  labels <- sort(unique(col))
  
  ## Extract Parameters ##
  samp <- ecdf_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to Factor ##
  samp[["values"]] <- factor(samp[["values"]], levels = labels)
  
  ## Drop Empty Levels ##
  if (ctrl[["drop_lev"]]) samp[["values"]] <- droplevels(samp[["values"]])
  
  ## Output ##
  return(samp)
  
}


### ecdf_sample.IDate() ###
ecdf_sample.IDate <- function(col, ctrl){
  
  ## Use Date Method ##
  samp <- NextMethod(col)
  
  ## Coerce to IDate ##
  if("data.table" %in% rownames(installed.packages())){
    
    samp[["values"]] <- data.table::as.IDate(samp[["values"]])
    
  } else {
    
    warning("Package 'data.table' not found. IDate will be converted to Date.")
    
  }
  
  ## Output ##
  return(samp)
  
}


### ecdf_sample.integer() ###
ecdf_sample.integer <- function(col, ctrl){
  
  ## Extract Parameters ##
  samp <- ecdf_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to Integer ##
  samp[["values"]] <- as.integer(samp[["values"]])
  
  ## Output ##
  return(samp)
  
}


# ### ecdf_sample.integer64() ###
# ecdf_sample.integer64 <- function(col, ctrl){
#   
#   ## Coerce 0-Length Vectors to Double ##
#   if (length(col) == 0) col <- double(0)
#   
#   ## Extract Parameters ##
#   samp <- ecdf_sample.double(col = col, ctrl = ctrl)
#   
#   ## Coerce to Integer64 ##
#   if("bit64" %in% rownames(installed.packages())){
#     
#     samp[["values"]] <- if(length(samp[["values"]]) == 0){
#       
#       bit64::integer64(0)
#       
#     } else {
#       
#       bit64::as.integer64(samp[["values"]])
#       
#     }
#     
#   } else {
#     
#     warning("Package 'bit64' not found. integer64 will be converted to double.")
#     
#   }
#   
#   ## Output ##
#   return(samp)
#   
# }


### ecdf_sample.integer64() ###
ecdf_sample.integer64 <- function(col, ctrl){
  
  ## Extract Parameters ##
  samp <- ecdf_sample.double(col = col, ctrl = ctrl)
  
  ## Coerce to Integer64 ##
  if("bit64" %in% rownames(installed.packages())){
    
    samp[["values"]] <- bit64::as.integer64(samp[["values"]])
    
  } else {
    
    warning("Package 'bit64' not found. integer64 will be converted to double.")
    
  }
  
  ## Output ##
  return(samp)
  
}


### ecdf_sample.logical() ###
ecdf_sample.logical <- function(col, ctrl){
  
  ## Extract Parameters ##
  samp <- ecdf_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to Logical ##
  samp[["values"]] <- as.logical(samp[["values"]])
  
  ## Output ##
  return(samp)
  
}


### ecdf_sample.ordered() ###
ecdf_sample.ordered <- function(col, ctrl){
  
  ## Use Factor Method ##
  samp <- NextMethod(col)
  
  ## Coerce to Ordered Factor ##
  samp[["values"]] <- ordered(samp[["values"]])
  
  ## Output ##
  return(samp)
  
}


### ecdf_sample.POSIXct() ###
ecdf_sample.POSIXct <- function(col, ctrl){
  
  ## Extract Parameters ##
  samp <- ecdf_sample_(col = col, ctrl = ctrl)
  
  ## Coerce to POSIXct ##
  samp[["values"]] <- as.POSIXct(samp[["values"]], tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(samp)
  
}


### ecdf_sample.POSIXlt() ###
ecdf_sample.POSIXlt <- function(col, ctrl){
  
  ## Coerce to POSIXct ##
  col <- as.POSIXct(col, tz = ctrl[["dttm_tz"]])
  
  ## Use POSIXct Method ##
  samp <- ecdf_sample.POSIXct(col = col, ctrl = ctrl)
  
  ## Coerce to POSIXlt ##
  samp[["values"]] <- as.POSIXlt(samp[["values"]], tz = ctrl[["dttm_tz"]])
  
  ## Output ##
  return(samp)
  
}


### ecdf_sample_() ###
ecdf_sample_ <- function(col, ctrl){
  
  ## Checks ##
  if (any(!is.numeric(ctrl[["n_exc"]]), !is.numeric(ctrl[["p_exc"]]))) stop("The 'n_exc' and 'p_exc' control parameters must be of class numeric.")
  if (ctrl[["n_exc"]] %% 1 != 0 | ctrl[["n_exc"]] < 0) stop("The 'n_exc' control parameter must be a positive whole number.")
  if (ctrl[["p_exc"]] < 0 | ctrl[["p_exc"]] > 1) stop("The 'p_exc' control parameter must be between 0 and 1.")
  if (!is.logical(ctrl[["fuzz_samp"]])) stop("The 'fuzz_samp' control parameter must be of class logical.")
  
  ## Tabulate Values ##
  n_obs <- table(col)
  p_obs <- prop.table(n_obs)
  
  ## Omit Low Counts/Prop from Resimulation ##
  wt <- p_obs[n_obs >= ctrl[["n_exc"]] & p_obs >= ctrl[["p_exc"]]]
  
  ## Extract Params ##
  values <- names(wt)
  wt <- as.vector(wt)
  
  ## Obfuscation ##
  if (ctrl[["fuzz_samp"]]) wt <- wt + runif(length(wt), -1e-3, 1e-3)
  
  ## Form Output ##
  out <- list(
    values = values,
    wt = wt
  )
  
  ## Output ##
  return(out)
  
}
