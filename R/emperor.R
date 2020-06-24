#' @title
#' Generate synthetic data using the empirical distribution of an R vector
#' 
#' @description
#' `emperor()` does the real work for [stubblise()]. Currently supported column
#' classes are `double`, `integer`, `character`, `factor`, `ordered`, `logical`,
#' `POSIXct`, `POSIXlt`, `Date` and `IDate`. There is limited support for `list`
#' (returns a list with all `NA`s).
#' 
#' @param col the vector from which the type of the synthetic data is taken.
#' @param elements the number of elements to generate.
#' @param index the index of the column (i.e. the position in the data frame)
#' for the purposes of extracting control parameters.
#' @param control a named list of control parameters for generating the
#' synthetic data. See [control].
#' @param ... named individual control parameters, which take precedence over
#' those in the `control` list.
#'
#' @details
#' `emperor()` calls an internal S3 generic function, `emperor_()`,
#' with built-in methods for base R vector types.
#' 
#' All methods return values sampled from the cumulative empirical distribution
#' function.
#' 
#' @return
#' Returns a vector of the same class as `col` with `elements` elements.
#' 
#' @note
#' This function is entirely experimental and should not be used yet. There are
#' currently risks of data leakage and many of the methods do not yet work.
#' 
#' @seealso 
#' [emperor_control()]
#' 
#' @examples
#' emperor(iris$Sepal.length)
#' emperor(iris$Species)
#' 
#' @concept empirical
#' @concept ecdf
#' @concept resample
#' @concept simulate
#' @concept simulated
#' @concept simulation
#' 
#' @keywords datagen distribution manip
#' 
#' @importFrom stats runif rnorm
#' @importFrom utils installed.packages
#' @importFrom grDevices nclass.FD
#' @importFrom graphics hist
#' @importFrom stats approxfun


### emperor() ###
#' @export
emperor <- function(col, elements = length(col), index = 1L, control = list(), ...){
  
  ## Checks ##
  if (!is.numeric(elements)) stop("'elements' argument must be numeric.")
  if (elements %% 1 != 0) stop("'elements' argument must be a whole number.")
  if (length(elements) > 1) stop("'elements' argument must be of length 1.")
  if (!{elements >= 1}) stop("'elements' argument must be >= 1.")
  
  ## Set Control Parameters ##
  this_ctrl <- emperor_control(..., old_ctrl = control, index = index)
  
  ## Simulate According to Data Type ##
  syn_col <- emperor_(col, elements = elements, ctrl = this_ctrl)
  
  ## Fallback Simulation Method ##
  if(!exists("syn_col")) syn_col <- emperor_.default(col, elements = elements, ctrl = this_ctrl)
  
  ## Parameterise p_na ##
  if(!is.na(this_ctrl[["p_na"]])){
    
    p_na <- as.double(this_ctrl[["p_na"]])
    
    if(p_na > 1){
      
      warning("Control parameter p_na > 1; value has been reset to 1")
      p_na <- 1
      
    } else if(p_na < 0){
      
      warning("Control parameter p_na < 0; value has been reset to 0")
      p_na <- 0
      
    }
    
  } else {
    
    p_na <- sum(is.na(col))/length(col)
    
  }
  
  ## Simulate Missing Data ##
  if(p_na == 1){
    
    syn_col[] <- NA
    
  } else if(p_na > 0 & p_na < 1){
    
    syn_col[rbinom(elements, 1L, p_na) == 1L] <- NA
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### emperor_() ###
#' @noRd
emperor_ <- function(col, ...){
  
  UseMethod("emperor_", col)
  
}


### emperor_arbiter() ###
#' @noRd
emperor_arbiter <- function(col, ctrl){
  
  # Complete Cases Only #
  cc_col <- col[!is.na(col)]
  
  # Uniqueness #
  p_uniq <- length(unique(cc_col))/length(cc_col)
  
  # Method Selection #
  method <- if(p_uniq > ctrl[["emp_sw"]]) "ecdf" else "sample" 
  
  # Output #
  return(method)
  
}


### fuzz_col() ###
#' @noRd
fuzz <- function(col, syn_col, elements, ctrl){
  
  # ## Debugging ##
  # cat("fuzz_col()", "\n")
  
  ## Limits ##
  limits <- range(col, na.rm = TRUE)
  
  ## SD ##
  col_sd <- sd(col, na.rm = TRUE)
  syn_col_sd <- sd(syn_col, na.rm = TRUE)
  fuzz_sd <- abs(col_sd - syn_col_sd)
  
  ## Apply Fuzzing ##
  fuzz_col <- syn_col + rnorm(elements, fuzz_sd)
    
  ## Check OOB ##
  oob <- length(fuzz_col[fuzz_col < limits[1] | fuzz_col > limits[2]])
  
  i <- 1L
  
  while(oob != 0){
    
    ## Indices to Fuzz ##
    ind <- which(fuzz_col <  limits[1] | fuzz_col > limits[2])
    
    ## Re-Fuzz ECDF Data ##
    fuzz_col[ind] <- syn_col[ind] + rnorm(oob, 0, fuzz_sd)
    
    ## Check OOB ##
    oob <- length(fuzz_col[fuzz_col <  limits[1] | fuzz_col > limits[2]])
    
    ## Break Clause ##
    if (i == 1e3) {
      
      warning(
        paste(
          "Maximum number of iterations reached.",
          "Some values will be outside the range of the source data.",
          sep = "\n"
        ))
      break
      
    }
    
    ## Iterate ##
    i <- i + 1L
    
  }
  
  ## Output ##
  return(fuzz_col)
  
}


### emperor_ecdf() ###
#' @noRd
emperor_ecdf <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_ecdf()", "\n")
  
  ## Determine Optimal Number of Breaks ##
  nclass <- nclass.FD(col[!is.na(col)])
  breaks <- seq(min(col, na.rm = TRUE), max(col, na.rm = TRUE), length.out = nclass + 1L)
  
  ## Histogram Object ##
  h <- hist(col, breaks = breaks, plot = FALSE)
  
  ## Cumulative Sum of Densities
  width <- diff(h[["breaks"]])
  area <- width*h[["density"]]
  cdf <- cumsum(area)
  
  ## Remove Duplicates ##
  mids <- h[["mids"]][!duplicated(cdf)]
  cdf <- cdf[!duplicated(cdf)]
  
  ## Generate a Spline Function ##
  inv_cdf_spf <- approxfun(x = cdf, y = mids, n = 10, rule = 1, ties = "ordered")
  
  ## Simulate Using Inverse CDF ##
  syn_col <- rep(NA_real_, elements)
  while (any(is.na(syn_col))){
    
    n <- sum(is.na(syn_col))
    x <- runif(n = n, min = 0 + ctrl[["tail_exc"]], max = 1 - ctrl[["tail_exc"]])
    syn_col[is.na(syn_col)] <- inv_cdf_spf(x)
    
  }
  
  ## Fuzzing ##
  if (ctrl[["fuzz_ecdf"]]){
    
    syn_col <- fuzz(col = col, syn_col = syn_col, elements = elements, ctrl = ctrl)
    
  }
  
  ## Output ##
  return(syn_col)
  
}

#' ### emperor_ecdf() ###
#' #' @noRd
#' emperor_ecdf <- function(col, elements = elements, ctrl){
#'   
#'   # ## Debugging ##
#'   # cat("emperor_ecdf()", "\n")
#'   
#'   ## Empirical CDF ##
#'   fn <- ecdf(col)
#'   
#'   ## Probabilites to Sample (with Distribution Tail Exclusions) ##
#'   p <- runif(elements, 0 + ctrl[["tail_exc"]], 1 - ctrl[["tail_exc"]])
#'   
#'   ## Quantile ECDF ##
#'   syn_col <- quantile(fn, p)
#'   
#'   ## Fuzzing ##
#'   if (ctrl[["fuzz_ecdf"]]){
#'     
#'     if (ctrl[["tail_exc"]] == 0){
#'       limits <- range(col)
#'     } else {
#'       limits <- quantile(col, c(0 + ctrl[["tail_exc"]], 1 - ctrl[["tail_exc"]]))
#'     }
#'     
#'     syn_col <- fuzz_col(syn_col = syn_col, limits = limits, elements = elements, ctrl = ctrl)
#'     
#'   }
#'   
#'   ## Strip Quantile Values ##
#'   names(syn_col) <- NULL
#'   
#'   ## Output ##
#'   return(syn_col)
#'   
#' }


### emperor_sample() ###
#' @noRd
emperor_sample <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_sample()", "\n")
  
  ## Tabulate Values ##
  n_obs <- table(col)
  p_obs <- prop.table(n_obs)
  
  ## Omit Low Counts/Prop from Resimulation ##
  p_obs <- p_obs[n_obs >= ctrl[["n_exc"]] & p_obs >= ctrl[["p_exc"]]]
  
  # Simulate Values #
  if(length(p_obs) != 0){
    
    p_obs <- p_obs + runif(length(p_obs), -1e-3, 1e-3)
    
    syn_col <- sample(as.integer(names(p_obs)), size = elements, replace = TRUE, prob = p_obs)
    
  } else {
    
    syn_col <- rep(NA_integer_, elements)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### emperor_.default() ###
#' @export
emperor_.default <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_.default()", "\n")
  
  warning("Could not generate data for ", class(col), "; returning NAs")
  
  syn_col <- rep(NA_integer_, elements)
  
  ## Output ##
  return(syn_col)
  
}


### emperor_.integer() ###
#' @export
emperor_.integer <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_.integer()", "\n")
  
  ## Determine Type ##
  method <- emperor_arbiter(col, ctrl = ctrl)
  
  ## Apply Appropriate Method ##
  if(method == "sample"){
    
    syn_col <- emperor_sample(col, elements = elements, ctrl = ctrl)
    
  } else if(method == "ecdf"){
    
    syn_col <- emperor_ecdf(col, elements = elements, ctrl = ctrl)
    
  }
  
  ## Coerce to Integer ##
  if(!is.integer(syn_col)) syn_col <- as.integer(round(syn_col))
  
  ## Output ##
  return(syn_col)
  
}


### emperor_.double() ###
#' @export
emperor_.double <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_.double()", "\n")
  
  ## Determine Type ##
  method <- emperor_arbiter(col, ctrl = ctrl)
  
  ## Apply Appropriate Method ##
  if(method == "sample"){
    
    syn_col <- emperor_sample(col, elements = elements, ctrl = ctrl)
    
  } else if(method == "ecdf"){
    
    syn_col <- emperor_ecdf(col, elements = elements, ctrl = ctrl)
    
  }
  
  ## Coerce to Double ##
  if(!is.double(syn_col)) syn_col <- as.double(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### emperor_.logical() ###
#' @export
emperor_.logical <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_.logical()", "\n")
  
  ## Coerce to Integer ##
  col_int <- as.integer(col)
  
  ## Use Sample Method ##
  syn_col <- emperor_sample(col_int, elements = elements, ctrl = ctrl)
  
  ## Coerce to Logical ##
  syn_col <- as.logical(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### emperor_.factor() ###
#' @export
emperor_.factor <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_.factor()", "\n")
  
  ## Coerce to Integer ##
  col_int <- as.integer(col)
  
  ## Use Sample Method ##
  syn_col <- emperor_sample(col_int, elements = elements, ctrl = ctrl)
  
  ## Coerce to Factor ##
  syn_col <- factor(syn_col, levels = sort(unique(col_int)), labels = levels(col))
  
  ## Drop Empty Levels ##
  if(ctrl[["drop_lev"]]) syn_col <- droplevels(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### emperor_.ordered() ###
#' @export
emperor_.ordered <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_.ordered()", "\n")
  
  ## Use Factor Method ##
  syn_col <- NextMethod(col)
  
  ## Coerce to Ordered Factor #
  syn_col <- ordered(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### emperor_.character() ###
#' @export
emperor_.character <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_.character()", "\n")
  
  ## Coerce to Factor ##
  col_fac <- factor(col)
  
  ## Use Factor Method ##
  syn_col <- emperor_.factor(col_fac, elements = elements, ctrl = ctrl)
  
  ## Coerce to Character ##
  syn_col <- as.character(syn_col)
  
  ## Output ##
  return(syn_col)
  
}


### emperor_.POSIXct() ###
#' @export
emperor_.POSIXct <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_.POSIXct()", "\n")
  
  ## Coerce to Double ##
  col <- as.double(col)
  
  ## Determine Type ##
  method <- emperor_arbiter(col, ctrl = ctrl)
  
  ## Apply Appropriate Method ##
  if(method == "sample"){
    
    syn_col <- emperor_sample(col, elements = elements, ctrl = ctrl)
    
  } else if(method == "ecdf"){
    
    syn_col <- emperor_ecdf(col, elements = elements, ctrl = ctrl)
    
  }
  
  ## Coerce to POSIXct ##
  syn_col <- as.POSIXct(round(syn_col), origin = "1970-01-01", tz = ctrl[["dtm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}


### emperor_.POSIXlt() ###
#' @export
emperor_.POSIXlt <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_.POSIXlt()", "\n")
  
  ## Coerce to POSIXct ##
  col <- as.POSIXct(col)
  
  ## Coerce to Double ##
  col <- as.double(col)
  
  ## Determine Type ##
  method <- emperor_arbiter(col, ctrl = ctrl)
  
  ## Apply Appropriate Method ##
  if(method == "sample"){
    
    syn_col <- emperor_sample(col, elements = elements, ctrl = ctrl)
    
  } else if(method == "ecdf"){
    
    syn_col <- emperor_ecdf(col, elements = elements, ctrl = ctrl)
    
  }
  
  ## Coerce to POSIXlt ##
  syn_col <- as.POSIXlt(round(syn_col), origin = "1970-01-01", tz = ctrl[["dtm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}


### emperor_.Date() ###
#' @export
emperor_.Date <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_.Date()", "\n")
  
  ## Coerce to Integer ##
  col <- as.integer(col)
  
  ## Determine Type ##
  method <- emperor_arbiter(col, ctrl = ctrl)
  
  ## Apply Appropriate Method ##
  if(method == "sample"){
    
    syn_col <- emperor_sample(col, elements = elements, ctrl = ctrl)
    
  } else if(method == "ecdf"){
    
    syn_col <- emperor_ecdf(col, elements = elements, ctrl = ctrl)
    
  }
  
  ## Coerce to Date ##
  syn_col <- as.Date(round(syn_col), origin = "1970-01-01", tz = ctrl[["dtm_tz"]])
  
  ## Output ##
  return(syn_col)
  
}


### emperor_.IDate() ###
#' @export
emperor_.IDate <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_.IDate()", "\n")
  
  if("data.table" %in% rownames(installed.packages())){
    
    ## Coerce to Integer ##
    col <- as.integer(col)
    
    ## Determine Type ##
    method <- emperor_arbiter(col, ctrl = ctrl)
    
    ## Apply Appropriate Method ##
    if(method == "sample"){
      
      syn_col <- emperor_sample(col, elements = elements, ctrl = ctrl)
      
    } else if(method == "ecdf"){
      
      syn_col <- emperor_ecdf(col, elements = elements, ctrl = ctrl)
      
    }

    ## Coerce to IDate ##
    syn_col <- data.table::as.IDate(round(syn_col), origin = "1970-01-01", tz = ctrl[["dtm_tz"]])
  
  } else {
    
    warning("Package 'data.table' not found. IDates will be converted to Dates.")
    syn_col <- NextMethod(col)
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### emperor_.list() ###
#' @export
emperor_.list <- function(col, elements = elements, ctrl){
  
  # ## Debugging ##
  # cat("emperor_.list()", "\n")
  
  syn_col <- as.list(rep(NA_integer_, elements))
  
  ## Output ##
  return(syn_col)
  
}
