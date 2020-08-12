#===============================#
#                               #
#### MISCELLANEOUS FUNCTIONS ####
#                               #
#===============================#


### ToDo ###
# - Make imput_na() an S3 Method with an impute_na.bit() method that coerces the
#   vector to type logical where p_na != 0 and a default method.
# - Alter fuzz() to accept syn_col, sd, elements and ctrl as parameters.


### Notes ###
# - Missing values are not possible for vectors of class 'bit'. They will need
#   to be coerced to logical.


### impute_na() ###
impute_na <- function(syn_col, p_na){
  
  ## Simulate Missing Data ##
  if(p_na == 1){
    
    syn_col[] <- NA
    
  } else {
    
    syn_col[rbinom(n = length(syn_col), size = 1L, prob = p_na) == 1L] <- NA
    
  }
  
  ## Output ##
  return(syn_col)
  
}


### is.installed.package() ###
is.installed.package <- function(pkg, ...){
  
  ## Assert Package Installation ##
  status <- pkg %in% rownames(installed.packages(...))
  
  ## Output ##
  return(status)
  
}


### fuzz() ###
fuzz <- function(syn_col, col_sd, elements, ctrl){
# fuzz <- function(col, syn_col, elements, ctrl){
  
  ## Limits ##
  # limits <- range(col, na.rm = TRUE) # No
  
  ## SD ##
  # col_sd <- sd(col, na.rm = TRUE)
  syn_col_sd <- sd(syn_col, na.rm = TRUE)
  fuzz_sd <- abs(col_sd - syn_col_sd)
  
  ## Apply Fuzzing ##
  fuzz_col <- syn_col + rnorm(elements, fuzz_sd)
  
  # ## Check OOB ##
  # oob <- length(fuzz_col[fuzz_col < limits[1] | fuzz_col > limits[2]])
  # 
  # i <- 1L
  # 
  # while(oob != 0){
  #   
  #   ## Indices to Fuzz ##
  #   ind <- which(fuzz_col <  limits[1] | fuzz_col > limits[2])
  #   
  #   ## Re-Fuzz ECDF Data ##
  #   fuzz_col[ind] <- syn_col[ind] + rnorm(oob, 0, fuzz_sd)
  #   
  #   ## Check OOB ##
  #   oob <- length(fuzz_col[fuzz_col <  limits[1] | fuzz_col > limits[2]])
  #   
  #   ## Break Clause ##
  #   if (i == 1e3) {
  #     
  #     warning(
  #       paste(
  #         "Maximum number of iterations reached.",
  #         "Some values will be outside the range of the source data.",
  #         sep = "\n"
  #       )
  #     )
  #     
  #     break
  #     
  #   }
  #   
  #   ## Iterate ##
  #   i <- i + 1L
  #   
  # }
  
  ## Output ##
  return(fuzz_col)
  
}
