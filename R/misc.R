#===============================#
#                               #
#### MISCELLANEOUS FUNCTIONS ####
#                               #
#===============================#


### ToDo ###
# - Assess need to use ctrl[["dttm_tz]] in POSIXc2lt(). Could be overridden elsewhere.


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
  
  ## Calculate Fuzz SD ##
  fuzz_sd <- col_sd*ctrl[["fuzz_ecdf_sca"]]
  
  ## Apply Fuzzing ##
  fuzz_col <- syn_col + rnorm(elements, fuzz_sd)
  
  ## Output ##
  return(fuzz_col)
  
}
