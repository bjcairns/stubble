#=================#
#                 #
#### IMPUTE NA ####
#                 #
#=================#


### ToDo ###
# - Make this an S3 Method with an impute_na.bit() method that coerces the
#   vector to type logical where p_na != 0 and a default method.


### Notes ###
# - Missing values are not possible for vectors of class 'bit'. They will need
#   to be coerced to logical.


### impute_na() ###
impute_na <- function(syn_col, p_na){
  
  ## Checks##
  if(missing(syn_col)) stop("'syn_col' not found.")
  if(missing(p_na)) stop("'p_na' not found.")
  if(!is.numeric(p_na)) stop("'p_na' is not of type numeric.")
  if(length(p_na) != 1) stop("'p_na' is not of length == 1.")
  if(p_na < 0 | p_na > 1) stop("'p_na' out of bounds.")
  
  ## Simulate Missing Data ##
  if(p_na == 1){
    
    syn_col[] <- NA
    
  } else {
    
    syn_col[rbinom(n = length(syn_col), size = 1L, prob = p_na) == 1L] <- NA
    
  }
  
  ## Output ##
  return(syn_col)
  
}
