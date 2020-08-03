library(testthat)

### is.installed.package() ###
is.installed.package <- function(pkg, ...){
  
  ## Assert Package Installation ##
  status <- pkg %in% rownames(installed.packages(...))
  
  ## Output ##
  return(status)
  
}

test_check("stubble")
