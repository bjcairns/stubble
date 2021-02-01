#==========#
#          #
#### ZZZ ###
#          #
#==========#


### ToDo ###
# - Fix CRAN R CMD check problem.


### .onLoad() ###
.onLoad <- function(libname, pkgname){
  
  ## Check Package Versions ##
  instPkgs <- is.installed.package(
    pkg = names(OPT_DEP),
    minimum_version = unlist(OPT_DEP) # This line causes devtools::check(cran = T) errors.
  )
  
  ## Set Options ##
  options(stubble_has_bit64 = instPkgs[["bit64"]])
  options(stubble_has_data.table = instPkgs[["data.table"]])
  options(stubble_has_tibble = instPkgs[["tibble"]])
  
}


### .onUnload() ###
.onUnload <- function(libpath){
  
  ## Unset Options ##
  options(stubble_has_bit64 = NULL)
  options(stubble_has_data.table = NULL)
  options(stubble_has_tibble = NULL)
  
}
