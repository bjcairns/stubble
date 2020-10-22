#==========#
#          #
#### ZZZ ###
#          #
#==========#


### .onLoad() ###
.onLoad <- function(libname, pkgname){
  
  ## Check Package Versions ##
  instPkgs <- is.installed.package(
    pkg = c("bit64", "data.table", "tibble"),
    minimum_version = c("4.0.2", "1.9.8", "1.1")
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
