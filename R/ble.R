#===========#
#           #
#### BLE ####
#           #
#===========#


### ble() ###
ble <- function(stub, rows, ctrl = list(), ...){
  
  ## Create Index ##
  index <- seq_along(stub)
  
  ### Rows ###
  if (missing(rows)) rows <- sapply(stub, `[[`, "n")
  
  ## Apply gen_ble() ##
  l <- mapply(
    FUN = gen_ble,
    x = stub,
    elements = rows,
    index = index,
    MoreArgs = list(ctrl = ctrl, ...),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )
  
  ## Output ##
  return(l)
  
}


# ### Testing ###
# ctrl <- list(p_na = 0.5)
# 
# moo <- stub(iris)
# 
# ble(moo, elements = 10)
