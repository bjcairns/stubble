#=====================#
#                     #
#### TEST BLE ATTR ####
#                     #
#=====================#


### ToDo ###
# - Everything!


### Params ###
## Control ##
ctrl_def <- list(
  emp_tail_exc = 0,
  emp_fuzz_spl = 0,
  emp_n_exc = 0L,
  emp_p_exc = 0,
  emp_drop_lev = 0,
  emp_fuzz_samp = 0
)

## Vars ##
vars_base <- c("character", "Date", "double", "factor", "integer", "logical", "ordered", "POSIXct", "POSIXlt")
vars_bit64 <- "integer64"
vars_dt <- c("IDate", "ITime")


### Output Classes ###
## base ##
test_that(
  desc = "Output Classes [base].",
  code = {
    expect_identical(
      object = sapply(
        X = lapply(
          X = stub(
            x = l1[vars_base],
            method = "agnostic",
            ctrl = ctrl_def)[["vars"]],
          FUN = ble_attr
        ),
        FUN = class
      ),
      expected = sapply(X = l1[vars_base], class)
    )
  }
)

## bit64 ##
test_that(
  desc = "Ouput Classes [bit64].",
  code = {
    expect_identical(
      object = sapply(
        X = lapply(
          X = stub(
            x = l1[vars_bit64],
            method = "agnostic",
            ctrl = ctrl_def)[["vars"]],
          FUN = ble_attr
        ),
        FUN = class
      ),
      expected = sapply(X = l1[vars_bit64], class)
    )
  }
)

## data.table ##
test_that(
  desc = "Ouput Classes [data.table].",
  code = {
    expect_identical(
      object = sapply(
        X = lapply(
          X = stub(
            x = l1[vars_dt],
            method = "agnostic",
            ctrl = ctrl_def)[["vars"]],
          FUN = ble_attr
        ),
        FUN = class
      ),
      expected = sapply(X = l1[vars_dt], class)
    )
  }
)


### Unsimulatable Data ###
test_that(
  desc = "Unsimulatable data.",
  code = {
    ## Agnostic ##
#     ctrl_def_ag0 <- list()
#     stub_l0_vars <- stub(x = l0, method = "agnostic", ctrl = ctrl_def)[["vars"]]
#     expect_identical(
#       object = lengths(lapply(X = stub_l0_vars, FUN = ble_attr, ctrl = ctrl_def)),
#       expected = lengths(l0),
#       label = "Zero-element source data; zero-element output data (agnostic)"
#     )
#     expect_equivalent(
#       object = sapply(
#         X = lapply(X = stub_l0_vars, FUN = ble_attr, elements = 10L, ctrl = ctrl_def),
#         FUN = function(var){all(is.na(var))}
#       ),
#       expected = rep(TRUE, length(l0)),
#       label = "Zero-element source data; n-element output data (agnostic)"
#     )
    ## Empirical ##
    stub_l0_vars <- stub(x = l0, method = "empirical", ctrl = ctrl_def)[["vars"]]
    expect_identical(
      object = lengths(lapply(X = stub_l0_vars, FUN = ble_attr, ctrl = ctrl_def)),
      expected = lengths(l0),
      label = "Zero-element source data; zero-element output data (empirical)"
    )
    expect_equivalent(
      object = sapply(
        X = lapply(X = stub_l0_vars, FUN = ble_attr, elements = 10L, ctrl = ctrl_def),
        FUN = function(var){all(is.na(var))}
      ),
      expected = rep(TRUE, length(l0)),
      label = "Zero-element source data; n-element output data (empirical)"
    )
    ## Tidy Up ##
    rm(stub_l0_vars)
  }
)


### 'elements' Parameter ###
test_that(
  desc = "'elements' Parameter.",
  code = {
    ## Agnostic ##
    stub_luniq_vars <- stub(x = luniq, method = "agnostic", ctrl = ctrl_def)[["vars"]]
    expect_equivalent(
      object = lengths(lapply(X = stub_luniq_vars, FUN = ble_attr, elements = 10L, ctrl = ctrl_def)),
      expected = rep(10L, length(luniq)),
      label = "elements = 10L (agnostic)"
    )
    expect_equivalent(
      object = lengths(lapply(X = stub_luniq_vars, FUN = ble_attr, elements = 0L, ctrl = ctrl_def)),
      expected = rep(0L, length(luniq)),
      label = "Zero-length outputs (agnostic)"
    )
    ## Empirical ##
    stub_luniq_vars <- stub(x = luniq, method = "empirical", ctrl = ctrl_def)[["vars"]]
    expect_equivalent(
      object = lengths(lapply(X = stub_luniq_vars, FUN = ble_attr, elements = 10L, ctrl = ctrl_def)),
      expected = rep(10L, length(luniq)),
      label = "elements = 10L (empirical)"
    )
    expect_equivalent(
      object = lengths(lapply(X = stub_luniq_vars, FUN = ble_attr, elements = 0L, ctrl = ctrl_def)),
      expected = rep(0L, length(luniq)),
      label = "Zero-length outputs (empirical)"
    )
    ## Tidy Up ##
    rm(stub_luniq_vars)
  }
)


### Tidy Up ###
rm(ctrl_def, vars_base, vars_bit64, vars_dt)
