#=======================#
#                       #
#### TEST BLE SAMPLE ####
#                       #
#=======================#


### Notes ###
# - Missing data tests not required.


### ToDo ###
# - Test elements parameter
# - Test ctrl parameter


### Params ###
## Observations ##
n <- 10L

## Control ##
ctrl_def <- stubble_ctrl(
  index = 1L,
  emp_sw = 1,
  emp_n_exc = 0,
  emp_p_exc = 0,
  emp_fuzz_samp = FALSE
)
ctrl_def <- ctrl_def[names(ctrl_def) != "index"]


## Vars ##
vars_base <- c("character", "Date", "double", "factor", "integer", "logical", "ordered", "POSIXct", "POSIXlt")
vars_dt <- c("IDate", "ITime")


### Unsupported Vector Classes ###
test_that(
  desc = "Unsupported vector classes.",
  code = {
    expect_warning(
      object = ble_sample(NULL, elements = n, ctrl = ctrl_def),
      regexp = "[nN]o\\s+method\\s+exists"
    )
    expect_identical(
      object = suppressWarnings(
        ble_sample(NULL, elements = n, ctrl = ctrl_def)
      ),
      expected = rep(NA_integer_, n)
    )
  }
)


### Zero-Length Vectors ###
## Data ##
stub_l0 <- stub(l0, rows = n, method = "empirical", ctrl = ctrl_def)[["vars"]]

## base ##
test_that(
  desc = "Zero-length vectors [base].",
  code = {
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["character"]], elements = n, ctrl = ctrl_def)
      ),
      expected = character(0),
      label = "character"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["Date"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.Date(character(0)),
      label = "Date"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["double"]], elements = n, ctrl = ctrl_def)
      ),
      expected = double(0),
      label = "double"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["factor"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.factor(character(0)),
      label = "factor"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["integer"]], elements = n, ctrl = ctrl_def)
      ),
      expected = integer(0),
      label = "integer"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["logical"]], elements = n, ctrl = ctrl_def)
      ),
      expected = logical(0),
      label = "logical"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["ordered"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.ordered(character(0)),
      label = "ordered"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["POSIXct"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.POSIXct(character(0), tz = ctrl_def[["dttm_tz"]]),
      label = "POSIXct"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["POSIXlt"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.POSIXlt(character(0), tz = ctrl_def[["dttm_tz"]]),
      label = "POSIXlt"
    )
  }
)

## bit64 ##
test_that(
  desc = "Zero-length vectors [bit64].",
  code = {
    skip_if_not_installed("bit64")
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["integer64"]], elements = n, ctrl = ctrl_def)
      ),
      expected = bit64::as.integer64(integer(0)),
      label = "integer64"
    )
  }
)

## data.table ##
test_that(
  desc = "Zero-length vectors [data.table].",
  code = {
    skip_if_not_installed("data.table")
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["IDate"]], elements = n, ctrl = ctrl_def)
      ),
      expected = data.table::as.IDate(character(0)),
      label = "IDate"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["ITime"]], elements = n, ctrl = ctrl_def)
      ),
      expected = data.table::as.ITime(character(0)),
      label = "ITime"
    )
  }
); rm(stub_l0)


### Output Classes ###
## Data ##
stub_l1 <- stub(l1, rows = n, method = "empirical", ctrl = ctrl_def)[["vars"]]

## base ##
test_that(
  desc = "Output Classes [base].",
  code = {
    expect_true(
      object = is.character(ble_sample(stub_l1[["character"]], elements = n, ctrl = ctrl_def)),
      label = "character"
    )
    expect_identical(
      object = class(ble_sample(stub_l1[["Date"]], elements = n, ctrl = ctrl_def)),
      expected = "Date",
      label = "Date"
    )
    expect_true(
      object = is.double(ble_sample(stub_l1[["double"]], elements = n, ctrl = ctrl_def)),
      label = "double"
    )
    expect_identical(
      object = class(ble_sample(stub_l1[["factor"]], elements = n, ctrl = ctrl_def)),
      expected = "factor",
      label = "factor"
    )
    expect_identical(
      object = class(ble_sample(stub_l1[["integer"]], elements = n, ctrl = ctrl_def)),
      expected = "integer",
      label = "integer"
    )
    expect_true(
      object = is.logical(ble_sample(stub_l1[["logical"]], elements = n, ctrl = ctrl_def)),
      label = "logical"
    )
    expect_true(
      object = "POSIXct" %in% class(ble_sample(stub_l1[["POSIXct"]], elements = n, ctrl = ctrl_def)),
      label = "POSIXct"
    )
    expect_true(
      object = "POSIXlt" %in% class(ble_sample(stub_l1[["POSIXlt"]], elements = n, ctrl = ctrl_def)),
      label = "POSIXlt"
    )
  }
)

## bit64 ##
test_that(
  desc = "Output Classes [bit64].",
  code = {
    skip_if_not_installed("bit64")
    expect_identical(
      object = class(ble_sample(stub_l1[["integer64"]], elements = n, ctrl = ctrl_def)),
      expected = "integer64",
      label = "integer64")
  }
)

## data.table ##
test_that(
  desc = "Output Classes [data.table].",
  code = {
    skip_if_not_installed("data.table")
    expect_true(
      object = "IDate" %in% class(ble_sample(stub_l1[["IDate"]], elements = n, ctrl = ctrl_def)),
      label = "IDate"
    )
    expect_identical(
      object = class(ble_sample(stub_l1[["ITime"]], elements = n, ctrl = ctrl_def)),
      expected = "ITime",
      label = "ITime"
    )
  }
); rm(stub_l1)


### 'elements' Parameter ###
## Data ##
stub_luniq <- stub(luniq, rows = n, method = "empirical", ctrl = ctrl_def)[["vars"]]

## base ##
test_that(
  desc = "'elements' Parameter [base]",
  code = {
    expect_identical(
      object = lengths(lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[["character"]], ctrl = ctrl_def)),
      expected = seq_len(n),
      label = "character"
    )
    expect_identical(
      object = lengths(lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[["Date"]], ctrl = ctrl_def)),
      expected = seq_len(n),
      label = "Date"
    )
    expect_identical(
      object = lengths(lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[["double"]], ctrl = ctrl_def)),
      expected = seq_len(n),
      label = "double"
    )
    expect_identical(
      object = lengths(lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[["factor"]], ctrl = ctrl_def)),
      expected = seq_len(n),
      label = "factor"
    )
    expect_identical(
      object = lengths(lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[["integer"]], ctrl = ctrl_def)),
      expected = seq_len(n),
      label = "integer"
    )
    expect_identical(
      object = lengths(lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[["logical"]], ctrl = ctrl_def)),
      expected = seq_len(n),
      label = "logical"
    )
    expect_identical(
      object = lengths(lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[["ordered"]], ctrl = ctrl_def)),
      expected = seq_len(n),
      label = "ordered"
    )
    expect_identical(
      object = lengths(lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[["POSIXct"]], ctrl = ctrl_def)),
      expected = seq_len(n),
      label = "POSIXct"
    )
    expect_identical(
      object = lengths(lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[["POSIXlt"]], ctrl = ctrl_def)),
      expected = seq_len(n),
      label = "POSIXlt"
    )
  }
)

## bit64 ##
test_that(
  desc = "'elements' Parameter [bit64]",
  code = {
    skip_if_not_installed("bit64")
    expect_identical(
      object = lengths(lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[["integer64"]], ctrl = ctrl_def)),
      expected = seq_len(n),
      label = "integer64"
    )
  }
)

## data.table ##
test_that(
  desc = "'elements' Parameter [data.table]",
  code = {
    skip_if_not_installed("data.table")
    expect_identical(
      object = lengths(lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[["IDate"]], ctrl = ctrl_def)),
      expected = seq_len(n),
      label = "IDate"
    )
    expect_identical(
      object = lengths(lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[["ITime"]], ctrl = ctrl_def)),
      expected = seq_len(n),
      label = "ITime"
    )
  }
); rm(stub_luniq)


### Tidy Up ###
rm(n, ctrl_def, vars_base, vars_dt)
