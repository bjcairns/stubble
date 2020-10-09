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
  emp_n_exc = 0L,
  emp_p_exc = 0,
  emp_fuzz_samp = 0
)
ctrl_def <- ctrl_def[names(ctrl_def) != "index"]


## Vars ##
vars_base <- c("character", "Date", "double", "factor", "integer", "logical", "ordered", "POSIXct", "POSIXlt")
vars_bit64 <- c("integer64")
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
      expected = as.character(rep(NA, n)),
      label = "character"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["Date"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.Date(rep(NA, n)),
      label = "Date"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["double"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.double(rep(NA, n)),
      label = "double"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["factor"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.factor(rep(NA, n)),
      label = "factor"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["integer"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.integer(rep(NA, n)),
      label = "integer"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["logical"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.logical(rep(NA, n)),
      label = "logical"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["ordered"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.ordered(rep(NA, n)),
      label = "ordered"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["POSIXct"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.POSIXct(rep(NA_character_, n), tz = ctrl_def[["dttm_tz"]]),
      label = "POSIXct"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["POSIXlt"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.POSIXlt(rep(NA_character_, n), tz = ctrl_def[["dttm_tz"]]),
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
      expected = bit64::as.integer64(rep(NA, n)),
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
      expected = data.table::as.IDate(rep(NA_character_, n)),
      label = "IDate"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_sample(stub_l0[["ITime"]], elements = n, ctrl = ctrl_def)
      ),
      expected = data.table::as.ITime(rep(NA_character_, n)),
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


### 'elements' Parameter ###
## Data ##
ctrl_def_uniq <- ctrl_def
ctrl_def_uniq$emp_p_exc <- 0.06
stub_luniq <- suppressWarnings(stub(luniq, rows = n, method = "empirical", ctrl = ctrl_def_uniq)[["vars"]])

## base ##
test_that(
  desc = "'elements' Parameter [base]",
  code = {
    for (col_type in vars_base) {
      syn_cols <- lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[[col_type]], ctrl = ctrl_def_uniq)
      expect_identical(
        object = lengths(syn_cols),
        expected = seq_len(n),
        label = col_type
      )
    }
  }
)

## bit64 ##
test_that(
  desc = "'elements' Parameter [bit64]",
  code = {
    skip_if_not_installed("bit64")
    for (col_type in vars_bit64) {
      syn_cols <- lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[[col_type]], ctrl = ctrl_def_uniq)
      expect_identical(
        object = lengths(syn_cols),
        expected = seq_len(n),
        label = col_type
      )
    }
  }
)

## data.table ##
test_that(
  desc = "'elements' Parameter [data.table]",
  code = {
    skip_if_not_installed("data.table")
    for (col_type in vars_dt) {
      syn_cols <- lapply(X = seq_len(n), FUN = ble_sample, x = stub_luniq[[col_type]], ctrl = ctrl_def_uniq)
      expect_identical(
        object = lengths(syn_cols),
        expected = seq_len(n),
        label = col_type
      )
    }
  }
); rm(stub_luniq)


### Tidy Up ###
rm(n, ctrl_def, vars_base, vars_dt)
