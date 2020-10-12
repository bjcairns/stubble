#=======================#
#                       #
#### TEST BLE SPLINE ####
#                       #
#=======================#


### ToDo ###
# - Everything!


### Params ###
## Observations ##
n <- 10L

## Control ##
ctrl_def <- stubble_ctrl(
  index = 1L,
  emp_sw = 0,
  emp_tail_exc = 0,
  emp_fuzz_spl = 0
)
ctrl_def <- ctrl_def[names(ctrl_def) != "index"]

## Vars ##
vars_base <- c("Date", "double", "integer", "POSIXct", "POSIXlt")
vars_bit64 <- c("integer64")
vars_dt <- c("IDate", "ITime")


### Unsupported Vector Classes ###
test_that(
  desc = "Unsupported vector classes.",
  code = {
    expect_warning(
      object = ble_spline(NULL, elements = n, ctrl = ctrl_def),
      regexp = "[nN]o\\s+method\\s+exists"
    )
    expect_identical(
      object = suppressWarnings(
        ble_spline(NULL, elements = n, ctrl = ctrl_def)
      ),
      expected = rep(NA_integer_, n)
    )
  }
)


### Zero-Length Vectors ###
## Data ##
stub_l0 <-suppressWarnings(
  stub(l0, rows = n, method = "empirical", ctrl = ctrl_def)[["vars"]]
)

## base ##
test_that(
  desc = "Zero-length vectors [base].",
  code = {
    expect_equivalent(
      object = suppressWarnings(
        ble_spline(stub_l0[["Date"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.Date(rep(NA, n)),
      label = "Date"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_spline(stub_l0[["double"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.double(rep(NA, n)),
      label = "double"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_spline(stub_l0[["integer"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.integer(rep(NA, n)),
      label = "integer"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_spline(stub_l0[["POSIXct"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.POSIXct(rep(NA_character_, n), tz = ctrl_def[["dttm_tz"]]),
      label = "POSIXct"
    )
    expect_equivalent(
      object = suppressWarnings(
        ble_spline(stub_l0[["POSIXlt"]], elements = n, ctrl = ctrl_def)
      ),
      expected = as.POSIXlt(rep(NA_character_, n), tz = ctrl_def[["dttm_tz"]]),
      label = "POSIXlt"
    )
  }
)

rm(stub_l0)


### Tidy Up ###
rm(n, ctrl_def, vars_base, vars_bit64, vars_dt)
