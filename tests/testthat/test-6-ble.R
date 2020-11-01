#================#
#                #
#### TEST BLE ####
#                #
#================#


### ToDo ###
# - Zero-Length outputs.


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

## Misc ##
n <- 10L


### Test Objects ###
stub_l1 <- stub(x = l1, method = "agnostic", ctrl = ctrl_def)
stub_dat1 <- stub(x = dat1, method = "agnostic", ctrl = ctrl_def)
if (getOption("stubble_has_data.table")) {
  stub_dt1 <- stub(x = dt1, method = "agnostic", ctrl = ctrl_def)
}
if (getOption("stubble_has_tibble")) {
  stub_df1 <- stub(x = df1, method = "agnostic", ctrl = ctrl_def)
}


### Output Class ###
## base ##
test_that(
  desc = "Output Class [base]",
  code = {
    expect_identical(
      object = class(ble(stb = stub_l1)),
      expected = class(l1),
      label = "list"
    )
    expect_identical(
      object = class(ble(stb = stub_dat1)),
      expected = class(dat1),
      label = "data.frame"
    )
  }
)

## data.table ##
test_that(
  desc = "Output Class [data.table]",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_identical(
      object = class(ble(stb = stub_dt1)),
      expected = class(dt1),
      label = "data.table"
    )
  }
)

## tibble ##
test_that(
  desc = "Output Class [tibble]",
  code = {
    skip_if_not_installed("tibble", min_v_tibble)
    expect_identical(
      object = class(ble(stb = stub_df1)),
      expected = class(df1),
      label = "tibble"
    )
  }
)


### Output Names ###
## base ##
test_that(
  desc = "Output Names [base]",
  code = {
    expect_identical(
      object = names(ble(stb = stub_l1)),
      expected = names(l1),
      label = "list"
    )
    expect_identical(
      object = names(ble(stb = stub_dat1)),
      expected = names(dat1),
      label = "data.frame"
    )
  }
)

## data.table ##
test_that(
  desc = "Output Names [data.table]",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_identical(
      object = names(ble(stb = stub_dt1)),
      expected = names(dt1),
      label = "data.table"
    )
  }
)

## tibble ##
test_that(
  desc = "Output Names [tibble]",
  code = {
    skip_if_not_installed("tibble", min_v_tibble)
    expect_identical(
      object = names(ble(stb = stub_df1)),
      expected = names(df1),
      label = "tibble"
    )
  }
)


### Output Dimensions ###
## base ##
test_that(
  desc = "Output Length [base]",
  code = {
    expect_length(
      object = ble(stb = stub_l1),
      n = length(l1)
    )
    expect_length(
      object = ble(stb = stub_dat1),
      n = length(dat1)
    )
  }
)

## data.table ##
test_that(
  desc = "Output Length [data.table]",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_length(
      object = ble(stb = stub_dt1),
      n = length(dt1)
    )
  }
)

## tibble ##
test_that(
  desc = "Output Length [tibble]",
  code = {
    skip_if_not_installed("tibble", min_v_tibble)
    expect_length(
      object = ble(stb = stub_df1),
      n = length(df1)
    )
  }
)


### Output Rows ###
## base ##
test_that(
  desc = "Output Rows [base]",
  code = {
    expect_equivalent(
      object = lengths(ble(stb = stub_l1, rows = n)),
      expected = rep(n, length(l1)),
      label = "Same length vectors (list)"
    )
    expect_equivalent(
      object = lengths(ble(stb = stub_l1, rows = seq_len(length(l1)))),
      expected = seq_len(length(l1)),
      label = "Different length vectors (list)"
    )
    expect_identical(
      object = nrow(ble(stb = stub_dat1, rows = n)),
      expected = n,
      label = "data.frame"
    )
  }
)

## data.table ##
test_that(
  desc = "Output Rows [data.table]",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_identical(
      object = nrow(ble(stb = stub_dt1, rows = n)),
      expected = n,
      label = "data.table"
    )
  }
)

## tibble ##
test_that(
  desc = "Output Rows [tibble]",
  code = {
    skip_if_not_installed("tibble", min_v_tibble)
    expect_identical(
      object = nrow(ble(stb = stub_df1, rows = n)),
      expected = n,
      label = "tibble"
    )
  }
)


### Tidy Up ###
rm(ctrl_def, n, stub_l1, stub_dat1, stub_dt1, stub_df1)
