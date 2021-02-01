#================#
#                #
#### TEST BLE ####
#                #
#================#


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

### Vars ###
vars_base <- c("character", "Date", "double", "factor", "integer", "logical", "ordered", "POSIXct", "POSIXlt")

### Test Objects ###
stub_l1 <- stub(x = l1[vars_base], method = "agnostic", ctrl = ctrl_def)
stub_dat1 <- stub(x = dat1[, vars_base], method = "agnostic", ctrl = ctrl_def)
if (getOption("stubble_has_data.table")) {
  stub_dt1 <- stub(x = dt1[, vars_base, with = FALSE], method = "agnostic", ctrl = ctrl_def)
}
if (getOption("stubble_has_tibble")) {
  stub_df1 <- stub(x = df1[, vars_base], method = "agnostic", ctrl = ctrl_def)
}


### Output Class ###
## base ##
test_that(
  desc = "Output Class [base].",
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
  desc = "Output Class [data.table].",
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
  desc = "Output Class [tibble].",
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
test_that(
  desc = "Output Names [base].",
  code = {
    expect_identical(
      object = names(ble(stb = stub_l1)),
      expected = names(l1[vars_base]),
      label = "list"
    )
    expect_identical(
      object = names(ble(stb = stub_dat1)),
      expected = names(dat1[, vars_base]),
      label = "data.frame"
    )
  }
)


### Output Columns ###
test_that(
  desc = "Output Length [base].",
  code = {
    expect_length(
      object = ble(stb = stub_l1),
      n = length(l1[vars_base])
    )
    expect_length(
      object = ble(stb = stub_dat1),
      n = length(dat1[vars_base])
    )
  }
)


### Output Rows ###
test_that(
  desc = "Output Rows [base].",
  code = {
    expect_equivalent(
      object = lengths(ble(stb = stub_l1, rows = 0L)),
      expected = rep(0L, length(l1[vars_base])),
      label = "Zero-length output"
    )
    expect_equivalent(
      object = lengths(ble(stb = stub_l1, rows = 10L)),
      expected = rep(10L, length(l1[vars_base])),
      label = "Same length vectors (list)"
    )
    expect_equivalent(
      object = lengths(ble(stb = stub_l1, rows = seq_len(length(l1[vars_base])))),
      expected = seq_len(length(l1[vars_base])),
      label = "Different length vectors (list)"
    )
  }
)


### Tidy Up ###
rm(ctrl_def, vars_base, stub_l1, stub_dat1)
if (getOption("stubble_has_data.table")) {
  rm(stub_dt1)
}
if (getOption("stubble_has_tibble")) {
  rm(stub_df1)
}
