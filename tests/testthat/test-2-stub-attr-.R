#=======================#
#                       #
#### TEST STUB_ATTR_ ####
#                       #
#=======================#


### Notes ###
# - emp_sw <- 0 - always "spline"
# - emp_sw <- 1 - always "sample"


### ToDo ###
# - Add more emp_sw tests for the new control flow in stub_attr__() for emp_sw
#   equal to 0 or 1.


### Params ###
## Control ##
ctrl_def <- stubble_ctrl(index = 1L)[-1]

## Vars ##
vars_base <- c("character", "Date", "double", "factor", "integer", "logical", "ordered", "POSIXct", "POSIXlt")
vars_dt <- c("IDate", "ITime")


### Unsupported Vector Classes ###
test_that(
  desc = "Unsupported vector classes.",
  code = {
    expect_warning(
      object = stub_attr_(NULL, ctrl = ctrl_def),
      regexp = "[nN]o\\s+method\\s+exists"
    )
    expect_identical(
      object = suppressWarnings(
        stub_attr_(NULL, ctrl = ctrl_def)
      ),
      expected = "sample"
    )
  }
)


### Zero-Length Vectors ###
## base ##
test_that(
  desc = "Zero-length vectors [base].",
  code = {
    expect_equivalent(
      object = sapply(X = l0[vars_base], FUN = stub_attr_, ctrl = ctrl_def),
      expected = rep("sample", length(l0[vars_base]))
    )
  }
)

## bit64 ##
test_that(
  desc = "Zero-length vectors [bit64].",
  code = {
    skip_if_not_installed("bit64", min_v_bit64)
    expect_identical(
      object = stub_attr_(l0[["integer64"]], ctrl = ctrl_def),
      expected = "sample"
    )
  }
)

## data.table ##
test_that(
  desc = "Zero-length vectors [data.table].",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_equivalent(
      object = sapply(X = l0[vars_dt], FUN = stub_attr_, ctrl = ctrl_def),
      expected = rep("sample", length(l0[vars_dt]))
    )
  }
)


### Missing Data Vectors ###
## base ##
test_that(
  desc = "Missing data vectors [base].",
  code = {
    expect_equivalent(
      object = sapply(X = lna[vars_base], FUN = stub_attr_, ctrl = ctrl_def),
      expected = rep("sample", length(lna[vars_base]))
    )
  }
)

## bit64 ##
test_that(
  desc = "Missing data vectors [bit64].",
  code = {
    skip_if_not_installed("bit64", min_v_bit64)
    expect_identical(
      object = stub_attr_(lna[["integer64"]], ctrl = ctrl_def),
      expected = "sample"
    )
  }
)

## IDate ##
test_that(
  desc = "Missing data vectors [data.table].",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_equivalent(
      object = sapply(X = lna[vars_dt], FUN = stub_attr_, ctrl = ctrl_def),
      expected = rep("sample", length(lna[vars_dt]))
    )
  }
)


### Class-Associated Methods (Always 'sample') ###
## Control ##
ctrl <- stubble_ctrl(old_ctrl = list(emp_sw = 0), index = 1L)[-1]

## Vars ##
vars <- c("character", "factor", "logical", "ordered")

## Test ##
test_that(
  desc = "Class-associated methods.",
  code = {
    expect_equivalent(
      object = sapply(X = luniq[vars], FUN = stub_attr_, ctrl = ctrl),
      expected = rep("sample", length(luniq[vars]))
    )
  }
); rm(ctrl, vars)


### Empirical Switch (Sample) ###
## Control ##
ctrl <- stubble_ctrl(old_ctrl = list(emp_sw = 1), index = 1L)[-1]

## Vars ##
vars <- c("Date", "double", "integer", "POSIXct", "POSIXlt")

## Test (base) ##
test_that(
  desc = "Empirical switch control parameter (always sample) [base]",
  code = {
    expect_equivalent(
      object = sapply(X = luniq[vars], FUN = stub_attr_, ctrl = ctrl),
      expected = rep("sample", length(luniq[vars]))
    )
  }
)

## Test (bit64) ##
test_that(
  desc = "Empirical switch control parameter (always sample) [bit64]",
  code = {
    skip_if_not_installed("bit64", min_v_bit64)
    expect_identical(
      object = stub_attr_(luniq[["integer64"]], ctrl = ctrl),
      expected = "sample"
    )
  }
)

## Test (data.table) ##
test_that(
  desc = "Empirical switch control parameter (always sample) [data.table]",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_equivalent(
      object = sapply(X = luniq[vars_dt], FUN = stub_attr_, ctrl = ctrl),
      expected = rep("sample", length(luniq[vars_dt]))
    )
  }
)

## Tidy Up ##
rm(ctrl, vars)


## Empirical Switch (Spline) ##
## Control ##
ctrl <- stubble_ctrl(old_ctrl = list(emp_sw = 0), index = 1L)[-1]

## Vars ##
vars <- c("Date", "double", "integer", "POSIXct", "POSIXlt")

## Test (base) ##
test_that(
  desc = "Empirical switch control parameter (always spline) [base]",
  code = {
    expect_equivalent(
      object = sapply(X = l1[vars], FUN = stub_attr_, ctrl = ctrl),
      expected = rep("spline", length(l1[vars]))
    )
  }
)

## Test (bit64) ##
test_that(
  desc = "Empirical switch control parameter (always spline) [bit64]",
  code = {
    skip_if_not_installed("bit64", min_v_bit64)
    expect_identical(
      object = stub_attr_(l1[["integer64"]], ctrl = ctrl),
      expected = "spline"
    )
  }
)

## Test (data.table) ##
test_that(
  desc = "Empirical switch control parameter (always spline) [data.table]",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_equivalent(
      object = sapply(X = l1[vars_dt], FUN = stub_attr_, ctrl = ctrl),
      expected = rep("spline", length(l1[vars_dt]))
    )
  }
)

## Tidy Up ##
rm(ctrl, vars)


### stub_attr__() Output ###
test_that(
  desc = "stub_attr__() output.",
  code = {
    expect_type(
      object = stub_attr__(NULL, ctrl = ctrl_def),
      type = "character"
    )
    expect_length(
      object = stub_attr__(NULL, ctrl = ctrl_def),
      n = 1L
    )
  }
)


### Tidy Up ###
rm(ctrl_def, vars_base, vars_dt)
