#========================#
#                        #
#### TEST ECDF METHOD ####
#                        #
#========================#


### Notes ###
# - emp_sw <- 0 - always "spline"
# - emp_sw <- 1 - always "sample"


### Params ###
## Control ##
ctrl_def <- gen_stubble_ctrl(index = 1L)[-1]

## Vars ##
vars_base <- c("character", "Date", "double", "factor", "integer", "logical", "ordered", "POSIXct", "POSIXlt")


### Unsupported Vector Classes ###
test_that(
  desc = "Unsupported vector classes.",
  code = {
    expect_warning(
      object = ecdf_method(NULL, ctrl = ctrl_def),
      regexp = "[nN]o\\s+method\\s+exists"
    )
    expect_identical(
      object = suppressWarnings(
        ecdf_method(NULL, ctrl = ctrl_def)
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
      object = sapply(X = l0[vars_base], FUN = ecdf_method, ctrl = ctrl_def),
      expected = rep("sample", length(l0[vars_base]))
    )
  }
)

## bit64 ##
test_that(
  desc = "Zero-length vectors [bit64].",
  code = {
    skip_if_not(is.installed.package("bit64"))
    expect_identical(
      object = ecdf_method(l0[["integer64"]], ctrl = ctrl_def),
      expected = "sample"
    )
  }
)

## IDate ##
test_that(
  desc = "Zero-length vectors [data.table].",
  code = {
    skip_if_not(is.installed.package("data.table"))
    expect_identical(
      object = ecdf_method(l0[["IDate"]], ctrl = ctrl_def),
      expected = "sample"
    )
  }
)


### Missing Data Vectors ###
## base ##
test_that(
  desc = "Missing data vectors [base].",
  code = {
    expect_equivalent(
      object = sapply(X = lna[vars_base], FUN = ecdf_method, ctrl = ctrl_def),
      expected = rep("sample", length(lna[vars_base]))
    )
  }
)

## bit64 ##
test_that(
  desc = "Missing data vectors [bit64].",
  code = {
    skip_if_not(is.installed.package("bit64"))
    expect_identical(
      object = ecdf_method(lna[["integer64"]], ctrl = ctrl_def),
      expected = "sample"
    )
  }
)

## IDate ##
test_that(
  desc = "Missing data vectors [data.table].",
  code = {
    skip_if_not(is.installed.package("data.table"))
    expect_identical(
      object = ecdf_method(lna[["IDate"]], ctrl = ctrl_def),
      expected = "sample"
    )
  }
)


### Class-Associated Methods (Always 'sample') ###
## Control ##
ctrl <- gen_stubble_ctrl(old_ctrl = list(emp_sw = 0), index = 1L)[-1]

## Vars ##
vars <- c("character", "factor", "logical", "ordered")

## Test ##
test_that(
  desc = "Class-associated methods.",
  code = {
    expect_equivalent(
      object = sapply(X = luniq[vars], FUN = ecdf_method, ctrl = ctrl),
      expected = rep("sample", length(luniq[vars]))
    )
  }
); rm(ctrl, vars)


### Empirical Switch (Sample) ###
## Control ##
ctrl <- gen_stubble_ctrl(old_ctrl = list(emp_sw = 1), index = 1L)[-1]

## Vars ##
vars <- c("Date", "double", "integer", "POSIXct", "POSIXlt")

## Test (base) ##
test_that(
  desc = "Empirical switch control parameter (always sample) [base]",
  code = {
    expect_equivalent(
      object = sapply(X = luniq[vars], FUN = ecdf_method, ctrl = ctrl),
      expected = rep("sample", length(luniq[vars]))
    )
  }
)

## Test (bit64) ##
test_that(
  desc = "Empirical switch control parameter (always sample) [bit64]",
  code = {
    skip_if_not(is.installed.package("bit64"))
    expect_identical(
      object = ecdf_method(luniq[["integer64"]], ctrl = ctrl),
      expected = "sample"
    )
  }
)

## Test (data.table) ##
test_that(
  desc = "Empirical switch control parameter (always sample) [data.table]",
  code = {
    skip_if_not(is.installed.package("data.table"))
    expect_identical(
      object = ecdf_method(luniq[["IDate"]], ctrl = ctrl),
      expected = "sample"
    )
  }
)

## Tidy Up ##
rm(ctrl, vars)


## Empirical Switch (Spline) ##
## Control ##
ctrl <- gen_stubble_ctrl(old_ctrl = list(emp_sw = 0), index = 1L)[-1]

## Vars ##
vars <- c("Date", "double", "integer", "POSIXct", "POSIXlt")

## Test (base) ##
test_that(
  desc = "Empirical switch control parameter (always spline) [base]",
  code = {
    expect_equivalent(
      object = sapply(X = l1[vars], FUN = ecdf_method, ctrl = ctrl),
      expected = rep("spline", length(l1[vars]))
    )
  }
)

## Test (bit64) ##
test_that(
  desc = "Empirical switch control parameter (always spline) [bit64]",
  code = {
    skip_if_not(is.installed.package("bit64"))
    expect_identical(
      object = ecdf_method(l1[["integer64"]], ctrl = ctrl),
      expected = "spline"
    )
  }
)

## Test (data.table) ##
test_that(
  desc = "Empirical switch control parameter (always spline) [data.table]",
  code = {
    skip_if_not(is.installed.package("data.table"))
    expect_identical(
      object = ecdf_method(l1[["IDate"]], ctrl = ctrl),
      expected = "spline"
    )
  }
)

## Tidy Up ##
rm(ctrl, vars)


### ecdf_method_() Output ###
test_that(
  desc = "ecdf_method_() output.",
  code = {
    expect_type(
      object = ecdf_method_(NULL, ctrl = ctrl_def),
      type = "character"
    )
    expect_length(
      object = ecdf_method_(NULL, ctrl = ctrl_def),
      n = 1L
    )
  }
)


### Tidy Up ###
rm(ctrl_def, vars_base)
