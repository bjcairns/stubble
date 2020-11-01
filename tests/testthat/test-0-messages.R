#=====================#
#                     #
#### TEST MESSAGES ####
#                     #
#=====================#


### Coercion Warnings ###
## No Method #
test_that(
  desc = "Coercion Warnings [no method]",
  code = {
    expect_error(
      object = .warning_coercion(NULL),
      regexp = "[nN]o\\s+applicable\\s+method.+NULL"
    )
  }
)

## bit64 ##
test_that(
  desc = "Coercion Warnings [bit64]",
  code = {
    skip_if_not_installed("bit64", min_v_bit64)
    expect_warning(
      object = .warning_coercion(l0[["integer64"]]),
      regexp = "integer64.+coerced.+double",
      label = "integer64"
    )
  }
)

## data.table ##
test_that(
  desc = "Coercion Warnings [data.table]",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_warning(
      object = .warning_coercion(data.table::data.table()),
      regexp = "data\\.table.+coerced.+data\\.frame",
      label = "data.table"
    )
    expect_warning(
      object = .warning_coercion(l0[["IDate"]]),
      regexp = "IDate.+coerced.+Date",
      label = "IDate"
    )
    expect_warning(
      object = .warning_coercion(l0[["ITime"]]),
      regexp = "ITime.+coerced.+POSIXct",
      label = "ITime"
    )
  }
)

## tibble ##
test_that(
  desc = "Coercion Warnings [tibble]",
  code = {
    skip_if_not_installed("tibble", min_v_tibble)
    expect_warning(
      object = .warning_coercion(tibble::tibble()),
      regexp = "tbl_df.+coerced.+data\\.frame",
      label = "tbl_df"
    )
  }
)


### No Method Warnings ###
test_that(
  desc = "No Method Warnings",
  code = {
    expect_warning(
      object = .warning_no_method(l0[["character"]]),
      regexp = "No\\smethod.+character"
    )
    expect_warning(
      object = .warning_no_method(l0[["Date"]]),
      regexp = "No\\smethod.+Date"
    )
    expect_warning(
      object = .warning_no_method(l0[["double"]]),
      regexp = "No\\smethod.+numeric"
    )
    expect_warning(
      object = .warning_no_method(l0[["factor"]]),
      regexp = "No\\smethod.+factor"
    )
    expect_warning(
      object = .warning_no_method(l0[["IDate"]]),
      regexp = "No\\smethod.+IDate"
    )
    expect_warning(
      object = .warning_no_method(l0[["integer"]]),
      regexp = "No\\smethod.+integer"
    )
    expect_warning(
      object = .warning_no_method(l0[["integer64"]]),
      regexp = "No\\smethod.+integer64"
    )
    expect_warning(
      object = .warning_no_method(l0[["ITime"]]),
      regexp = "No\\smethod.+ITime"
    )
    expect_warning(
      object = .warning_no_method(l0[["logical"]]),
      regexp = "No\\smethod.+logical"
    )
    expect_warning(
      object = .warning_no_method(l0[["ordered"]]),
      regexp = "No\\smethod.+ordered"
    )
    expect_warning(
      object = .warning_no_method(l0[["POSIXct"]]),
      regexp = "No\\smethod.+POSIXct"
    )
    expect_warning(
      object = .warning_no_method(l0[["POSIXlt"]]),
      regexp = "No\\smethod.+POSIXlt"
    )
  }
)


### No Method Errors ###
test_that(
  desc = "No Method Errors",
  code = {
    expect_error(
      object = .stop_no_method(l0[["POSIXct"]]),
      regexp = "No\\smethod.+POSIXct"
    )
  }
)
