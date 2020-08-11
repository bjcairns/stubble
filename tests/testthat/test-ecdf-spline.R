#========================#
#                        #
#### TEST ECDF SPLINE ####
#                        #
#========================#


### ToDo ###
# - Assess the 'tail_exc' control parameter?
# - Tests for ecdf_spline_().


### Control Parameters ###
old_ctrl <- list(
  dttm_tz = "UTC",
  tail_exc = 0.025,
  fuzz_ecdf = TRUE,
)
ctrl <- gen_attr_ctrl(old_ctrl = old_ctrl, index = 1L)


### Unsupported Vector Classes ###
v <- complex(0)

test_that(
  desc = "Unsupported output classes.",
  code = {
    expect_warning(
      object = ecdf_spline(v, ctrl = ctrl),
      regexp = "[nN]o\\s+method\\s+exists"
    )
    expect_true(
      object = suppressWarnings(
        is.list(ecdf_spline(v, ctrl = ctrl))
      )
    )
  }
); rm(v)


### 0-Length Vectors ###
l <- list(
  Date = as.Date(character(0)),
  double = double(0),
  IDate = if (is.installed.package("data.table")) data.table::as.IDate(character(0)) else NULL,
  integer = integer(0),
  integer64 = if (is.installed.package("bit64")) bit64::integer64(0) else NULL,
  POSIXct = as.POSIXct(character(0), tz = ctrl[["dttm_tz"]]),
  POSIXlt = as.POSIXlt(character(0), tz = ctrl[["dttm_tz"]])
)

test_that(
  desc = "0-length vectors.",
  code = {
    expect_true(
      object = is.list(ecdf_spline(l[["Date"]], ctrl = ctrl)),
      label = "Date"
    )
    expect_true(
      object = is.list(ecdf_spline(l[["double"]], ctrl = ctrl)),
      label = "double"
    )
    expect_true(
      object = is.list(ecdf_spline(l[["integer"]], ctrl = ctrl)),
      label = "integer"
    )
    expect_true(
      object = is.list(ecdf_spline(l[["POSIXct"]], ctrl = ctrl)),
      label = "POSIXct"
    )
    expect_true(
      object = is.list(ecdf_spline(l[["POSIXlt"]], ctrl = ctrl)),
      label = "POSIXlt"
    )
  }
)

test_that(
  desc = "0-length vectors (bit64).",
  code = {
    skip_if_not(is.installed.package("bit64"))
    expect_true(
      object = is.list(ecdf_spline(l[["integer64"]], ctrl = ctrl)),
      label = "integer64"
    )
  }
)

test_that(
  desc = "0-length vectors (data.table).",
  code = {
    skip_if_not(is.installed.package("data.table"))
    expect_true(
      object = is.list(ecdf_spline(l[["IDate"]], ctrl = ctrl)),
      label = "IDate"
    )
  }
)

rm(l)


### Tidy Up ###
rm(ctrl)
