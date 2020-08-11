#========================#
#                        #
#### TEST ECDF SAMPLE ####
#                        #
#========================#


### ToDo ###
# - Assess the 'n_exc' & 'p_exc' control parameters.
# - Tests for ecdf_sample_().


### Control Parameters ###
old_ctrl <- list(
  n_exc = 0,
  p_exc = 0,
  fuzz_samp = FALSE,
  dttm_tz = "UTC"
)
ctrl <- gen_attr_ctrl(old_ctrl = old_ctrl, index = 1L)


### Unsupported Vector Classes ###
v <- complex(0)

test_that(
  desc = "Unsupported output classes.",
  code = {
    expect_warning(
      object = ecdf_sample(v, ctrl = ctrl),
      regexp = "[nN]o\\s+method\\s+exists"
    )
    expect_true(
      object = suppressWarnings(
        is.list(ecdf_sample(v, ctrl = ctrl))
      )
    )
  }
); rm(v)


### 0-Length Vectors ###
l <- list(
  bit = if (is.installed.package("bit")) bit = bit::bit(0) else NULL,
  character = character(0),
  Date = as.Date(character(0)),
  double = double(0),
  factor = factor(character(0)),
  IDate = if (is.installed.package("data.table")) data.table::as.IDate(character(0)) else NULL,
  integer = integer(0),
  integer64 = if (is.installed.package("bit64")) bit64::integer64(0) else NULL,
  logical = logical(0),
  ordered = ordered(character(0)),
  POSIXct = as.POSIXct(character(0), tz = ctrl[["dttm_tz"]]),
  POSIXlt = as.POSIXlt(character(0), tz = ctrl[["dttm_tz"]])
)

test_that(
  desc = "0-length vectors.",
  code = {
    expect_true(
      object = is.list(ecdf_sample(l[["character"]], ctrl = ctrl)),
      label = "character"
    )
    expect_true(
      object = is.list(ecdf_sample(l[["Date"]], ctrl = ctrl)),
      label = "Date"
    )
    expect_true(
      object = is.list(ecdf_sample(l[["double"]], ctrl = ctrl)),
      label = "double"
    )
    expect_true(
      object = is.list(ecdf_sample(l[["factor"]], ctrl = ctrl)),
      label = "factor"
    )
    expect_true(
      object = is.list(ecdf_sample(l[["integer"]], ctrl = ctrl)),
      label = "integer"
    )
    expect_true(
      object = is.list(ecdf_sample(l[["logical"]], ctrl = ctrl)),
      label = "logical"
    )
    expect_true(
      object = is.list(ecdf_sample(l[["ordered"]], ctrl = ctrl)),
      label = "ordered"
    )
    expect_true(
      object = is.list(ecdf_sample(l[["POSIXct"]], ctrl = ctrl)),
      label = "POSIXct"
    )
    expect_true(
      object = is.list(ecdf_sample(l[["POSIXlt"]], ctrl = ctrl)),
      label = "POSIXlt"
    )
  }
)

test_that(
  desc = "0-length vectors (bit).",
  code = {
    skip_if_not(is.installed.package("bit"))
    expect_true(
      object = is.list(ecdf_sample(l[["bit"]], ctrl = ctrl)),
      label = "bit"
    )
  }
)

test_that(
  desc = "0-length vectors (bit64).",
  code = {
    skip_if_not(is.installed.package("bit64"))
    expect_true(
      object = is.list(ecdf_sample(l[["integer64"]], ctrl = ctrl)),
      label = "integer64"
    )
  }
)

test_that(
  desc = "0-length vectors (data.table).",
  code = {
    skip_if_not(is.installed.package("data.table"))
    expect_true(
      object = is.list(ecdf_sample(l[["IDate"]], ctrl = ctrl)),
      label = "IDate"
    )
  }
)

rm(l)


### Output Classes ###
l <- list(
  bit = if (is.installed.package("bit")) bit::as.bit(F) else NULL,
  character = "",
  Date = as.Date("1970-01-01"),
  double = 0,
  factor = factor(""),
  IDate = if (is.installed.package("data.table")) data.table::as.IDate("1970-01-01") else NULL,
  integer = 0L,
  integer64 = if (is.installed.package("bit64")) bit64::as.integer64(0) else NULL,
  logical = F,
  ordered = ordered(""),
  POSIXct = as.POSIXct("1970-01-01", tz = ctrl[["dttm_tz"]]),
  POSIXlt = as.POSIXlt("1970-01-01", tz = ctrl[["dttm_tz"]])
)

test_that(
  desc = "Output Classes (base).",
  code = {
    expect_identical(
      object = class(ecdf_sample(l[["character"]], ctrl = ctrl)[["values"]]),
      expected = "character",
      label = "character"
    )
    expect_identical(
      object = class(ecdf_sample(l[["Date"]], ctrl = ctrl)[["values"]]),
      expected = "Date",
      label = "Date"
    )
    expect_type(
      object = ecdf_sample(l[["double"]], ctrl = ctrl)[["values"]],
      type = "double"
    )
    expect_identical(
      object = class(ecdf_sample(l[["factor"]], ctrl = ctrl)[["values"]]),
      expected = "factor",
      label = "factor"
    )
    expect_identical(
      object = class(ecdf_sample(l[["integer"]], ctrl = ctrl)[["values"]]),
      expected = "integer",
      label = "integer"
    )
    expect_identical(
      object = class(ecdf_sample(l[["logical"]], ctrl = ctrl)[["values"]]),
      expected = "logical",
      label = "logical"
    )
    expect_identical(
      object = class(ecdf_sample(l[["ordered"]], ctrl = ctrl)[["values"]]),
      expected = c("ordered", "factor"),
      label = "ordered"
    )
    expect_identical(
      object = class(ecdf_sample(l[["POSIXct"]], ctrl = ctrl)[["values"]]),
      expected = c("POSIXct", "POSIXt"),
      label = "POSIXct"
    )
    expect_identical(
      object = class(ecdf_sample(l[["POSIXlt"]], ctrl = ctrl)[["values"]]),
      expected = c("POSIXlt", "POSIXt"),
      label = "POSIXlt"
    )
  }
)

test_that(
  desc = "Output Classes (bit).",
  code = {
    skip_if_not(is.installed.package("bit"))
    expect_identical(
      object = class(ecdf_sample(l[["bit"]], ctrl = ctrl)[["values"]]),
      expected = c("booltype", "bit"),
      label = "bit"
    )
  }
)

test_that(
  desc = "Output Classes (bit64).",
  code = {
    skip_if_not(is.installed.package("bit64"))
    expect_identical(
      object = class(ecdf_sample(l[["integer64"]], ctrl = ctrl)[["values"]]),
      expected = "integer64",
      label = "integer64")
  }
)

test_that(
  desc = "Output Classes (data.table).",
  code = {
    skip_if_not(is.installed.package("data.table"))
    expect_identical(
      object = class(ecdf_sample(l[["IDate"]], ctrl = ctrl)[["values"]]),
      expected = c("IDate", "Date"),
      label = "IDate"
    )
  }
)

rm(l)


### Category Exclusions ('n_exc' & 'p_exc') ###
v <- c(NA, 1L)
l <- list(
  m00 = rep(v, times = c(0, 10)),
  m01 = rep(v, times = c(1, 9)),
  m02 = rep(v, times = c(2, 8)),
  m03 = rep(v, times = c(3, 7)),
  m04 = rep(v, times = c(4, 6)),
  m05 = rep(v, times = c(5, 5)),
  m06 = rep(v, times = c(6, 4)),
  m07 = rep(v, times = c(7, 3)),
  m08 = rep(v, times = c(8, 2)),
  mo9 = rep(v, times = c(9, 1)),
  m10 = rep(v, times = c(10, 0))
); rm(v)


### Tidy Up ###
rm(ctrl)
