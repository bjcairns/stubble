#========================#
#                        #
#### TEST ECDF SAMPLE ####
#                        #
#========================#


### ToDo ###
# - Assess the 'n_exc' & 'p_exc' control parameters.


### Control Parameters ###
ctrl <- list(
  n_exc = 0,
  p_exc = 0,
  fuzz_samp = FALSE,
  dttm_tz = "UTC"
)


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
  bit = bit::bit(0),
  character = character(0),
  Date = as.Date(character(0)),
  double = double(0),
  factor = factor(character(0)),
  IDate = data.table::as.IDate(character(0)),
  integer = integer(0),
  integer64 = bit64::integer64(0),
  logical = logical(0),
  ordered = ordered(character(0)),
  POSIXct = as.POSIXct(character(0), tz = ctrl[["dttm_tz"]]),
  POSIXlt = as.POSIXlt(character(0), tz = ctrl[["dttm_tz"]])
)

test_that(
  desc = "0-length vectors.",
  code = {
    expect_true(is.list(ecdf_sample(l[["bit"]], ctrl = ctrl)))
    expect_true(is.list(ecdf_sample(l[["character"]], ctrl = ctrl)))
    expect_true(is.list(ecdf_sample(l[["Date"]], ctrl = ctrl)))
    expect_true(is.list(ecdf_sample(l[["double"]], ctrl = ctrl)))
    expect_true(is.list(ecdf_sample(l[["factor"]], ctrl = ctrl)))
    expect_true(is.list(ecdf_sample(l[["IDate"]], ctrl = ctrl)))
    expect_true(is.list(ecdf_sample(l[["integer"]], ctrl = ctrl)))
    expect_true(is.list(ecdf_sample(l[["integer64"]], ctrl = ctrl)))
    expect_true(is.list(ecdf_sample(l[["logical"]], ctrl = ctrl)))
    expect_true(is.list(ecdf_sample(l[["ordered"]], ctrl = ctrl)))
    expect_true(is.list(ecdf_sample(l[["POSIXct"]], ctrl = ctrl)))
    expect_true(is.list(ecdf_sample(l[["POSIXlt"]], ctrl = ctrl)))
  }
); rm(l)


### Output Classes ###
l <- list(
  bit = bit::as.bit(F),
  character = "",
  Date = as.Date("1970-01-01"),
  double = 0,
  factor = factor(""),
  IDate = data.table::as.IDate("1970-01-01"),
  integer = 0L,
  integer64 = bit64::as.integer64(0),
  logical = F,
  ordered = ordered(""),
  POSIXct = as.POSIXct("1970-01-01", tz = ctrl[["dttm_tz"]]),
  POSIXlt = as.POSIXlt("1970-01-01", tz = ctrl[["dttm_tz"]])
)

test_that(
  desc = "Output Classes",
  code = {
    expect_identical(class(ecdf_sample(l[["bit"]], ctrl = ctrl)[["values"]]), "bit")
    expect_identical(class(ecdf_sample(l[["character"]], ctrl = ctrl)[["values"]]), "character")
    expect_identical(class(ecdf_sample(l[["Date"]], ctrl = ctrl)[["values"]]), "Date")
    expect_type(ecdf_sample(l[["double"]], ctrl = ctrl)[["values"]], "double")
    expect_identical(class(ecdf_sample(l[["factor"]], ctrl = ctrl)[["values"]]), "factor")
    expect_identical(class(ecdf_sample(l[["IDate"]], ctrl = ctrl)[["values"]]), c("IDate", "Date"))
    expect_identical(class(ecdf_sample(l[["integer"]], ctrl = ctrl)[["values"]]), "integer")
    expect_identical(class(ecdf_sample(l[["integer64"]], ctrl = ctrl)[["values"]]), "integer64")
    expect_identical(class(ecdf_sample(l[["logical"]], ctrl = ctrl)[["values"]]), "logical")
    expect_identical(class(ecdf_sample(l[["ordered"]], ctrl = ctrl)[["values"]]), c("ordered", "factor"))
    expect_identical(class(ecdf_sample(l[["POSIXct"]], ctrl = ctrl)[["values"]]), c("POSIXct", "POSIXt"))
    expect_identical(class(ecdf_sample(l[["POSIXlt"]], ctrl = ctrl)[["values"]]), c("POSIXlt", "POSIXt"))
  }
); rm(l)


### Category Exclusions ('n_exc' & 'p_exc') ###
l <- list(
  m0.0 = rep(1L, 10),
  m0.1 = c(rep(1L, 9), NA),
  m0.2 = c(rep(1L, 8), rep(NA, 2)),
  m0.3 = c(rep(1L, 7), rep(NA, 3)),
  m0.4 = c(rep(1L, 6), rep(NA, 4))
)


### Tidy Up ###
rm(ctrl)
