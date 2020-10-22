#========================#
#                        #
#### TEST STUB SPLINE ####
#                        #
#========================#


### ToDo ###
# - Assess the 'emp_tail_exc' control parameter.
# - Assess the 'dttm_tz' control parameter.


### Params ###
## Control ##
ctrl_def <- stubble_ctrl(
  index = 1L,
  emp_sw = 0L,
  emp_tail_exc = 0,
  emp_fuzz_spl = 0
)
ctrl_def <- ctrl_def[names(ctrl_def) != "index"]

# ## Vars ##
# vars_spl <- c("Date", "double", "integer", "POSIXct", "POSIXlt")


### Unsupported Vector Classes ###
test_that(
  desc = "Unsupported output classes.",
  code = {
    expect_warning(
      object = stub_spline(NULL, ctrl = ctrl_def),
      regexp = "[nN]o\\s+method\\s+exists",
      label = "Warning"
    )
    expect_type(
      object = suppressWarnings(
        stub_spline(NULL, ctrl = ctrl_def)
      ),
      type = "list"
    )
    expect_length(
      object = suppressWarnings(
        stub_spline(NULL, ctrl = ctrl_def)
      ),
      n = 1L
    )
    expect_named(
      object = suppressWarnings(
        stub_spline(NULL, ctrl = ctrl_def)
      ),
      expected = "fun",
      label = "Output names"
    )
  }
)


### Zero-Length Vectors ###
## base ##
test_that(
  desc = "Zero-length vectors [base].",
  code = {
    expect_type(
      object = suppressWarnings(
        stub_spline(l0[["Date"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
    expect_type(
      object = suppressWarnings(
        stub_spline(l0[["double"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
    expect_type(
      object = suppressWarnings(
        stub_spline(l0[["integer"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
    expect_type(
      object = suppressWarnings(
        stub_spline(l0[["POSIXct"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
    expect_type(
      object = suppressWarnings(
        stub_spline(l0[["POSIXlt"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
  }
)

## bit64 ##
test_that(
  desc = "Zero-length vectors [bit64].",
  code = {
    skip_if_not_installed("bit64", min_v_bit64)
    expect_type(
      object = suppressWarnings(
        stub_spline(l0[["integer64"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
  }
)

## data.table ##
test_that(
  desc = "Zero-length vectors [data.table].",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_type(
      object = suppressWarnings(
        stub_spline(l0[["IDate"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
  }
)

### Missing Data Vectors ###
## base ##
test_that(
  desc = "Missing data vectors [base].",
  code = {
    expect_type(
      object = suppressWarnings(
        stub_spline(lna[["Date"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
    expect_type(
      object = suppressWarnings(
        stub_spline(lna[["double"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
    expect_type(
      object = suppressWarnings(
        stub_spline(lna[["integer"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
    expect_type(
      object = suppressWarnings(
        stub_spline(lna[["POSIXct"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
    expect_type(
      object = suppressWarnings(
        stub_spline(lna[["POSIXlt"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
  }
)

## bit64 ##
test_that(
  desc = "Missing data vectors [bit64].",
  code = {
    skip_if_not_installed("bit64", min_v_bit64)
    expect_type(
      object = suppressWarnings(
        stub_spline(lna[["integer64"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
  }
)

## data.table ##
test_that(
  desc = "Missing data vectors [data.table].",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_type(
      object = suppressWarnings(
        stub_spline(lna[["IDate"]], ctrl = ctrl_def)
      ),
      type = "list"
    )
  }
)


### Test Control Parameters ###
# - Todo!


### stub_spline_() output ###
## length(col) < 2L ##
test_that(
  desc = "stub_spline_() output (length(col) < 2L).",
  code = {
    expect_type(
      object = suppressWarnings(
        stub_spline_(NULL, ctrl = ctrl_def)
      ),
      type = "list"
    )
    expect_length(
      object = suppressWarnings(
        stub_spline_(NULL, ctrl = ctrl_def)
      ),
      n = 1L
    )
    expect_named(
      object = suppressWarnings(
        stub_spline_(NULL, ctrl = ctrl_def)
      ),
      expected = "fun",
      label = "Output names"
    )
  }
)

## length(col) >= 2L ##
test_that(
  desc = "stub_spline_() output (length(col) >= 2L).",
  code = {
    expect_type(
      object = stub_spline_(1:10, ctrl = ctrl_def),
      type = "list"
    )
    expect_length(
      object = stub_spline_(1:10, ctrl = ctrl_def),
      n = 1L
    )
    expect_named(
      object = stub_spline_(1:10, ctrl = ctrl_def),
      expected = "fun",
      label = "Output names"
    )
  }
)


### Tidy Up ###
rm(ctrl_def) # vars_spl
