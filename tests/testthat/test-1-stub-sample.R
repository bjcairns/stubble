#========================#
#                        #
#### TEST STUB_SAMPLE ####
#                        #
#========================#


### ToDo ###
# - Assess the 'emp_n_exc' & 'emp_p_exc' control parameters.
# - Assess the 'emp_drop_lev' control parameter.
# - Assess the 'emp_fuzz_samp' control parameter.
# - Assess the 'dttm_tz' control parameter.


### Params ###
## Control ##
ctrl_def <- stubble_ctrl(
  index = 1L,
  emp_sw = 1L,
  emp_n_exc = 0L,
  emp_p_exc = 0,
  emp_fuzz_samp = 0,
  emp_drop_lev = FALSE
)
ctrl_def <- ctrl_def[names(ctrl_def) != "index"]

# ## Vars ##
# vars_base <- c("character", "Date", "double", "factor", "integer", "logical", "ordered", "POSIXct", "POSIXlt")


### Unsupported Vector Classes ###
test_that(
  desc = "No method exists for vector of class.",
  code = {
    expect_warning(
      object = stub_sample(NULL, ctrl = ctrl_def),
      regexp = "[nN]o\\s+method\\s+exists",
      label = "Warning"
    )
    expect_type(
      object = suppressWarnings(
        stub_sample(NULL, ctrl = ctrl_def)
      ),
      type = "list"
    )
    expect_length(
      object = suppressWarnings(
        stub_sample(NULL, ctrl = ctrl_def)
      ),
      n = 2L
    )
    expect_named(
      object = suppressWarnings(
        stub_sample(NULL, ctrl = ctrl_def)
      ),
      expected = c("values", "wt"),
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
      object = stub_sample(l0[["character"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(l0[["Date"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(l0[["double"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(l0[["factor"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(l0[["integer"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(l0[["logical"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(l0[["ordered"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(l0[["POSIXct"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(l0[["POSIXlt"]], ctrl = ctrl_def),
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
      object = stub_sample(l0[["integer64"]], ctrl = ctrl_def),
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
      object = stub_sample(l0[["IDate"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(l0[["ITime"]], ctrl = ctrl_def),
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
      object = stub_sample(lna[["character"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(lna[["Date"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(lna[["double"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(lna[["factor"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(lna[["integer"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(lna[["logical"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(lna[["ordered"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(lna[["POSIXct"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(lna[["POSIXlt"]], ctrl = ctrl_def),
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
      object = stub_sample(lna[["integer64"]], ctrl = ctrl_def),
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
      object = stub_sample(lna[["IDate"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = stub_sample(lna[["ITime"]], ctrl = ctrl_def),
      type = "list"
    )
  }
)


### Output Classes ###

## control parameters
ctrl_def_output <- ctrl_def
ctrl_def_output[["emp_n_exc"]] <- 0L

## base ##
test_that(
  desc = "Output Classes [base].",
  code = {
    expect_true(
      object = is.character(stub_sample(l1[["character"]], ctrl = ctrl_def_output)[["values"]]),
      label = "character"
    )
    expect_identical(
      object = class(stub_sample(l1[["Date"]], ctrl = ctrl_def_output)[["values"]]),
      expected = "Date",
      label = "Date"
    )
    expect_type(
      object = stub_sample(l1[["double"]], ctrl = ctrl_def_output)[["values"]],
      type = "double"
    )
    expect_identical(
      object = class(stub_sample(l1[["factor"]], ctrl = ctrl_def_output)[["values"]]),
      expected = "factor",
      label = "factor"
    )
    expect_identical(
      object = class(stub_sample(l1[["integer"]], ctrl = ctrl_def_output)[["values"]]),
      expected = "integer",
      label = "integer"
    )
    expect_identical(
      object = class(stub_sample(l1[["logical"]], ctrl = ctrl_def_output)[["values"]]),
      expected = "logical",
      label = "logical"
    )
    expect_identical(
      object = class(stub_sample(l1[["ordered"]], ctrl = ctrl_def_output)[["values"]]),
      expected = c("ordered", "factor"),
      label = "ordered"
    )
    expect_true(
      object = "POSIXct" %in% class(stub_sample(l1[["POSIXct"]], ctrl = ctrl_def_output)[["values"]]),
      label = "POSIXct"
    )
    expect_true(
      object = "POSIXlt" %in% class(stub_sample(l1[["POSIXlt"]], ctrl = ctrl_def_output)[["values"]]),
      label = "POSIXlt"
    )
  }
)

## bit64 ##
test_that(
  desc = "Output Classes [bit64].",
  code = {
    skip_if_not_installed("bit64", min_v_bit64)
    expect_identical(
      object = class(stub_sample(l1[["integer64"]], ctrl = ctrl_def_output)[["values"]]),
      expected = "integer64",
      label = "integer64")
  }
)

## data.table ##
test_that(
  desc = "Output Classes [data.table].",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_true(
      object = "IDate" %in% class(stub_sample(l1[["IDate"]], ctrl = ctrl_def_output)[["values"]]),
      label = "IDate"
    )
    expect_identical(
      object = class(stub_sample(l1[["ITime"]], ctrl = ctrl_def_output)[["values"]]),
      expected = "ITime",
      label = "ITime"
    )
  }
); rm(ctrl_def_output)


# ### Category Exclusions ('emp_n_exc' & 'emp_p_exc') ###
# ## Control ##
# ctrl <- stubble_ctrl(old_ctrl = list(emp_n_exc = 0, emp_p_exc = 0), index = 1L)[-1]
# 
# ## Data ##
# v <- c(NA, 1L)
# l <- list(
#   na00 = rep(v, times = c(0, 10)),
#   na01 = rep(v, times = c(1, 9)),
#   na02 = rep(v, times = c(2, 8)),
#   na03 = rep(v, times = c(3, 7)),
#   na04 = rep(v, times = c(4, 6)),
#   na05 = rep(v, times = c(5, 5)),
#   na06 = rep(v, times = c(6, 4)),
#   n07 = rep(v, times = c(7, 3)),
#   na08 = rep(v, times = c(8, 2)),
#   na09 = rep(v, times = c(9, 1)),
#   na10 = rep(v, times = c(10, 0))
# ); rm(v)
# 
# ## Test ##
# test_that(
#   desc = "",
#   code = {
#     
#   }
# )
# 
# ## Tidy Up ##
# rm(ctrl, l)


### stub_sample_() output ###
test_that(
  desc = "stub_sample_() output.",
  code = {
    expect_type(
      object = stub_sample_(NULL, ctrl = ctrl_def),
      type = "list"
    )
    expect_length(
      object = stub_sample_(NULL, ctrl = ctrl_def),
      n = 2L
    )
    expect_named(
      object = stub_sample_(NULL, ctrl = ctrl_def),
      expected = c("values", "wt"),
      label = "Output names"
    )
  }
)


### Correct Functioning ###
test_that(
  desc = "Correct Functioning.",
  code = {
    expect_identical(
      object = lengths(
        lapply(
          X = lapply(
            X = luniq,
            FUN = stub_sample,
            ctrl = ctrl_def
          ),
          FUN = `[[`,
          "values"
        )
      ),
      expected = lengths(lapply(X = luniq, FUN = unique)),
      label = "Correctly sampled unique values"
    )
    expect_equivalent(
      object = lapply(
        X = lapply(
          X = lapply(
            X = luniq,
            FUN = stub_sample,
            ctrl = ctrl_def
          ),
          FUN = `[[`,
          "wt"
        ),
        FUN = sum
      ),
      expected = rep(1, length(luniq)),
      label = "Correct calculation of 'wt'"
    )
    expect_identical(
      object = lengths(
        lapply(
          X = lapply(
            X = luniq,
            FUN = stub_sample,
            ctrl = ctrl_def
          ),
          FUN = `[[`,
          "values"
        )
      ),
      expected = lengths(
        lapply(
          X = lapply(
            X = luniq,
            FUN = stub_sample,
            ctrl = ctrl_def
          ),
          FUN = `[[`,
          "wt"
        )
      ),
      label = "lengths of 'values' & 'wt' match"
    )
  }
)


### Tidy Up ###
rm(ctrl_def) # vars_base
