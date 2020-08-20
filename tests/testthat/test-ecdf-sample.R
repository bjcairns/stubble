#========================#
#                        #
#### TEST ECDF SAMPLE ####
#                        #
#========================#


### ToDo ###
# - Assess the 'n_exc' & 'p_exc' control parameters.
# - Assess the 'drop_lev' control parameter.
# - Assess the 'fuzz_samp' control parameter.
# - Assess the 'dttm_tz' control parameter.


### Params ###
## Control ##
ctrl_def <- gen_stubble_ctrl(index = 1L)[-1]

# ## Vars ##
# vars_base <- c("character", "Date", "double", "factor", "integer", "logical", "ordered", "POSIXct", "POSIXlt")


### Unsupported Vector Classes ###
test_that(
  desc = "No method exists for vector of class.",
  code = {
    expect_warning(
      object = ecdf_sample(NULL, ctrl = ctrl_def),
      regexp = "[nN]o\\s+method\\s+exists",
      label = "Warning"
    )
    expect_type(
      object = suppressWarnings(
        ecdf_sample(NULL, ctrl = ctrl_def)
      ),
      type = "list"
    )
    expect_length(
      object = suppressWarnings(
        ecdf_sample(NULL, ctrl = ctrl_def)
      ),
      n = 2L
    )
    expect_named(
      object = suppressWarnings(
        ecdf_sample(NULL, ctrl = ctrl_def)
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
      object = ecdf_sample(l0[["character"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(l0[["Date"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(l0[["double"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(l0[["factor"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(l0[["integer"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(l0[["logical"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(l0[["ordered"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(l0[["POSIXct"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(l0[["POSIXlt"]], ctrl = ctrl_def),
      type = "list"
    )
  }
)

## bit64 ##
test_that(
  desc = "Zero-length vectors [bit64].",
  code = {
    skip_if_not(is.installed.package("bit64"))
    expect_type(
      object = ecdf_sample(l0[["integer64"]], ctrl = ctrl_def),
      type = "list"
    )
  }
)

## data.table ##
test_that(
  desc = "Zero-length vectors [data.table].",
  code = {
    skip_if_not(is.installed.package("data.table"))
    expect_type(
      object = ecdf_sample(l0[["IDate"]], ctrl = ctrl_def),
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
      object = ecdf_sample(lna[["character"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(lna[["Date"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(lna[["double"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(lna[["factor"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(lna[["integer"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(lna[["logical"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(lna[["ordered"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(lna[["POSIXct"]], ctrl = ctrl_def),
      type = "list"
    )
    expect_type(
      object = ecdf_sample(lna[["POSIXlt"]], ctrl = ctrl_def),
      type = "list"
    )
  }
)

## bit64 ##
test_that(
  desc = "Missing data vectors [bit64].",
  code = {
    skip_if_not(is.installed.package("bit64"))
    expect_type(
      object = ecdf_sample(lna[["integer64"]], ctrl = ctrl_def),
      type = "list"
    )
  }
)

## data.table ##
test_that(
  desc = "Missing data vectors [data.table].",
  code = {
    skip_if_not(is.installed.package("data.table"))
    expect_type(
      object = ecdf_sample(lna[["IDate"]], ctrl = ctrl_def),
      type = "list"
    )
  }
)


### Output Classes ###
## base ##
test_that(
  desc = "Output Classes (base).",
  code = {
    expect_identical(
      object = class(ecdf_sample(l1[["character"]], ctrl = ctrl_def)[["values"]]),
      expected = "character",
      label = "character"
    )
    expect_identical(
      object = class(ecdf_sample(l1[["Date"]], ctrl = ctrl_def)[["values"]]),
      expected = "Date",
      label = "Date"
    )
    expect_type(
      object = ecdf_sample(l1[["double"]], ctrl = ctrl_def)[["values"]],
      type = "double"
    )
    expect_identical(
      object = class(ecdf_sample(l1[["factor"]], ctrl = ctrl_def)[["values"]]),
      expected = "factor",
      label = "factor"
    )
    expect_identical(
      object = class(ecdf_sample(l1[["integer"]], ctrl = ctrl_def)[["values"]]),
      expected = "integer",
      label = "integer"
    )
    expect_identical(
      object = class(ecdf_sample(l1[["logical"]], ctrl = ctrl_def)[["values"]]),
      expected = "logical",
      label = "logical"
    )
    expect_identical(
      object = class(ecdf_sample(l1[["ordered"]], ctrl = ctrl_def)[["values"]]),
      expected = c("ordered", "factor"),
      label = "ordered"
    )
    expect_identical(
      object = class(ecdf_sample(l1[["POSIXct"]], ctrl = ctrl_def)[["values"]]),
      expected = c("POSIXct", "POSIXt"),
      label = "POSIXct"
    )
    expect_identical(
      object = class(ecdf_sample(l1[["POSIXlt"]], ctrl = ctrl_def)[["values"]]),
      expected = c("POSIXlt", "POSIXt"),
      label = "POSIXlt"
    )
  }
)

## bit64 ##
test_that(
  desc = "Output Classes [bit64].",
  code = {
    skip_if_not(is.installed.package("bit64"))
    expect_identical(
      object = class(ecdf_sample(l1[["integer64"]], ctrl = ctrl_def)[["values"]]),
      expected = "integer64",
      label = "integer64")
  }
)

## data.table ##
test_that(
  desc = "Output Classes [data.table].",
  code = {
    skip_if_not(is.installed.package("data.table"))
    expect_identical(
      object = class(ecdf_sample(l1[["IDate"]], ctrl = ctrl_def)[["values"]]),
      expected = c("IDate", "Date"),
      label = "IDate"
    )
  }
)


# ### Category Exclusions ('n_exc' & 'p_exc') ###
# ## Control ##
# ctrl <- gen_stubble_ctrl(old_ctrl = list(n_exc = 0, p_exc = 0), index = 1L)[-1]
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


### ecdf_sample_() output ###
test_that(
  desc = "ecdf_sample_() output.",
  code = {
    expect_type(
      object = ecdf_sample_(NULL, ctrl = ctrl_def),
      type = "list"
    )
    expect_length(
      object = ecdf_sample_(NULL, ctrl = ctrl_def),
      n = 2L
    )
    expect_named(
      object = ecdf_sample_(NULL, ctrl = ctrl_def),
      expected = c("values", "wt"),
      label = "Output names"
    )
  }
)


### Tidy Up ###
rm(ctrl_def) # vars_base
