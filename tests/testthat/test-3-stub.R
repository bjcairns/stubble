#=================#
#                 #
#### TEST STUB ####
#                 #
#=================#


### ToDo ###
# - Test function can run on list objects with different length elements.
# - Test `rows` argument is encoded.
# - Test `method` argument is encoded.


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


### Data Structure Encoding ###
## base ##
test_that(
  desc = "Data Structure Encoding [base]",
  code = {
    expect_identical(
      object = class(stub(l1, ctrl = ctrl_def)[["dtype"]]),
      expected = "list",
      label = "list"
    )
    expect_identical(
      object = class(stub(as.data.frame(l1), ctrl = ctrl_def)[["dtype"]]),
      expected = "data.frame",
      label = "data.frame"
    )
  }
)

## data.table ##
test_that(
  desc = "Data Structure Encoding [data.table]",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_identical(
      object = class(stub(suppressWarnings(data.table::as.data.table(l1)), ctrl = ctrl_def)[["dtype"]]),
      expected = c("data.table", "data.frame"),
      label = "data.table"
    )
  }
)

## tibble ##
test_that(
  desc = "Data Structure Encoding [tibble]",
  code = {
    skip_if_not_installed("tibble", min_v_tibble)
    expect_identical(
      object = class(stub(tibble::as_tibble(l1), ctrl = ctrl_def)[["dtype"]]),
      expected = c("tbl_df", "tbl", "data.frame"),
      label = "tibble"
    )
  }
)


### Tidy Up ###
rm(ctrl_def)
