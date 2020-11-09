#=========================#
#                         #
#### TEST STUBBLE_CTRL ####
#                         #
#=========================#


### ToDo ###
# - Test fallback class assertions.


### Output Structure ###
test_that(
  desc = "Ouput structure.",
  code = {
    ## Set Control Params (List Format) ##
    ctrl_def <- stubble_ctrl()
    
    ## List Structure ##
    expect_true(
      object = is.list(ctrl_def),
      label = "List structure (list format)"
    )
    expect_true(
      object = all(sapply(ctrl_def, is.list)),
      label = "Nested list structure"
    )
    
    ## Set Control Params (Vector Format) ##
    ctrl_def <- stubble_ctrl(index = 1L)
    
    ## List Structure ##
    expect_true(
      object = is.list(ctrl_def),
      label = "List structure (vector format)"
    )
    expect_false(
      object = all(sapply(ctrl_def, is.list)),
      label = "Nested vector structure"
    )
  }
)


### Output Classes ###
## base ##
test_that(
  desc = "Output classes [base].",
  code = {
    ## Set Control Params (Vector Format) ##
    ctrl_def <- stubble_ctrl(index = 1L)
    
    ## Classes ##
    # expect_true(
    #   object = all(sapply(ctrl_def[CTRL_CLASS[["character"]]], is.character)),
    #   label = "character"
    # )
    expect_equivalent(
      object = sapply(ctrl_def[CTRL_CLASS[["Date"]]], class),
      expected = rep("Date", length(CTRL_CLASS[["Date"]])),
      label = "Date"
    )
    expect_true(
      object = all(sapply(ctrl_def[CTRL_CLASS[["integer"]]], is.integer)),
      label = "integer"
    )
    expect_true(
      object = all(sapply(ctrl_def[CTRL_CLASS[["logical"]]], is.logical)),
      label = "logical"
    )
    expect_equivalent(
      object = sapply(ctrl_def[CTRL_CLASS[["POSIXct"]]], function(x){class(x)[1L]}),
      expected = rep("POSIXct", length(CTRL_CLASS[["POSIXct"]])),
      label = "POSIXct"
    )
  }
)

## bit64 ##
test_that(
  desc = "Output classes [bit64].",
  code = {
    skip_if_not_installed("bit64", min_v_bit64)
    
    ## Set Control Params (Vector Format) ##
    ctrl_def <- stubble_ctrl(index = 1L)
    
    ## Classes ##
    expect_true(
      object = all(sapply(ctrl_def[CTRL_CLASS[["integer64"]]], bit64::is.integer64)),
      label = "integer64"
    )
  }
)

## data.table ##
test_that(
  desc = "Output classes [data.table].",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    
    ## Set Control Params (Vector Format) ##
    ctrl_def <- stubble_ctrl(index = 1L)
    
    ## Classes ##
    expect_equivalent(
      object = sapply(ctrl_def[CTRL_CLASS[["ITime"]]], class),
      expected = rep("ITime", length(CTRL_CLASS[["ITime"]])),
      label = "ITime"
    )
  }
)


### Old Param Retention ###
test_that(
  desc = "stubble_ctrl correctly retains old parameters.",
  code = {
    
    # Parameters #1 has custom agn_int_min; #2 adds custom agn_int_max; #3 overwrites #
    expect_error(
      object = ctrl1 <- stubble_ctrl(agn_int_min = -1L),
      regexp = NA
    )
    
    ctrl1 <- stubble_ctrl(agn_int_min = -1L)
    ctrl2 <- stubble_ctrl(agn_int_max = 99L, old_ctrl = ctrl1)
    ctrl3 <- stubble_ctrl(agn_int_min = -2L, old_ctrl = ctrl1)
    
    expect_equal(
      object = ctrl1$agn_int_min,
      expected = list(-1L)
    )
    
    expect_equal(
      object = ctrl2$agn_int_max,
      expected = list(99L)
    )
    
    # Old parameter retained #
    expect_equal(
      object = ctrl2$agn_int_min,
      expected = ctrl1$agn_int_min
    )
    
    # Old parameter overwritten #
    expect_equal(
      object = ctrl3$agn_int_min,
      expected = list(-2L)
    )
    
  }
)


### Vector Param Inputs ###
test_that(
  desc = "stubble_ctrl handles vector parameters.",
  code = {
    
    # Test: can set a mix of vector / non-vector parameters #
    expect_error(
      object = ctrl1 <- stubble_ctrl(agn_int_min = c(-1L, -2L), agn_int_max = 99L),
      regexp = NA
    )
    
    # Vector parameter stored #
    expect_equal(
      object = ctrl1$agn_int_min,
      expected = as.list(c(-1L, -2L))
    )
    
    # Non-vector parameter stored #
    expect_equal(
      object = ctrl1$agn_int_max,
      expected = list(99L)
    )
    
  }
)
