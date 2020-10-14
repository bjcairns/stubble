#=========================#
#                         #
#### TEST STUBBLE CTRL ####
#                         #
#=========================#


### Old Param Retention ###
test_that(
  desc = "stubble_ctrl correctly retains old parameters",
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
  desc = "stubble_ctrl handles vector parameters",
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
