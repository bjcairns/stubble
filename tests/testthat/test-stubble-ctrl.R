test_that("stubble_ctrl correctly retains old parameters", {
  
  # Parameters #1 has custom agn_int_min; #2 adds custom agn_int_max; #3 overwrites
  expect_error(ctrl1 <- stubble_ctrl(agn_int_min = -1L), NA)
  ctrl2 <- stubble_ctrl(agn_int_max = 99L, old_ctrl = ctrl1)
  ctrl3 <- stubble_ctrl(agn_int_min = -2L, old_ctrl = ctrl1)
  
  expect_equal(ctrl1$agn_int_min, list(-1L))
  expect_equal(ctrl2$agn_int_max, list(99L))
  expect_equal(ctrl2$agn_int_min, ctrl1$agn_int_min)    # Old parameter retained
  expect_equal(ctrl3$agn_int_min, list(-2L))        # Old parameter overwritten
  
})

test_that("stubble_ctrl handles vector parameters", {
  
  # Test: can set a mix of vector / non-vector parameters
  expect_error(
    ctrl1 <- stubble_ctrl(agn_int_min = c(-1L, -2L), agn_int_max = 99L),
    NA
  )
  
  expect_equal(ctrl1$agn_int_min, as.list(c(-1L, -2L)))  # Vector parameter stored
  expect_equal(ctrl1$agn_int_max, list(99L))          # Non-vector parameter stored
  
})
