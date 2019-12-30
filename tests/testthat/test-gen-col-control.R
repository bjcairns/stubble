context("gen_col_control()")

test_that("gen_col_control() correctly retains old parameters", {

  # Parameters #1 has custom int_min; #2 adds custom int_max; #3 overwrites
  expect_error(ctrl1 <- gen_col_control(int_min = -1L), NA)
  ctrl2 <- gen_col_control(int_max = 99L, old_ctrl = ctrl1)
  ctrl3 <- gen_col_control(int_min = -2L, old_ctrl = ctrl1)

  expect_equal(ctrl1$int_min, list(-1L))
  expect_equal(ctrl2$int_max, list(99L))
  expect_equal(ctrl2$int_min, ctrl1$int_min)    # Old parameter retained
  expect_equal(ctrl3$int_min, list(-2L))        # Old parameter overwritten

})

test_that("gen_col_control() handles vector parameters", {

  # Test: can set a mix of vector / non-vector parameters
  expect_error(
    ctrl1 <- gen_col_control(int_min = c(-1L, -2L), int_max = 99L),
    NA
  )

  expect_equal(ctrl1$int_min, list(c(-1L, -2L)))  # Vector parameter stored
  expect_equal(ctrl1$int_max, list(99L))          # Non-vector parameter stored

})
