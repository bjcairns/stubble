context("gen_col()")

test_that("gen_col returns expected classes", {
  expect_equal(class(gen_col(as.integer(c()), 1)), "integer")
})
