context("gen_col()")

test_that("gen_col returns expected classes", {
  expect_true(is.integer(gen_col(as.integer(c()), 10)))      # integer
  expect_true(is.numeric(gen_col(as.numeric(c()), 10)))      # numeric
  expect_true(is.list(gen_col(as.list(c()), 10)))            # list
  expect_true(is.factor(gen_col(as.factor(c()), 10)))        # factor
  expect_true(is.logical(gen_col(TRUE, 10)))                 # logical
  expect_identical(                                          # date
    "Date",
    class(gen_col(as.Date(character()), 10))
  )
  expect_identical(                                          # date-time
    "POSIXct",
    class(gen_col(as.POSIXct(numeric(), origin = "1970-01-01"), 10))[1]
  )
})

test_that("gen_col can return a length-0 vector", {
  expect_identical(
    length(gen_col(as.integer(c()), 0)),
    0L
  )
})
