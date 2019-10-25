context("gen_col()")

test_that("gen_col returns expected classes", {
  expect_true(is.integer(gen_col(as.integer(c()), elements = 10L)))  # integer
  expect_true(is.numeric(gen_col(as.numeric(c()), elements = 10L)))  # numeric
  expect_true(is.list(gen_col(as.list(c()), elements = 10L)))        # list
  expect_true(is.factor(gen_col(as.factor(c()), elements = 10L)))    # factor
  expect_true(is.logical(gen_col(TRUE, elements = 10L)))             # logical
  expect_identical(                                                  # date
    "Date",
    class(gen_col(as.Date(character()), elements = 10L))
  )
  expect_identical(                                                  # date-time
    "POSIXct",
    class(gen_col(as.POSIXct(numeric(), origin = "1970-01-01"), elements = 10L))[1]
  )
})

test_that("gen_col can return a length-0 vector", {
  expect_identical(
    length(gen_col(as.integer(c()), 0)),
    0L
  )
})
