context("stubblise()")

test_that("stubblise maintains structure", {

  expect_identical(lapply(iris, names), lapply(stubblise(iris), names))
  expect_identical(lapply(iris, class), lapply(stubblise(iris), class))

  expect_identical(lapply(mtcars, names), lapply(stubblise(mtcars), names))
  expect_identical(lapply(mtcars, class), lapply(stubblise(mtcars), class))

})
