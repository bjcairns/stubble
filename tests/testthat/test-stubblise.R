context("stubblise()")

test_that("stubblise maintains structure", {

  expect_identical(lapply(iris, names), lapply(stubblise(iris), names))
  expect_identical(lapply(iris, class), lapply(stubblise(iris), class))

  expect_identical(lapply(mtcars, names), lapply(stubblise(mtcars), names))
  expect_identical(lapply(mtcars, class), lapply(stubblise(mtcars), class))

})

test_that("stubblise can return a tibble with 0 rows",{

  iris_stbl <- stubblise(iris, rows = 0)

  expect_identical(lapply(iris, names), lapply(iris_stbl, names))
  expect_identical(lapply(iris, class), lapply(iris_stbl, class))

  expect_true(
    dim(iris_stbl)[1] == 0L
  )

})

test_that("stubblise and stubblize are equivalent", {

  expect_identical(stubblise, stubblize)

})
