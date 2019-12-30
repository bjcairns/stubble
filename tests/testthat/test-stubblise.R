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

test_that("stubblise handles a range of data frame-like classes", {

  # Standard data frame-like objects
  iris_tbl_df <- tibble::as_tibble(iris)
  iris_dt <- data.table::as.data.table(iris)
  iris_list <- as.list(iris)

  # List of vectors without identical lengths
  iris_list_uneven <- iris_list
  iris_list_uneven[[2]] <- iris_list_uneven[[2]][1:10]

  # Synthesise the reference data.frame
  use_seed <- 237892342L
  set.seed(use_seed)
  iris_stbl <- stubblise(iris, rows = 100L)

  # Compare all classes of iris data to the reference
  purrr::map(
    list(iris_tbl_df, iris_dt, iris_list, iris_list_uneven),
    ~ {
      set.seed(use_seed)
      expect_identical(iris_stbl, as.data.frame(stubblise(.x, rows = 100L)))
    }
  )

})

test_that("stubblise and stubblize are equivalent", {

  expect_identical(stubblise, stubblize)

})
