context("stubblise()")

# seed for common results
use_seed <- 237892342L

# Synthesise a reference data.frame
set.seed(use_seed)
iris_stbl_df <- stubblise(iris, rows = 100L)

test_that("stubblise maintains structure", {

  expect_identical(lapply(iris, names), lapply(stubblise(iris), names))
  expect_identical(lapply(iris, class), lapply(stubblise(iris), class))

  expect_identical(lapply(mtcars, names), lapply(stubblise(mtcars), names))
  expect_identical(lapply(mtcars, class), lapply(stubblise(mtcars), class))

})

test_that("stubblise can return data with 0 rows or many",{

  iris_stbl0 <- stubblise(iris, rows = 0L)

  expect_identical(lapply(iris, names), lapply(iris_stbl0, names))
  expect_identical(lapply(iris, class), lapply(iris_stbl0, class))

  expect_true(
    dim(iris_stbl0)[1] == 0L
  )

  nr <- as.integer(Sys.time()) %% 100 + 1
  iris_stbln <- stubblise(iris, rows = nr)
  expect_true(
    dim(iris_stbln)[1] == nr
  )

})

test_that("stubblise handles tibbles", {

  skip_if_not_installed("tibble")

  iris_tbl_df <- tibble::as_tibble(iris)
  set.seed(use_seed)
  expect_identical(
    iris_stbl_df,
    as.data.frame(stubblise(iris_tbl_df, rows = 100L))
  )

})

test_that("stubblise handles data.tables", {

  # data.tables
  iris_dt <- data.table::as.data.table(iris)
  set.seed(use_seed)
  expect_identical(
    iris_stbl_df,
    as.data.frame(stubblise(iris_dt, rows = 100L))
  )

})

test_that("stubblise handles lists", {

  # Standard list
  iris_list <- as.list(iris)

  # List of vectors without identical lengths
  iris_list_uneven <- iris_list
  iris_list_uneven[[2]] <- iris_list_uneven[[2]][1:10]

  set.seed(use_seed)
  expect_identical(
    iris_stbl_df,
    as.data.frame(stubblise(iris_list, rows = 100L))
  )

  set.seed(use_seed)
  expect_identical(
    iris_stbl_df,
    as.data.frame(stubblise(iris_list_uneven, rows = 100L))
  )

})

test_that("stubblise correctly handles control lists", {

  set.seed(2342343)
  syn_iris_1 <- stubblise(
    iris,
    fct_lvls = list(levels(iris$Species))
  )

  set.seed(2342343)
  syn_iris_2 <- stubblise(
    iris,
    ctrl = list(fct_lvls = list(levels(iris$Species)))
  )

  set.seed(2342343)
  syn_iris_3 <- stubblise(
    iris,
    ctrl = list(fct_lvls = list(letters[1:3])),
    fct_lvls = list(levels(iris$Species))
  )

  expect_identical(syn_iris_1, syn_iris_2)
  expect_identical(syn_iris_1, syn_iris_3)

})

test_that("stubblise and stubblize are equivalent", {

  expect_identical(stubblise, stubblize)

})
