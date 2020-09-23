context("stubblise()")

# seed for common results
use_seed <- 237892342L

# Synthesise a reference data.frame
set.seed(use_seed)
penguins_stbl_df <- stubblise(penguins, rows = 100L)

test_that("stubblise maintains structure", {

  expect_identical(lapply(penguins, names), lapply(stubblise(penguins), names))
  expect_identical(lapply(penguins, class), lapply(stubblise(penguins), class))

  expect_identical(lapply(mtcars, names), lapply(stubblise(mtcars), names))
  expect_identical(lapply(mtcars, class), lapply(stubblise(mtcars), class))

})

test_that("stubblise can return data with 0 rows or many",{

  penguins_stbl0 <- stubblise(penguins, rows = 0L)

  expect_identical(names(penguins), names(penguins_stbl0))
  expect_identical(lapply(penguins, class), lapply(penguins_stbl0, class))

  expect_true(
    dim(penguins_stbl0)[1] == 0L
  )

  nr <- as.integer(Sys.time()) %% 100 + 1
  penguins_stbln <- stubblise(penguins, rows = nr)
  expect_true(
    dim(penguins_stbln)[1] == nr
  )

})

test_that("stubblise handles tibbles", {

  skip_if_not_installed("tibble")

  penguins_tbl_df <- tibble::as_tibble(penguins)
  set.seed(use_seed)
  expect_identical(
    penguins_stbl_df,
    as.data.frame(stubblise(penguins_tbl_df, rows = 100L))
  )

})

test_that("stubblise handles data.tables", {

  # data.tables
  penguins_dt <- data.table::as.data.table(penguins)
  set.seed(use_seed)
  expect_identical(
    penguins_stbl_df,
    as.data.frame(stubblise(penguins_dt, rows = 100L))
  )

})

test_that("stubblise handles lists", {

  # Standard list
  penguins_list <- as.list(penguins)

  # List of vectors without identical lengths
  penguins_list_uneven <- penguins_list
  penguins_list_uneven[[2]] <- penguins_list_uneven[[2]][1:10]

  set.seed(use_seed)
  expect_identical(
    penguins_stbl_df,
    as.data.frame(stubblise(penguins_list, rows = 100L))
  )

  set.seed(use_seed)
  expect_identical(
    penguins_stbl_df,
    as.data.frame(stubblise(penguins_list_uneven, rows = 100L))
  )

})

test_that("stubblise correctly handles control lists", {

  set.seed(2342343)
  syn_penguins_1 <- stubblise(
    penguins,
    fct_lvls = list(levels(penguins$species))
  )

  set.seed(2342343)
  syn_penguins_2 <- stubblise(
    penguins,
    ctrl = list(fct_lvls = list(levels(penguins$species)))
  )

  set.seed(2342343)
  syn_penguins_3 <- stubblise(
    penguins,
    ctrl = list(fct_lvls = list(letters[1:3])),
    fct_lvls = list(levels(penguins$species))
  )

  expect_identical(syn_penguins_1, syn_penguins_2)
  expect_identical(syn_penguins_1, syn_penguins_3)

})

test_that("stubblise and stubblize are equivalent", {

  expect_identical(stubblise, stubblize)

})
