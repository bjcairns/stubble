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

test_that("gen_col_ methods support alternative RNG algorithms", {
  # Currently only methods for numeric vectors support this
  RNGkind(kind = "Mersenne-Twister")
  set.seed(8972345)
  num1 <- gen_col(
    as.numeric(c()),
    elements = 10L,
    dbl_rng_kind = "Mersenne-Twister"
  )
  set.seed(8972345)
  num2 <- gen_col(
    as.numeric(c()),
    elements = 10L,
    dbl_rng_kind = "L'Ecuyer-CMRG"
  )
  expect_false(identical(num1, num2))
  expect_equal("Mersenne-Twister", RNGkind()[1])
})

test_that("synthetic integer and numeric columns are unique as required", {

  int1 <- gen_col(as.integer(c()), elements = 500000L, unique = TRUE)
  expect_equal(anyDuplicated(int1), 0)

  # numerics should always be unique by construction
  num1 <- gen_col(as.numeric(c()), elements = 500000L)
  expect_equal(anyDuplicated(num1), 0)

})

test_that("synthetic character columns are unique as required", {

  # Warn when there is a *risk* of duplication
  expect_warning(
    gen_col(
      as.character(c()),
      elements = 10L,
      unique = TRUE,
      chr_min = 1,
      chr_max = 2,
      chr_sym = LETTERS[1:4]
    )
  )

  # Error when duplication is certain
  expect_error(
    gen_col(
      as.character(c()),
      elements = 10L,
      unique = TRUE,
      chr_min = 1,
      chr_max = 2,
      chr_sym = LETTERS[1:2]
    )
  )

  # Expect warning when there is duplication but uniqueness is not enforced
  expect_warning(

    gen_col(
      as.character(c()),
      elements = 10L,
      unique = TRUE,
      chr_min = 1,
      chr_max = 2,
      chr_sym = LETTERS[1:2]
    )
  )

  expect_equal(anyDuplicated(char1), 0)

})
