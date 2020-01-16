context("gen_col()")

# Fixed seed for RNGs
use.seed <- 29305678L

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
  suppressWarnings(
    num1 <- gen_col(
      as.numeric(c()),
      elements = 10L,
      dbl_rng_kind = "Mersenne-Twister"
    )
  )
  set.seed(8972345)
  suppressWarnings(
    num2 <- gen_col(
      as.numeric(c()),
      elements = 10L,
      dbl_rng_kind = "L'Ecuyer-CMRG"
    )
  )

  expect_false(identical(num1, num2))

  # dbl_rng_kind only makes temporary changes
  expect_equal("Mersenne-Twister", RNGkind()[1])

})

test_that("synthetic integer and numeric columns are unique as required", {

  # integer columns must have a range large enough to select uniquely from
  int1 <- gen_col(
    integer(), elements = 100L,
    int_max = 100, unique = TRUE
  )
  expect_equal(anyDuplicated(int1), 0)

  expect_error(
    int2 <- gen_col(
      integer(), elements = 100L,
      int_max = 10, unique = TRUE
    )
  )

  # numerics should be unique (before rounding) whenever RNGkind is
  # "Wichmann-Hill"
  num1 <- gen_col(numeric(), elements = 500000L, unique = TRUE)
  expect_equal(anyDuplicated(num1), 0)

  expect_error(
    num2 <- gen_col(
      numeric(),
      elements = 500000L,
      unique = TRUE,
      dbl_rng_kind = "Mersenne-Twister"
    )
  )

  expect_warning(
    gen_col(
      numeric(),
      elements = 500000L,
      unique = FALSE,
      dbl_rng_kind = "Mersenne-Twister"
    )
  )

})

test_that("synthetic character columns are unique as required", {

  # Expect unique elements; lots of symbols, few elements
  chr1 <- gen_col(
    character(),
    elements = 10L,
    unique = TRUE,
    chr_min = 5L, chr_max = 10L,
    chr_try_unique = TRUE
  )
  expect_equal(anyDuplicated(chr1), 0)

  # Expect error; insufficient symbols
  expect_error(
    gen_col(
      character(),
      elements = 10L,
      unique = TRUE,
      chr_min = 1L, chr_max = 2L,
      chr_sym = list(LETTERS[1:2])
    )
  )

  # Expect error, retries allowed but no attempts
  set.seed(use.seed)
  expect_error(
    gen_col(
      character(),
      elements = 10L,
      unique = TRUE,
      chr_min = 1L, chr_max = 3L,
      chr_sym = list(LETTERS[1:2]),
      chr_try_unique = TRUE,
      chr_try_unique_attempts = 0L
    )
  )

  # Expect no error (i.e. error is NA); 10 retries allowed
  set.seed(use.seed)
  expect_error(
    gen_col(
      character(),
      elements = 10L,
      unique = TRUE,
      chr_min = 1L, chr_max = 3L,
      chr_sym = list(LETTERS[1:2]),
      chr_try_unique = TRUE,
      chr_try_unique_attempts = 10L
    ),
    NA
  )

})
