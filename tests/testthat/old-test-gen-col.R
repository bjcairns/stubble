context("gen_col()")

# Fixed seed for RNGs
use.seed <- 29305678L

test_that("gen_col returns expected classes", {
  expect_true(is.integer(gen_col(integer(), elements = 10L)))  # integer
  expect_true(is.double(gen_col(double(), elements = 10L)))    # numeric
  expect_true(is.list(gen_col(list(), elements = 10L)))        # list
  expect_true(is.factor(gen_col(factor(), elements = 10L)))    # factor
  expect_true(is.logical(gen_col(logical(), elements = 10L)))  # logical
  expect_identical(                                                        # date
    "Date",
    class(gen_col(as.Date(character()), elements = 10L))
  )
  expect_identical(                                                        # date-time
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

test_that("gen_col generates unique integer and numeric columns as required", {

  # integer columns must have a range large enough to select uniquely from
  int1 <- gen_col(
    integer(), elements = 100L,
    agn_int_max = 100, agn_unique = TRUE
  )
  expect_equal(anyDuplicated(int1), 0)

  expect_error(
    int2 <- gen_col(
      integer(), elements = 100L,
      agn_int_max = 10, agn_unique = TRUE
    )
  )

  # numerics should be unique (before rounding) whenever RNGkind is
  # "Wichmann-Hill"
  num1 <- gen_col(numeric(), elements = 500000L, agn_unique = TRUE)
  expect_equal(anyDuplicated(num1), 0)

  expect_error(
    num2 <- gen_col(
      numeric(),
      elements = 500000L,
      agn_unique = TRUE,
      dbl_rng_kind = "Mersenne-Twister"
    )
  )

  expect_warning(
    gen_col(
      numeric(),
      elements = 500000L,
      agn_unique = FALSE,
      dbl_rng_kind = "Mersenne-Twister"
    )
  )

})

test_that("gen_col allows an integer list", {

  agn_int_list <- sample.int(1000,100)

  expect_true(
    all(
      gen_col(
        integer(),
        elements = 100L,
        agn_int_list = list(agn_int_list),
        agn_unique = TRUE
      ) %in%
        agn_int_list
    )
  )

})

test_that("gen_col generates unique character columns as required", {

  # Expect unique elements; lots of symbols, few elements
  chr1 <- gen_col(
    character(),
    elements = 10L,
    agn_unique = TRUE,
    agn_chr_min = 5L, agn_chr_max = 10L,
    agn_chr_try_unique = TRUE
  )
  expect_equal(anyDuplicated(chr1), 0)

  # Expect error; insufficient symbols
  expect_error(
    gen_col(
      character(),
      elements = 10L,
      agn_unique = TRUE,
      agn_chr_min = 1L, agn_chr_max = 2L,
      agn_chr_sym = list(LETTERS[1:2])
    )
  )

  # Expect error, retries allowed but no attempts
  set.seed(use.seed)
  expect_error(
    gen_col(
      character(),
      elements = 10L,
      agn_unique = TRUE,
      agn_chr_min = 1L, agn_chr_max = 3L,
      agn_chr_sym = list(LETTERS[1:2]),
      agn_chr_try_unique = TRUE,
      agn_chr_try_unique_attempts = 0L
    )
  )

  # Expect no error (i.e. error is NA); 10 retries allowed
  set.seed(use.seed)
  expect_error(
    gen_col(
      character(),
      elements = 10L,
      agn_unique = TRUE,
      agn_chr_min = 1L, agn_chr_max = 3L,
      agn_chr_sym = list(LETTERS[1:2]),
      agn_chr_try_unique = TRUE,
      agn_chr_try_unique_attempts = 10L
    ),
    NA
  )

})

test_that("gen_col correctly includes separators in character columns", {

  no_sep <- gen_col(
    character(),
    elements = 50L,
    agn_unique = FALSE,
    agn_chr_min = 1L, agn_chr_max = 10L,
    agn_chr_sym = list(LETTERS[1:2]),
    agn_chr_sep = ""
  )
  no_sep_commas <- lengths(regmatches(no_sep, gregexpr(",", no_sep)))
  expect_equal(no_sep_commas, rep(0L, 50L))

  comma_sep <- gen_col(
    character(),
    elements = 50L,
    agn_unique = FALSE,
    agn_chr_min = 1L, agn_chr_max = 10L,
    agn_chr_sym = list(LETTERS[1:2]),
    agn_chr_sep = ","
  )
  comma_sep_commas <- lengths(regmatches(comma_sep, gregexpr(",", comma_sep)))
  expect_equal(comma_sep_commas, (nchar(comma_sep) - 1)/2)

  # Works also with agn_unique
  set.seed(use.seed)
  comma_sep_uniq <- gen_col(
    character(),
    elements = 10L,
    agn_unique = TRUE,
    agn_chr_min = 1L, agn_chr_max = 3L,
    agn_chr_sym = list(LETTERS[1:2]),
    agn_chr_sep = ",",
    agn_chr_try_unique = TRUE,
    agn_chr_try_unique_attempts = 10L
  )
  comma_sep_uniq_commas <-
    lengths(regmatches(comma_sep_uniq, gregexpr(",", comma_sep_uniq)))
  expect_equal(comma_sep_uniq_commas, (nchar(comma_sep_uniq) - 1)/2)

})

test_that("gen_col handles missingness correctly", {

  # p_na is 0 means no missings
  expect_false(
    any(is.na(gen_col(integer(), elements = 1000L, p_na = 0, agn_int_max = 100)))
  )

  # p_na == 1 means all missings
  expect_true(
    all(is.na(gen_col(integer(), elements = 1000L, p_na = 1, agn_int_max = 100)))
  )

  # Expect warning if p_na > 1, but then all missing
  expect_warning(
    int1 <- gen_col(integer(), elements = 1000L, p_na = 2, agn_int_max = 100)
  )
  expect_true(all(is.na(int1)))

  # Expect SOME missing if p_na < 1 and p_na > 0
  int2 <- gen_col(integer(), elements = 1000L, p_na = 0.5, agn_int_max = 100)
  expect_true(any(is.na(int2)) & !all(is.na(int2)))

})

test_that("gen_col preserves vector types when some values are NA", {

  expect_true(is.integer(gen_col(integer(), elements = 1000L, p_na = 0.5)))  # integer
  expect_true(is.double(gen_col(double(), elements = 1000L, p_na = 0.5)))    # numeric
  expect_true(is.list(gen_col(list(), elements = 1000L, p_na = 0.5)))        # list
  expect_true(is.factor(gen_col(factor(), elements = 1000L, p_na = 0.5)))    # factor
  expect_true(is.logical(gen_col(logical(), elements = 1000L, p_na = 0.5)))  # logical
  expect_identical(                                                          # date
    "Date",
    class(gen_col(as.Date(character()), elements = 1000L, p_na = 0.5))
  )
  expect_identical(                                                          # date-time
    "POSIXct",
    class(gen_col(as.POSIXct(numeric(), origin = "1970-01-01"), elements = 1000L, p_na = 0.5))[1]
  )

})

test_that("gen_col correctly handles control lists", {

  set.seed(349866)
  syn_col_1 <- gen_col(
    factor(),
    agn_fct_lvls = list(levels(iris$Species))
  )

  set.seed(349866)
  syn_col_2 <- gen_col(
    factor(),
    control = list(agn_fct_lvls = list(levels(iris$Species)))
  )

  set.seed(349866)
  syn_col_3 <- gen_col(
    factor(),
    control = list(agn_fct_lvls = list(letters[1:3])),
    agn_fct_lvls = list(levels(iris$Species))
  )

  expect_identical(syn_col_1, syn_col_2)
  expect_identical(syn_col_1, syn_col_3)

})
