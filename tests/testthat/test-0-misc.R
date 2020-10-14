#=================#
#                 #
#### TEST MISC ####
#                 #
#=================#


### Notes ###
# - impute_na() will never need to evaluate vectors containing NAs.
# - Sample chars cannot take NA values as inputs.
# - Doesn't matter if nchar_min > nchar_max in sample_chars().


###################
### impute_na() ###
###################


### Data ###
n <- 10L
syn_col <- rep(1L, n)

## p_na ##
test_that(
  desc = "p_na",
  code = {
    expect_true(
      object = all(!is.na(impute_na(syn_col = syn_col, p_na = 0))),
      label = "p_na == 0"
    )
    expect_true(
      object = all(is.na(impute_na(syn_col = syn_col, p_na = 1))),
      label = "p_na == 1"
    )
  }
)


### Zero-Length Inputs ###
test_that(
  desc = "Zero-Length Inputs",
  code = {
    expect_length(
      object = impute_na(character(0), p_na = 0),
      n = 0
    )
    expect_length(
      object = impute_na(character(0), p_na = 0.5),
      n = 0
    )
    expect_length(
      object = impute_na(character(0), p_na = 1),
      n = 0
    )
  }
)


### Output Lengths ###
test_that(
  desc = "Output Lengths",
  code = {
    expect_length(
      object = impute_na(syn_col[1], p_na = 0),
      n = 1
    )
    expect_length(
      object = impute_na(syn_col[1], p_na = 0.5),
      n = 1
    )
    expect_length(
      object = impute_na(syn_col[1], p_na = 1),
      n = 1
    )
    expect_length(
      object = impute_na(syn_col, p_na = 0),
      n = n
    )
    expect_length(
      object = impute_na(syn_col, p_na = 0.5),
      n = n
    )
    expect_length(
      object = impute_na(syn_col, p_na = 1),
      n = n
    )
  }
); rm(n, syn_col)


### Output Classes ###
## base ##
test_that(
  desc = "output classes [base]",
  code = {
    expect_true(
      object = all(
        is.character(impute_na(syn_col = luniq[["character"]], p_na = 0)),
        is.character(impute_na(syn_col = luniq[["character"]], p_na = 0.5)),
        is.character(impute_na(syn_col = luniq[["character"]], p_na = 1))
      ),
      label = "character"
    )
    expect_true(
      object = all(
        class(impute_na(syn_col = luniq[["Date"]], p_na = 0)) == "Date",
        class(impute_na(syn_col = luniq[["Date"]], p_na = 0.5)) == "Date",
        class(impute_na(syn_col = luniq[["Date"]], p_na = 1)) == "Date"
      ),
      label = "Date"
    )
    expect_true(
      object = all(
        is.double(impute_na(syn_col = luniq[["double"]], p_na = 0)),
        is.double(impute_na(syn_col = luniq[["double"]], p_na = 0.5)),
        is.double(impute_na(syn_col = luniq[["double"]], p_na = 1))
      ),
      label = "double"
    )
    expect_true(
      object = all(
        is.factor(impute_na(syn_col = luniq[["factor"]], p_na = 0)),
        is.factor(impute_na(syn_col = luniq[["factor"]], p_na = 0.5)),
        is.factor(impute_na(syn_col = luniq[["factor"]], p_na = 1))
      ),
      label = "factor"
    )
    expect_true(
      object = all(
        is.integer(impute_na(syn_col = luniq[["integer"]], p_na = 0)),
        is.integer(impute_na(syn_col = luniq[["integer"]], p_na = 0.5)),
        is.integer(impute_na(syn_col = luniq[["integer"]], p_na = 1))
      ),
      label = "integer"
    )
    expect_true(
      object = all(
        is.logical(impute_na(syn_col = luniq[["logical"]], p_na = 0)),
        is.logical(impute_na(syn_col = luniq[["logical"]], p_na = 0.5)),
        is.logical(impute_na(syn_col = luniq[["logical"]], p_na = 1))
      ),
      label = "logical"
    )
    expect_true(
      object = all(
        is.ordered(impute_na(syn_col = luniq[["ordered"]], p_na = 0)),
        is.ordered(impute_na(syn_col = luniq[["ordered"]], p_na = 0.5)),
        is.ordered(impute_na(syn_col = luniq[["ordered"]], p_na = 1))
      ),
      label = "ordered"
    )
    expect_true(
      object = all(
        "POSIXct" %in% class(impute_na(syn_col = luniq[["POSIXct"]], p_na = 0)),
        "POSIXct" %in% class(impute_na(syn_col = luniq[["POSIXct"]], p_na = 0.5)),
        "POSIXct" %in% class(impute_na(syn_col = luniq[["POSIXct"]], p_na = 1))
      ),
      label = "POSIXct"
    )
    expect_true(
      object = all(
        "POSIXlt" %in% class(impute_na(syn_col = luniq[["POSIXlt"]], p_na = 0)),
        "POSIXlt" %in% class(impute_na(syn_col = luniq[["POSIXlt"]], p_na = 0.5)),
        "POSIXlt" %in% class(impute_na(syn_col = luniq[["POSIXlt"]], p_na = 1))
      ),
      label = "POSIXlt"
    )
  }
)

## bit64 ##
test_that(
  desc = "output classes [bit64]",
  code = {
    skip_if_not_installed("bit64")
    expect_true(
      object = all(
        bit64::is.integer64(impute_na(syn_col = luniq[["integer64"]], p_na = 0)),
        bit64::is.integer64(impute_na(syn_col = luniq[["integer64"]], p_na = 0.5)),
        bit64::is.integer64(impute_na(syn_col = luniq[["integer64"]], p_na = 1))
      ),
      label = "integer64"
    )
  }
)

## data.table ##
test_that(
  desc = "output classes [data.table]",
  code = {
    skip_if_not_installed("data.table")
    expect_true(
      object = all(
        "IDate" %in% class(impute_na(syn_col = luniq[["IDate"]], p_na = 0)),
        "IDate" %in% class(impute_na(syn_col = luniq[["IDate"]], p_na = 0.5)),
        "IDate" %in% class(impute_na(syn_col = luniq[["IDate"]], p_na = 1))
      ),
      label = "IDate"
    )
    expect_true(
      object = all(
        class(impute_na(syn_col = luniq[["ITime"]], p_na = 0)) == "ITime",
        class(impute_na(syn_col = luniq[["ITime"]], p_na = 0.5)) == "ITime",
        class(impute_na(syn_col = luniq[["ITime"]], p_na = 1)) == "ITime"
      ),
      label = "ITime"
    )
  }
)


##############################
### is.installed.package() ###
##############################


### Zero-Length Inputs ###
test_that(
  desc = "Zero-Length Inputs",
  code = {
    expect_length(
      object = is.installed.package(character(0)),
      n = 0L
    )
  }
)


### NA Pass Through ###
test_that(
  desc = "NA Pass Through",
  code = {
    expect_identical(
      object = is.installed.package(NA),
      expected = NA,
      label = "NA pass through"
    )
  }
)


### Output Lengths ###
test_that(
  desc = "Output Lengths",
  code = {
    expect_length(
      object = is.installed.package(character(1)),
      n = 1
    )
    expect_length(
      object = is.installed.package(character(2)),
      n = 2
    )
  }
)


### Output Class ###
test_that(
  desc = "Output Class",
  code = {
    expect_true(
      object = is.logical(is.installed.package("base")),
      label = "output class"
    )
  }
)


### Core Functioning ###
test_that(
  desc = "Present Packages",
  code = {
    expect_true(
      object = is.installed.package("base"),
      label = "base"
    )
    expect_false(
      object = is.installed.package("_1"),
      label = "non-existent package"
    )
    expect_identical(
      object = is.installed.package(c("base", "_1")),
      expected = c(TRUE, FALSE),
      label = "output order"
    )
    expect_identical(
      object = is.installed.package(c("_1", "base")),
      expected = c(FALSE, TRUE),
      label = "output order"
    )
  }
)


################
### dtype0() ###
################


### Output Lengths ###
test_that(
  desc = "output lengths",
  code = {
    expect_true(
      object = all(lengths(lapply(l0, dtype0)) == 0L),
      label = "l0"
    )
    expect_true(
      object = all(lengths(lapply(l1, dtype0)) == 0L),
      label = "l1"
    )
    expect_true(
      object = all(lengths(lapply(lna, dtype0)) == 0L),
      label = "lna"
    )
    expect_true(
      object = all(lengths(lapply(luniq, dtype0)) == 0L),
      label = "luniq"
    )
  }
)


### Output Classes ###
## unrecognised ##
test_that(
  desc = "output classes [unrecognised]",
  code = {
    expect_error(
      object = dtype0(NULL),
      regexp = "\\bNo\\smethod\\sexists\\sfor\\sobject\\sof\\sclass\\b",
      label = "NULL"
    )
  }
)

## base ##
test_that(
  desc = "output classes [base]",
  code = {
    expect_identical(
      object = lapply(l0, dtype0),
      expected = l0,
      label = "l0"
    )
    expect_true(
      object = all(is.list(dtype0(l0)), !is.data.frame(l0)),
      label = "list"
    )
    expect_true(
      object = is.data.frame(dtype0(as.data.frame(l0))),
      label = "data.frame"
    )
  }
)

## data.table ##
test_that(
  desc = "output classes [data.table]",
  code = {
    skip_if_not_installed("data.table")
    expect_true(
      object = suppressWarnings(
        data.table::is.data.table(dtype0(data.table::as.data.table(l0)))
      ),
      label = "data.table"
    )
  }
)

## tibble ##
test_that(
  desc = "output classes [tibble]",
  code = {
    skip_if_not_installed("tibble")
    expect_true(
      object = tibble::is_tibble(dtype0(tibble::as_tibble(l0))),
      label = "tibble"
    )
  }
)


######################
### sample_chars() ###
######################


### Zero-Length Inputs ###
test_that(
  desc = "zero-length inputs",
  code = {
    expect_error(
      object = sample_chars(x = character(0), size = 1, nchar_min = 1, nchar_max = 1),
      regexp = "\\binvalid\\s+first\\s+argument\\b",
      label = "sample something from nothing"
    )
    expect_true(
      object = all(nchar(sample_chars(x = character(0), size = 1e3, nchar_min = 0, nchar_max = 0)) == 0),
      label = "sample zero from zero"
    )
    expect_length(
      object = sample_chars(x = character(0), size = 0, nchar_min = 1, nchar_max = 1),
      n = 0L
    )
  }
)


### Output Lengths ###
test_that(
  desc = "output lengths",
  code = {
    expect_length(
      object = sample_chars(x = letters, size = 1),
      n = 1
    )
    expect_length(
      object = sample_chars(x = letters, size = 2),
      n = 2
    )
  }
)


### Output Class ###
test_that(
  desc = "output class",
  code = {
    expect_true(
      object = is.character(sample_chars(x = letters, size = 0))
    )
  }
)


### Core Functioning ###
test_that(
  desc = "core functioning",
  code = {
    expect_true(
      object = all(grepl("^[a-z]+$", sample_chars(x = letters, size = 1e3, nchar_min = 1L))),
      label = "character selection"
    )
    expect_true(
      object = all(nchar(sample_chars(x = letters, size = 1e3, nchar_min = 0, nchar_max = 0)) == 0),
      label = "nchar == 0"
    )
    expect_true(
      object = all(nchar(sample_chars(x = letters, size = 1e3, nchar_min = 1, nchar_max = 1)) == 1),
      label = "nchar == 1"
    )
    expect_true(
      object = all(nchar(sample_chars(x = letters, size = 1e3, nchar_min = 10, nchar_max = 10)) == 10),
      label = "nchar == 10"
    )
    expect_true(
      object = all(nchar(sample_chars(x = letters, size = 1e3, nchar_min = 1, nchar_max = 10)) %in% 1:10),
      label = "nchar %in% 1:10"
    )
  }
)
