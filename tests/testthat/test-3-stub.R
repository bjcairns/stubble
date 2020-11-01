#=================#
#                 #
#### TEST STUB ####
#                 #
#=================#


### Params ###
## Control ##
ctrl_def <- list(
  emp_tail_exc = 0,
  emp_fuzz_spl = 0,
  emp_n_exc = 0L,
  emp_p_exc = 0,
  emp_drop_lev = 0,
  emp_fuzz_samp = 0
)


### Output Structure ###
test_that(
  desc = "Output structure",
  code = {
    expect_identical(
      object = class(stub(l1, ctrl = ctrl_def)),
      expected = "stub",
      label = "Output class"
    )
    expect_length(
      object = stub(l1, ctrl = ctrl_def),
      n = 3L
    )
    expect_identical(
      object = names(stub(l1, ctrl = ctrl_def)),
      expected = c("ctrl", "dtype", "vars"),
      label = "Output names"
    )
    expect_length(
      object = stub(l1, ctrl = ctrl_def)[["ctrl"]],
      n = length(ctrl_def)
    )
    expect_length(
      object = stub(l1, ctrl = ctrl_def)[["dtype"]],
      n = 0L
    )
    expect_length(
      object = stub(l1, ctrl = ctrl_def)[["vars"]],
      n = length(l1)
    )
    expect_identical(
      object = names(stub(l1, ctrl = ctrl_def)[["vars"]]),
      expected = names(l1),
      label = "Variable names"
    )
  }
)


### Data Structure Encoding ###
## base ##
test_that(
  desc = "Data Structure Encoding [base].",
  code = {
    expect_identical(
      object = class(stub(l1, ctrl = ctrl_def)[["dtype"]]),
      expected = "list",
      label = "list"
    )
    expect_identical(
      object = class(stub(as.data.frame(l1), ctrl = ctrl_def)[["dtype"]]),
      expected = "data.frame",
      label = "data.frame"
    )
  }
)

## data.table ##
test_that(
  desc = "Data Structure Encoding [data.table].",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_identical(
      object = class(stub(suppressWarnings(data.table::as.data.table(l1)), ctrl = ctrl_def)[["dtype"]]),
      expected = c("data.table", "data.frame"),
      label = "data.table"
    )
  }
)

## tibble ##
test_that(
  desc = "Data Structure Encoding [tibble].",
  code = {
    skip_if_not_installed("tibble", min_v_tibble)
    expect_identical(
      object = class(stub(tibble::as_tibble(l1), ctrl = ctrl_def)[["dtype"]]),
      expected = c("tbl_df", "tbl", "data.frame"),
      label = "tibble"
    )
  }
)


### 'rows' argument ###
test_that(
  desc = "'rows' Argument.",
  code = {
    expect_identical(
      object = sapply(X = stub(luniq, ctrl = ctrl_def)[["vars"]], FUN = `[[`, "n"),
      expected = lengths(luniq),
      label = "automatic"
    )
    expect_equivalent(
      object = sapply(X = stub(luniq, rows = 10L, ctrl = ctrl_def)[["vars"]], FUN = `[[`, "n"),
      expected = rep(10L, length(luniq)),
      label = "manual (single value)"
    )
    expect_equivalent(
      object = sapply(X = stub(luniq, rows = seq_along(luniq), ctrl = ctrl_def)[["vars"]], FUN = `[[`, "n"),
      expected = seq_along(luniq),
      label = "manual (multiple values)"
    )
  }
)


### 'method' argument ###
test_that(
  desc = "'method' argument",
  code = {
    expect_equivalent(
      object = sapply(
        X = lapply(
          X = stub(luniq, method = "agnostic", ctrl = ctrl_def)[["vars"]],
          FUN = `[[`, "sim"
        ),
        FUN = `[[`, "method"
      ),
      expected = rep("agnostic", length(luniq)),
      label = "agnostic"
    )
    expect_true(
      object = all(sapply(
        X = lapply(
          X = stub(luniq, method = "empirical", ctrl = ctrl_def)[["vars"]],
          FUN = `[[`, "sim"
        ),
        FUN = `[[`, "method"
      ) %in% c("sample", "spline")),
      label = "empirical"
    )
    expect_equivalent(
      object = sapply(
        X = lapply(
          X = stub(luniq, method = rep("agnostic", length(luniq)), ctrl = ctrl_def)[["vars"]],
          FUN = `[[`, "sim"
        ),
        FUN = `[[`, "method"
      ),
      expected = rep("agnostic", length(luniq)),
      label = "'method' vector (agnostic)"
    )
  }
)


### Tidy Up ###
rm(ctrl_def)
