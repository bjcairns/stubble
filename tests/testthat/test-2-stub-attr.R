#======================#
#                      #
#### TEST STUB ATTR ####
#                      #
#======================#


### ToDo ###
# - Test the emp_sw override for when emp_sw == 0 or 1.


### Params ###
## Control ##
ctrl_def <- list(
  emp_n_exc = 0L,
  emp_p_exc = 0,
  emp_drop_lev = FALSE,
  emp_fuzz_samp = 0,
  emp_tail_exc = 0,
  emp_fuzz_spl = 0
)
ctrl_def <- ctrl_def[!{names(ctrl_def) %in% "index"}]


## Vars ##
vars_base <- c("character", "Date", "double", "factor", "integer", "logical", "ordered", "POSIXct", "POSIXlt")
vars_bit64 <- "integer64"
vars_dt <- c("IDate", "ITime")


### Output Structure ###
test_that(
  desc = "Output Structure [base].",
  code = {
    expect_equivalent(
      object = lengths(lapply(X = luniq[vars_base], FUN = stub_attr, ctrl = ctrl_def)),
      expected = rep(4L, length(luniq[vars_base])),
      label = "output dimensions"
    )
    expect_equivalent(
      object = sapply(X = lapply(X = luniq[vars_base], FUN = stub_attr, ctrl = ctrl_def), FUN = names),
      expected = matrix(
        data = rep(c("dtype", "n", "p_na", "sim"), length(luniq[vars_base])),
        ncol = length(luniq[vars_base])
      ),
      label = "output names"
    )
  }
)


### 'elements' Argument ###
## base ##
test_that(
  desc = "'elements' Argument [base].",
  code = {
    expect_identical(
      object = sapply(
        X = lapply(X = luniq[vars_base], FUN = stub_attr, ctrl = ctrl_def),
        FUN = `[[`, "n"
      ),
      expected = lengths(luniq[vars_base]),
      label = "automatic length outputs"
    )
    expect_equivalent(
      object = sapply(
        X = lapply(X = luniq[vars_base], FUN = stub_attr, elements = 0L, ctrl = ctrl_def),
        FUN = `[[`, "n"
      ),
      expected = rep(0L, length(luniq[vars_base])),
      label = "zero-length outputs"
    )
  }
)

## bit64 ##
test_that(
  desc = "'elements' Argument [bit64].",
  code = {
    expect_identical(
      object = sapply(
        X = lapply(X = luniq[vars_bit64], FUN = stub_attr, ctrl = ctrl_def),
        FUN = `[[`, "n"
      ),
      expected = lengths(luniq[vars_bit64]),
      label = "automatic length outputs"
    )
    expect_equivalent(
      object = sapply(
        X = lapply(X = luniq[vars_bit64], FUN = stub_attr, elements = 0L, ctrl = ctrl_def),
        FUN = `[[`, "n"
      ),
      expected = rep(0L, length(luniq[vars_bit64])),
      label = "zero-length outputs"
    )
  }
)

## base ##
test_that(
  desc = "'elements' Argument [data.table].",
  code = {
    expect_identical(
      object = sapply(
        X = lapply(X = luniq[vars_dt], FUN = stub_attr, ctrl = ctrl_def),
        FUN = `[[`, "n"
      ),
      expected = lengths(luniq[vars_dt]),
      label = "automatic length outputs"
    )
    expect_equivalent(
      object = sapply(
        X = lapply(X = luniq[vars_dt], FUN = stub_attr, elements = 0L, ctrl = ctrl_def),
        FUN = `[[`, "n"
      ),
      expected = rep(0L, length(luniq[vars_dt])),
      label = "zero-length outputs"
    )
  }
)


### 'method' Argument ###
## base ##
test_that(
  desc = "'method' Argument [base].",
  code = {
    expect_equivalent(
      object = sapply(
        X = lapply(
          X = lapply(
            X = luniq[vars_base],
            FUN = stub_attr,
            method = "agnostic",
            ctrl = ctrl_def
          ),
          FUN = `[[`, "sim"
        ),
        FUN = `[[`, "method"
      ),
      expected = rep("agnostic", length(luniq[vars_base])),
      label = "agnostic"
    )
    expect_true(
      object = all(sapply(
        X = lapply(
          X = lapply(
            X = luniq[vars_base],
            FUN = stub_attr,
            method = "empirical",
            ctrl = ctrl_def
          ),
          FUN = `[[`, "sim"
        ),
        FUN = `[[`, "method"
      ) %in% c("sample", "spline")),
      label = "empirical"
    )
  }
)

## bit64 ##
test_that(
  desc = "'method' Argument [bit64].",
  code = {
    skip_if_not_installed("bit64", min_v_bit64)
    expect_equal(
      object = stub_attr(luniq[["integer64"]], method = "agnostic", ctrl = ctrl_def)[["sim"]][["method"]],
      expected = "agnostic",
      label = "agnostic"
    )
    expect_true(
      object = stub_attr(luniq[["integer64"]], method = "empirical", ctrl = ctrl_def)[["sim"]][["method"]] %in% c("sample", "spline"),
      label = "empirical"
    )
  }
)

## data.table ##
test_that(
  desc = "'method' Argument [data.table].",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_equivalent(
      object = sapply(
        X = lapply(
          X = lapply(
            X = luniq[vars_dt],
            FUN = stub_attr,
            method = "agnostic",
            ctrl = ctrl_def
          ),
          FUN = `[[`, "sim"
        ),
        FUN = `[[`, "method"
      ),
      expected = rep("agnostic", length(luniq[vars_dt])),
      label = "agnostic"
    )
    expect_true(
      object = all(sapply(
        X = lapply(
          X = lapply(
            X = luniq[vars_dt],
            FUN = stub_attr,
            method = "empirical",
            ctrl = ctrl_def
          ),
          FUN = `[[`, "sim"
        ),
        FUN = `[[`, "method"
      ) %in% c("sample", "spline")),
      label = "empirical"
    )
  }
)


### Zero-Length Inputs ###
## base ##
test_that(
  desc = "Zero-Length Inputs [base].",
  code = {
    expect_true(
      object = all(lengths(lapply(X = l0[vars_base], FUN = stub_attr, ctrl = ctrl_def)) == 4L),
      label = "output lengths"
    )
    expect_equivalent(
      object = sapply(X = lapply(X = l0[vars_base], FUN = stub_attr, ctrl = ctrl_def), FUN = names),
      expected = matrix(
        data = rep(c("dtype", "n", "p_na", "sim"), length(l0[vars_base])),
        ncol = length(l0[vars_base])
      ),
      label = "output names"
    )
  }
)

## bit64 ##
test_that(
  desc = "Zero-Length Inputs [bit64].",
  code = {
    skip_if_not_installed("bit64", min_v_bit64)
    expect_true(
      object = all(lengths(lapply(X = l0[vars_bit64], FUN = stub_attr, ctrl = ctrl_def)) == 4L),
      label = "output lengths"
    )
    expect_equivalent(
      object = sapply(X = lapply(X = l0[vars_bit64], FUN = stub_attr, ctrl = ctrl_def), FUN = names),
      expected = matrix(
        data = rep(c("dtype", "n", "p_na", "sim"), length(l0[vars_bit64])),
        ncol = length(l0[vars_bit64])
      ),
      label = "output names"
    )
  }
)

## data.table ##
test_that(
  desc = "Zero-Length Inputs [data.table].",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_true(
      object = all(lengths(lapply(X = l0[vars_bit64], FUN = stub_attr, ctrl = ctrl_def)) == 4L),
      label = "output lengths"
    )
    expect_equivalent(
      object = sapply(X = lapply(X = l0[vars_dt], FUN = stub_attr, ctrl = ctrl_def), FUN = names),
      expected = matrix(
        data = rep(c("dtype", "n", "p_na", "sim"), length(l0[vars_dt])),
        ncol = length(l0[vars_dt])
      ),
      label = "output names"
    )
  }
)


### Missing Data Vectors ###
## base ##
test_that(
  desc = "Missing Data Vectors [base].",
  code = {
    expect_true(
      object = all(lengths(lapply(X = lna[vars_base], FUN = stub_attr, ctrl = ctrl_def)) == 4L),
      label = "output lengths"
    )
    expect_equivalent(
      object = sapply(X = lapply(X = lna[vars_base], FUN = stub_attr, ctrl = ctrl_def), FUN = names),
      expected = matrix(
        data = rep(c("dtype", "n", "p_na", "sim"), length(lna[vars_base])),
        ncol = length(lna[vars_base])
      ),
      label = "output names"
    )
  }
)

## bit64 ##
test_that(
  desc = "Missing Data Vectors [bit64].",
  code = {
    skip_if_not_installed("bit64", min_v_bit64)
    expect_true(
      object = all(lengths(lapply(X = lna[vars_bit64], FUN = stub_attr, ctrl = ctrl_def)) == 4L),
      label = "output lengths"
    )
    expect_equivalent(
      object = sapply(X = lapply(X = lna[vars_bit64], FUN = stub_attr, ctrl = ctrl_def), FUN = names),
      expected = matrix(
        data = rep(c("dtype", "n", "p_na", "sim"), length(lna[vars_bit64])),
        ncol = length(lna[vars_bit64])
      ),
      label = "output names"
    )
  }
)

## data.table ##
test_that(
  desc = "Missing Data Vectors [data.table].",
  code = {
    skip_if_not_installed("data.table", min_v_dt)
    expect_true(
      object = all(lengths(lapply(X = lna[vars_bit64], FUN = stub_attr, ctrl = ctrl_def)) == 4L),
      label = "output lengths"
    )
    expect_equivalent(
      object = sapply(X = lapply(X = lna[vars_dt], FUN = stub_attr, ctrl = ctrl_def), FUN = names),
      expected = matrix(
        data = rep(c("dtype", "n", "p_na", "sim"), length(lna[vars_dt])),
        ncol = length(lna[vars_dt])
      ),
      label = "output names"
    )
  }
)


### Tidy Up ###
rm(ctrl_def, vars_base, vars_bit64, vars_dt)
