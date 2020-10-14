#======================#
#                      #
#### TEST STUBBLISE ####
#                      #
#======================#


### ToDo ###
# - Query use of different seed in later tests?


### Synthesise a reference data.frame ###
set.seed(use_seed)
penguins_stbl_df <- stubblise(penguins, rows = 100L)


### Matching Structure ###
test_that(
  desc = "stubblise maintains structure",
  code = {
    
    ## penguins ##
    # Names #
    expect_identical(
      object = lapply(penguins, names),
      expected = lapply(stubblise(penguins), names),
      label = "names(penguins)"
    )
    # Class #
    expect_identical(
      object = lapply(penguins, class),
      expected = lapply(stubblise(penguins), class),
      label = "class(penguins)"
    )
    
    ## mtcars ##
    # Names #
    expect_identical(
      object = lapply(mtcars, names),
      expected = lapply(stubblise(mtcars), names),
      label = "names(mtcars)"
    )
    # Class #
    expect_identical(
      object = lapply(mtcars, class),
      expected = lapply(stubblise(mtcars), class),
      label = "class(mtcars)"
    )
    
  }
)


### Zero-Length Outputs ###
test_that(
  desc = "stubblise can return data with 0 rows or many",
  code = {
    
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
    
  }
)


### Different Data Structures ###
## tbl_df ##
test_that(
  desc = "stubblise handles tibbles",
  code = {
    
    skip_if_not_installed("tibble", min_v_tibble)
    
    # Tibble #
    penguins_tbl_df <- tibble::as_tibble(penguins)
    
    set.seed(use_seed)
    expect_identical(
      object = penguins_stbl_df,
      expected = as.data.frame(
        stubblise(penguins_tbl_df, rows = 100L)
      )
    )
    
  }
)

## data.table ##
test_that(
  desc = "stubblise handles data.tables",
  code = {
    
    skip_if_not_installed("data.table", min_v_dt)
    
    # Coerce to data.table #
    penguins_dt <- data.table::as.data.table(penguins)
    
    set.seed(use_seed)
    expect_identical(
      object = penguins_stbl_df,
      expected = as.data.frame(
        stubblise(penguins_dt, rows = 100L)
      )
    )
    
  }
)

## data.frame ##
test_that(
  desc = "stubblise handles data.frames",
  code = {
    
    # Coerce to data.frame #
    penguins_dat <- as.data.frame(penguins)
    
    set.seed(use_seed)
    expect_identical(
      object = penguins_stbl_df,
      expected = as.data.frame(
        stubblise(penguins_dat, rows = 100L)
      )
    )
    
  }
)

## list ##
test_that(
  desc = "stubblise handles lists",
  code = {
    
    # Coerce to list #
    penguins_list <- as.list(penguins)
    
    # List of vectors without identical lengths #
    penguins_list_uneven <- penguins_list
    penguins_list_uneven[[2]] <- penguins_list_uneven[[2]][1:10]
    
    set.seed(use_seed)
    expect_identical(
      object = penguins_stbl_df,
      expected = as.data.frame(
        stubblise(penguins_list, rows = 100L)
      )
    )
    
    set.seed(use_seed)
    expect_identical(
      object = penguins_stbl_df,
      expected = as.data.frame(
        stubblise(penguins_list_uneven, rows = 100L)
      )
    )
    
  }
)


### Control Lists ###
test_that(
  desc = "stubblise correctly handles control lists",
  code = {
    
    set.seed(2342343)
    syn_penguins_1 <- stubblise(
      penguins,
      agn_fct_lvls = list(levels(penguins$species))
    )
    
    set.seed(2342343)
    syn_penguins_2 <- stubblise(
      penguins,
      ctrl = list(agn_fct_lvls = list(levels(penguins$species)))
    )
    
    set.seed(2342343)
    syn_penguins_3 <- stubblise(
      penguins,
      ctrl = list(agn_fct_lvls = list(letters[1:3])),
      agn_fct_lvls = list(levels(penguins$species))
    )
    
    expect_identical(
      object = syn_penguins_1,
      expected = syn_penguins_2
    )
    expect_identical(
      object = syn_penguins_1,
      expected = syn_penguins_3
    )
    
  }
)


### Alias ###
test_that(
  desc = "stubblise and stubblize are equivalent",
  code = {
    
    expect_identical(
      object = stubblise,
      expected = stubblize
    )
    
  }
)
