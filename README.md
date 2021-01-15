
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stubble

<!-- badges: start -->

<!-- badges: end -->

stubble helps you generate simple synthetic datasets matching the format
of a supplied data frame-like object (including base R data frames,
tibbles, data.tables, and lists of vectors).

stubble replicates the column names and types of the original data, but
the synthetic data are randomly generated. By default, these values are
completely random and contain no information about the original data,
but it is also possible to tell stubble to draw synthetic values from
the empirical distributions of the original data.

The original intended use of stubble was to generate simple test data
for analysis projects and R package development. It could also be used
in teaching, methods research, study design, and any other context where
realistic-but-fake data may be useful.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("bjcairns/stubble")
```

## Example

Here is a very simple example of using stubble on the included
`penguins_ext` dataset (derived from
[palmerpenguins](https://github.com/allisonhorst/palmerpenguins)).

``` r
library(stubble)

# Example with several data types 
# (character, factor, double, integer, logical and Date)
p <- penguins_ext[, c(
       "id", 
       "species", 
       "bill_length_mm", 
       "body_mass_g", 
       "clutch_completion", 
       "date_egg"
     )]

head(p)
#>     id species bill_length_mm body_mass_g clutch_completion   date_egg
#> 1 N1A1  Adelie           39.1        3750              TRUE 2007-11-11
#> 2 N1A2  Adelie           39.5        3800              TRUE 2007-11-11
#> 3 N2A1  Adelie           40.3        3250              TRUE 2007-11-16
#> 4 N2A2  Adelie             NA          NA              TRUE 2007-11-16
#> 5 N3A1  Adelie           36.7        3450              TRUE 2007-11-16
#> 6 N3A2  Adelie           39.3        3650              TRUE 2007-11-16
```

The default in stubble is to maintain strict data protection. Unless you
tell it otherwise, it generates nonsense values which bear no relation
to the original values, other than having the same vector type.

``` r

# stubblise to obtain a dataset with the same structure, but random data
p_stbl <- stubblise(p)
head(p_stbl)
#>         id species bill_length_mm body_mass_g clutch_completion   date_egg
#> 1 *<n b#Ac       b      68.226910          42              TRUE 1972-06-23
#> 2   5zp3>j       a      93.392560          56             FALSE 2000-08-31
#> 3      a:#       b       5.276353          56              TRUE 1984-05-02
#> 4  |!9SQp5       b       3.560817          62             FALSE 1998-04-27
#> 5 H}&}kY^W       c      12.401692          56             FALSE 2002-06-20
#> 6        N       d      77.027949          44              TRUE 1991-07-04
```

More advanced use is also possible, such as generating values from the
empirical distributions of each variable.

``` r

# Use method = "empirical" to obtain data with marginal distributions similar 
# to the original. The emp_p_exc and emp_n_exc control parameters allow that 
# all values of the id variable have only a small number of observations.
p_stbl_emp <- stubblise(p, method = "empirical", emp_p_exc = 0, emp_n_exc = 0)
head(p_stbl_emp)
#>      id species bill_length_mm body_mass_g clutch_completion   date_egg
#> 1 N88A2  Gentoo       41.47851        3393              TRUE 2008-02-11
#> 2  N1A2  Adelie       48.18967        5247              TRUE 2008-07-07
#> 3 N28A1  Adelie       47.11368        3592              TRUE 2007-12-12
#> 4 N51A1  Gentoo       42.78341        3363              TRUE 2008-02-17
#> 5 N58A2  Adelie       45.53278        4202              TRUE 2008-03-02
#> 6 N27A1  Adelie       49.82995        3780              TRUE 2009-08-12
```

See the “Using stubble” and "".

## Known issues

  - See [Issues](https://github.com/bjcairns/stubble/issues) on the
    stubble GitHub repository.
