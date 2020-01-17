
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stubble

<!-- badges: start -->

<!-- badges: end -->

stubble helps you generate simple synthetic datasets matching the format
of a supplied data frame-like object (including base R data frames,
tibbles, data.tables, and lists of vectors).

stubble replicates the column names and types of the original data, but
rows contain only random data.

The original intended use of stubble is to generate simple test data for
analysis projects and R package development.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bjcairns/stubble")
```

## Example

Here is a very simple example of using stubble on the `iris` dataset. By
default, stubble generates nonsense values which bear no relation to the
original values, other than having the same vector type.

``` r
library(stubble)
stubblise(iris)
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1      14.22267    71.97860    60.872462   76.821229       c
#> 2      70.68800    99.67255    73.174538   83.916148       b
#> 3      42.51078    79.46222    48.873406   86.666664       d
#> 4      22.30129    55.17357     2.139613   87.888812       a
#> 5      31.48242    75.46695    39.898222    8.257759       b
#> 6      57.30266    37.67906    61.809849   41.909280       c
#> 7      31.89043    51.78012     8.403783   89.241951       c
#> 8      34.09865    51.84473    35.528298   41.264489       b
#> 9      80.97297    68.56383    48.563535   64.267011       b
#> 10     61.16300    20.80409    51.426009   91.121066       b
```

For more advanced use, see the “Using stubble” vignette.

## Known issues

  - See [Issues](https://github.com/bjcairns/stubble/issues) on the
    stubble GitHub repository.
