
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stubble

<!-- badges: start -->

<!-- badges: end -->

stubble helps you generate simple synthetic datasets matching the format
of a supplied data frame-like object (including base R data frames,
tibbles, data.tables, and lists of vectors).

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

### Default operation

Because real data are often sensitive and unsuitable for sharing,
stubble produces nonsense data unless you tell it not to. With default
options, the function `stubblise()` (equivalently, `stubblize()`) will
produce a data frame that shares only the column names and types with
the source dataset.

``` r
library(stubble)
stubblise(iris)
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1     25.568783    74.77071    21.186907    88.97505       a
#> 2     28.169458    12.27111    79.616305    22.42311       a
#> 3     31.632333    27.95975    40.408360    24.80044       b
#> 4     85.914181    55.70665     9.320685    35.67728       a
#> 5     39.761448    49.68258    91.067641    28.85033       a
#> 6     63.395060    79.82531     4.422596    99.28586       a
#> 7      9.531892    93.33323    30.339102    70.48209       b
#> 8     78.141349    54.81941    53.200398    23.17261       c
#> 9     66.747679    98.84024    44.244858    11.28218       a
#> 10    65.569869    43.51208    73.961217    22.05888       c
```

Contrast this with the actual `iris` dataset, and note that the
distribution of the values, features like rounding, and factor levels,
etc. are not preserved:

``` r
head(iris, 10)
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1           5.1         3.5          1.4         0.2  setosa
#> 2           4.9         3.0          1.4         0.2  setosa
#> 3           4.7         3.2          1.3         0.2  setosa
#> 4           4.6         3.1          1.5         0.2  setosa
#> 5           5.0         3.6          1.4         0.2  setosa
#> 6           5.4         3.9          1.7         0.4  setosa
#> 7           4.6         3.4          1.4         0.3  setosa
#> 8           5.0         3.4          1.5         0.2  setosa
#> 9           4.4         2.9          1.4         0.2  setosa
#> 10          4.9         3.1          1.5         0.1  setosa
```

### Control parameters

If you did want to use the same factor levels, however, you can do so,
using the relevant control parameters which are available (see
`?gen_col_control` for details).

``` r
stubblise(iris, fct_lvls = list(levels(iris$Species)))
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#> 1     49.758731   59.594768     1.531368   78.636368  virginica
#> 2     72.422227   68.752217    46.042337   85.366629     setosa
#> 3     52.972977   51.567118    63.846013   23.345042     setosa
#> 4     96.708715   95.836232    28.092262   31.508934  virginica
#> 5     49.555034   26.631954    36.906548   49.891221     setosa
#> 6     43.134759   68.310736    42.303992    8.988727 versicolor
#> 7      4.054335   27.984743    18.940767   84.578984     setosa
#> 8     63.052379   37.371892    42.363624   82.308245     setosa
#> 9     11.911299    4.223318    63.353743   80.422769     setosa
#> 10    67.904145   63.732989    69.832666   15.240468 versicolor
```

Going further, control parameters can be specified for each column so as
to produce more realistic looking data:

``` r
mins <- as.numeric(dplyr::summarise_if(iris, is.numeric, min))
maxs <- as.numeric(dplyr::summarise_if(iris, is.numeric, max))
lvls <- levels(iris$Species)
rows <- dplyr::count(iris, Species)$n

syn_iris <- purrr::map2_dfr(
  rows, lvls,
  ~ stubblise(
    iris, rows = .x,
    dbl_min = mins, dbl_max = maxs, dbl_round = 1L,
    fct_lvls = list(lvls), fct_use_lvls = .y
  )
)

head(syn_iris, 10)
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1           6.1         3.8          3.1         2.4  setosa
#> 2           6.1         2.9          2.4         1.0  setosa
#> 3           6.0         4.3          5.8         1.8  setosa
#> 4           4.4         2.4          6.8         0.2  setosa
#> 5           5.4         3.3          3.9         2.1  setosa
#> 6           6.4         2.8          6.3         0.9  setosa
#> 7           6.0         2.2          3.1         0.8  setosa
#> 8           5.6         4.1          4.8         0.7  setosa
#> 9           6.0         3.2          4.6         2.5  setosa
#> 10          7.1         2.2          6.4         2.2  setosa
```

The code above extracts minima and maxima and factor levels, finds the
number of rows for each species, then calls `stubblise()` for each
species (via `purrr::map2_dfr()`) to give the same number of rows per
species as in the original `iris`. Because the `map2_dfr()` function
binds its results into a single data frame, the `Species` columns need
to be compatible. This is ensured by giving a `fct_lvls` parameter
including all 3 levels, while the `fct_use_lvls` parameter includes only
the level for the current species.

Note this important feature of the control parameter syntax: elements of
vector parameters (like `mins`) apply to corresponding columns (first
element to first column, second to second, and so on, with recycling).
If the parameter for a single column is a vector (like `fct_lvls`), then
it should be wrapped in `list()`.

Although the result of the above looks much more realistic, there are
obvious differences. It is easy to see, for example, that the
`Petal.Width` column for setosa differs from the original `iris`
dataset. This is because although different species tend to have
different petal widths in the original data, in the synthetic version
there are no such relationships. There is no facility in stubble to
automatically generate relationships between variables, but different
control parameters for the numeric columns can be passed to
`stubblise()` for each species to achieve this effect manually.

## Known issues

  - It is intended that stubble will support generation of character
    columns with unique values, but this is not currently guaranteed.
