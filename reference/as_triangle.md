# Build a claims triangle from a table

Makes the triangle of cumulative claims that the reserving functions
take, from a table with a row per origin and development period (such as
payments by accident year and development year), or from a table with a
row per origin and a column per development period.

## Usage

``` r
as_triangle(
  data,
  origin = "origin",
  development = "development",
  value = "value",
  cumulative = TRUE
)
```

## Arguments

- data:

  A data frame: in the long form with origin, development and value
  columns; in the wide form with a row per origin and a column per
  development period (and the origin in the column named by `origin`, or
  in the row names).

- origin, development, value:

  The names of the origin, development period and value columns of the
  long form. For the wide form, give `development = NULL` and
  `value = NULL`, and `origin` the name of the origin column or `NULL`
  for the row names.

- cumulative:

  `TRUE` (the default) if the values are cumulative, `FALSE` if they are
  incremental.

## Value

A numeric matrix of cumulative claims, one row per origin and one column
per development period, with `NA` for the future.

## Details

In the long form, the origin and development periods are each sorted and
taken to be consecutive: the development periods can be numbered (1, 2,
...) or ages (12, 24, ... months). Incremental values of the same origin
and development period are added up, so claim transactions can be given
as they are; a cell of the past with no value is then zero. The past is
every cell on or before the latest calendar diagonal in the data.
Cumulative values must have one value per cell of the past.

In the wide form, the values of each origin run from the first
development period, with `NA` (or empty) for the future.

## Examples

``` r
head(simulated_motor_claims)
#>   accident_year development_year    paid incurred earned_premium
#> 1          2014                1 1644095  3709012        8150000
#> 2          2014                2 1502381  1044516        8150000
#> 3          2014                3  714586   164925        8150000
#> 4          2014                4  491949   222467        8150000
#> 5          2014                5  362668    48340        8150000
#> 6          2014                6  249976    64828        8150000
as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid", cumulative = FALSE)
#>       development
#> origin       1       2       3       4       5       6       7       8       9
#>   2014 1644095 3146476 3861062 4353011 4715679 4965655 5063891 5165111 5192337
#>   2015 1879754 3451386 4286565 4805419 5109496 5419496 5546421 5649666 5727854
#>   2016 1948022 3576956 4752049 5456660 5792357 6080841 6193474 6315756 6386889
#>   2017 2248011 3842756 5095157 5668707 6200949 6579528 6766657 6840101 6930336
#>   2018 1871802 3485889 4409419 4995844 5421575 5599291 5767056 5865110      NA
#>   2019 2230676 4045938 4938286 5677863 6027131 6336038 6621118      NA      NA
#>   2020 1841708 3187405 4408760 5136075 5644214 5908704      NA      NA      NA
#>   2021 2260170 4708704 6187873 7194278 7639503      NA      NA      NA      NA
#>   2022 1994480 3882850 4916375 5615887      NA      NA      NA      NA      NA
#>   2023 2160218 4289123 5290187      NA      NA      NA      NA      NA      NA
#>   2024 2764604 5479114      NA      NA      NA      NA      NA      NA      NA
#>   2025 2420850      NA      NA      NA      NA      NA      NA      NA      NA
#>       development
#> origin      10      11      12
#>   2014 5305019 5314769 5322407
#>   2015 5741459 5743751      NA
#>   2016 6457627      NA      NA
#>   2017      NA      NA      NA
#>   2018      NA      NA      NA
#>   2019      NA      NA      NA
#>   2020      NA      NA      NA
#>   2021      NA      NA      NA
#>   2022      NA      NA      NA
#>   2023      NA      NA      NA
#>   2024      NA      NA      NA
#>   2025      NA      NA      NA
```
