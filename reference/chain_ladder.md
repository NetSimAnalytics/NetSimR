# Chain ladder reserves with Mack's standard errors

Projects a triangle of cumulative claims to ultimate with the chain
ladder, and estimates the standard error of each origin's reserve and of
the total with Mack's distribution-free model, including the tail.

## Usage

``` r
chain_ladder(
  triangle,
  average = c("volume", "simple", "regression"),
  periods = NULL,
  exclude = NULL,
  tail = 1,
  tail_from = 1,
  sigma = c("log-linear", "Mack")
)
```

## Arguments

- triangle:

  A numeric matrix of cumulative claims: one row per origin period,
  oldest first, and one column per development period, with `NA` for the
  future. A data frame is converted with
  [`as.matrix()`](https://rdrr.io/r/base/matrix.html); see
  [`as_triangle`](https://netsimanalytics.github.io/NetSimR/reference/as_triangle.md)
  to build one from a table of claims.

- average:

  `"volume"` (the default) for volume-weighted development factors,
  `"simple"` for simple averages of the link ratios, or `"regression"`
  for least squares through the origin.

- periods:

  The number of latest calendar periods (diagonals) whose link ratios
  are used, or `NULL` (the default) for all of them.

- exclude:

  Link ratios to leave out: a logical matrix with a row per origin and a
  column per development factor (`TRUE` to leave out), or `NULL` (the
  default) for none.

- tail:

  The tail factor from the last development period to ultimate: a
  positive number (1, the default, for none), or `"exponential"` or
  `"inverse power"` for a curve fitted to the development factors above
  1 and extrapolated over 100 periods.

- tail_from:

  The first development factor the tail curve is fitted to.

- sigma:

  How \\\sigma\\ is found for a period with a single link ratio:
  `"log-linear"` or `"Mack"`.

## Value

An object of class `netsimr_chain_ladder`, a list with:

- by_origin:

  a data frame with, for each origin, the latest claims, the share
  developed (latest over ultimate), the ultimate, the IBNR (ultimate
  less latest: for a paid triangle, the IBNR and case reserves), its
  Mack standard error and coefficient of variation, and the process and
  parameter parts of the standard error;

- total:

  the same for all origins together;

- factors, cdf, tail:

  the development factors, the cumulative development factor from each
  period to ultimate (with the tail) and the tail factor;

- sigma, factor_se:

  Mack's \\\sigma\\ and the standard error of each factor, the tail's
  last;

- link_ratios, used:

  the link ratios and which of them the factors use;

- triangle, full_triangle:

  the triangle and the triangle completed with the projections, with an
  ultimate column;

- notes:

  messages on the choices made, such as an extrapolated \\\sigma\\.

## Details

The development factor from period \\j\\ to \\j + 1\\ is the weighted
average of the link ratios \\F\_{ij} = C\_{i,j+1} / C\_{ij}\\ that are
used, with weights \\C\_{ij}^\alpha\\: \\\alpha = 1\\ for the
volume-weighted average (the usual chain ladder), 0 for the simple
average and 2 for the regression through the origin. Link ratios can be
left out with `exclude`, or by using only the latest `periods` calendar
periods. Link ratios from claims that are zero or negative are always
left out.

The standard errors follow Mack (1993, 1999): the variance parameter of
each period is \\\sigma_j^2 = \sum w\_{ij} C\_{ij}^\alpha (F\_{ij} -
f_j)^2 / (n_j - 1)\\, and the reserve's mean squared error is the sum of
the process and the parameter error, found recursively; the total adds
the correlation of the origins through the shared factors. Where a
period has a single link ratio, its \\\sigma\\ is extrapolated: by a
log-linear regression on the others (`"log-linear"`, the default,
falling back to Mack's rule when that regression is not significant at
5%), or by Mack's rule \\\sigma\_{n-1}^2 = \min(\sigma\_{n-2}^4 /
\sigma\_{n-3}^2, \sigma\_{n-3}^2, \sigma\_{n-2}^2)\\ (`"Mack"`). With a
tail factor, the tail's standard error and \\\sigma\\ are extrapolated
by log-linear regressions to where a curve through the factors reaches
the tail factor. These choices are those of the ChainLadder package's
`MackChainLadder()`, whose results the function reproduces.

## References

Mack, T. (1993). Distribution-free calculation of the standard error of
chain ladder reserve estimates. ASTIN Bulletin 23(2), 213-225.

Mack, T. (1999). The standard error of chain ladder reserve estimates:
recursive calculation and inclusion of a tail factor. ASTIN Bulletin
29(2), 361-366.

## See also

[`bornhuetter_ferguson`](https://netsimanalytics.github.io/NetSimR/reference/bornhuetter_ferguson.md),
[`cape_cod`](https://netsimanalytics.github.io/NetSimR/reference/cape_cod.md)
and
[`bootstrap_chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/bootstrap_chain_ladder.md);
[`run_shiny_reserving_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_reserving_tool.md),
an application for all of them.

## Examples

``` r
cl <- chain_ladder(raa_triangle)
cl
#> Chain ladder 
#> 
#>  origin  latest developed ultimate   ibnr     se    cv process_se parameter_se
#>    1981  18,834     1.000   18,834      0      0                0            0
#>    1982  16,704     0.991   16,858    154    143 0.928        104           98
#>    1983  23,466     0.974   24,083    617    592 0.959        451          383
#>    1984  27,067     0.943   28,703  1,636    713 0.436        530          476
#>    1985  26,180     0.905   28,927  2,747  1,452 0.529      1,219          790
#>    1986  15,852     0.813   19,501  3,649  1,995 0.547      1,820          817
#>    1987  12,314     0.694   17,749  5,435  2,204 0.405      2,039          837
#>    1988  13,112     0.546   24,019 10,907  5,354 0.491      4,946        2,051
#>    1989   5,395     0.336   16,045 10,650  6,332 0.595      6,034        1,918
#>    1990   2,063     0.112   18,402 16,339 24,566 1.503     23,464        7,275
#>   Total 160,987     0.755  213,122 52,135 26,881 0.516     24,917       10,085
cl$factors
#>      1-2      2-3      3-4      4-5      5-6      6-7      7-8      8-9 
#> 2.999359 1.623523 1.270888 1.171675 1.113385 1.041935 1.033264 1.016936 
#>     9-10 
#> 1.009217 

# simple averages of the latest five years, with a fitted tail
chain_ladder(raa_triangle, average = "simple", periods = 5, tail = "exponential")$total
#>       latest    developed     ultimate         ibnr           se           cv 
#> 1.609870e+05 6.745770e-01 2.386488e+05 7.766182e+04 2.765635e+04 3.561126e-01 
#>   process_se parameter_se 
#> 2.408836e+04 1.358767e+04 
```
