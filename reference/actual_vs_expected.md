# Actual against expected development

Checks the chain ladder against the claims of the latest calendar
period: the chain ladder of the triangle without its latest diagonal
predicts how much each origin develops in that period, which is compared
with what was reported.

## Usage

``` r
actual_vs_expected(triangle, ...)
```

## Arguments

- triangle:

  A triangle of cumulative claims, as for
  [`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md).

- ...:

  Arguments passed to
  [`chain_ladder()`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md)
  for the earlier triangle, such as `average` or `periods`.

## Value

An object of class `netsimr_reserve`: a list with `by_origin`, a data
frame with each origin's claims at its previous age, the expected and
the actual development, their difference, the ratio of actual to
expected, the standard error of the expected development and the
standardised difference; `total`, the same for all origins; and
`chain_ladder`, the chain ladder of the earlier triangle.

## Details

The triangle without its latest diagonal (each origin's latest value
left out) gives development factors \\f_k\\ and Mack's \\\sigma_k\\ and
factor standard errors. An origin with \\C\\ claims at its previous age
\\k\\ was expected to develop by \\C (f_k - 1)\\, with a standard error
\\\sqrt{C^{2 - \alpha} \sigma_k^2 + C^2 se(f_k)^2}\\ (process and
parameter error, \\\alpha\\ as in
[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md)).
The standardised difference (actual less expected, over its standard
error) shows which origins moved more than the method allows for: beyond
about 2 in either direction is worth a look. The total's standard error
adds that the origins at the same age share a factor. An origin that has
a single value, or whose previous age has no development factor in the
earlier triangle (the oldest origin, at the end of the triangle), has no
expected development and is left out of the total.

## See also

[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md);
[`merz_wuthrich`](https://netsimanalytics.github.io/NetSimR/reference/merz_wuthrich.md)
for the uncertainty of the next period's development.

## Examples

``` r
ave <- actual_vs_expected(raa_triangle)
ave
#> Actual vs expected development of the latest calendar period 
#> 
#>  origin previous expected actual difference actual_to_expected     se      z
#>    1981   18,662             172                                            
#>    1982   16,169       47    535        488             11.402    120  4.050
#>    1983   22,863      868    603       -265              0.695    182 -1.456
#>    1984   26,083    1,147    984       -163              0.858  1,535 -0.106
#>    1985   25,955    3,958    225     -3,733              0.057  1,016 -3.675
#>    1986   12,935    2,111  2,917        806              1.382    992  0.813
#>    1987   10,946    3,203  1,368     -1,835              0.427  3,124 -0.587
#>    1988    6,947    4,092  6,165      2,073              1.507  3,073  0.675
#>    1989    3,133    6,935  2,262     -4,673              0.326 10,651 -0.439
#>    1990                                                                     
#>   Total  125,031   22,360 15,059     -7,301              0.673 11,708 -0.624
ave$by_origin[, c("origin", "expected", "actual", "z")]
#>    origin   expected actual          z
#> 1    1981         NA    172         NA
#> 2    1982   46.92208    535  4.0504517
#> 3    1983  867.98197    603 -1.4557689
#> 4    1984 1146.81339    984 -0.1060883
#> 5    1985 3958.19260    225 -3.6751055
#> 6    1986 2110.82325   2917  0.8128652
#> 7    1987 3203.06059   1368 -0.5873377
#> 8    1988 4091.89296   6165  0.6746367
#> 9    1989 6934.62805   2262 -0.4386906
#> 10   1990         NA     NA         NA
```
