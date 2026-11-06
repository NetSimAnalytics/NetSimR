# Taylor and Ashe triangle of paid claims

The cumulative paid claims of Taylor and Ashe (1983) for ten accident
years: the triangle of Mack's (1993) paper on the standard error of the
chain ladder.

## Usage

``` r
taylor_ashe_triangle
```

## Format

A numeric matrix with 10 rows (accident years) and 10 columns
(development years), `NA` for the future.

## Source

Taylor, G. C. and Ashe, F. R. (1983). Second moments of estimates of
outstanding claims. Journal of Econometrics 23(1), 37-61.

## See also

[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md).

## Examples

``` r
chain_ladder(taylor_ashe_triangle, sigma = "Mack")$total
#>       latest    developed     ultimate         ibnr           se           cv 
#> 3.435809e+07 6.477898e-01 5.303895e+07 1.868086e+07 2.447095e+06 1.309948e-01 
#>   process_se parameter_se 
#> 1.878292e+06 1.568532e+06 
```
