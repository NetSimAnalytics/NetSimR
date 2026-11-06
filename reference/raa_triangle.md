# RAA triangle of incurred claims

The cumulative incurred claims of the Reinsurance Association of America
for accident years 1981 to 1990, in thousands: the triangle of Mack
(1994), much used to compare reserving methods.

## Usage

``` r
raa_triangle
```

## Format

A numeric matrix with 10 rows (accident years 1981 to 1990) and 10
columns (development years 1 to 10), `NA` for the future.

## Source

Mack, T. (1994). Measuring the variability of chain ladder reserve
estimates. Casualty Actuarial Society Forum, Spring 1994, 101-182.

## See also

[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md).

## Examples

``` r
raa_triangle
#>       development
#> origin    1     2     3     4     5     6     7     8     9    10
#>   1981 5012  8269 10907 11805 13539 16181 18009 18608 18662 18834
#>   1982  106  4285  5396 10666 13782 15599 15496 16169 16704    NA
#>   1983 3410  8992 13873 16141 18735 22214 22863 23466    NA    NA
#>   1984 5655 11555 15766 21266 23425 26083 27067    NA    NA    NA
#>   1985 1092  9565 15836 22169 25955 26180    NA    NA    NA    NA
#>   1986 1513  6445 11702 12935 15852    NA    NA    NA    NA    NA
#>   1987  557  4020 10946 12314    NA    NA    NA    NA    NA    NA
#>   1988 1351  6947 13112    NA    NA    NA    NA    NA    NA    NA
#>   1989 3133  5395    NA    NA    NA    NA    NA    NA    NA    NA
#>   1990 2063    NA    NA    NA    NA    NA    NA    NA    NA    NA
chain_ladder(raa_triangle)$total
#>       latest    developed     ultimate         ibnr           se           cv 
#> 1.609870e+05 7.553740e-01 2.131222e+05 5.213523e+04 2.688074e+04 5.155965e-01 
#>   process_se parameter_se 
#> 2.491727e+04 1.008484e+04 
```
