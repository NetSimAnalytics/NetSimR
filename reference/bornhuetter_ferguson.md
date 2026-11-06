# Bornhuetter-Ferguson and Benktander reserves

Reserves from an a priori expected loss ratio: the latest claims plus
the share of the expected ultimate still to emerge, by the chain
ladder's development pattern.

## Usage

``` r
bornhuetter_ferguson(triangle, premium, loss_ratio, iterations = 1, ...)
```

## Arguments

- triangle:

  A triangle of cumulative claims, as for
  [`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md),
  or the result of
  [`chain_ladder()`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md),
  whose development pattern is then used.

- premium:

  The premium (or other exposure) of each origin.

- loss_ratio:

  The a priori expected loss ratio: one for all origins or one for each,
  as a share of the premium (0.7 for 70%).

- iterations:

  1 (the default) for Bornhuetter-Ferguson, 2 for Benktander.

- ...:

  Arguments passed to
  [`chain_ladder()`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md)
  for the development pattern, such as `average` and `tail`, when
  `triangle` is a triangle.

## Value

An object of class `netsimr_reserve`: a list with `by_origin`, a data
frame with the latest claims, premium, a priori loss ratio, share
developed, a priori ultimate, ultimate, IBNR and ultimate loss ratio of
each origin, `total`, the same for all origins together, and
`chain_ladder`, the development pattern.

## Details

With \\p_i = 1 / CDF_i\\ the share developed of origin \\i\\ (the
inverse of its chain ladder cumulative development factor, with the
tail) and \\U_0 = P_i \times LR_i\\ the a priori ultimate, each
iteration gives \\U\_{k} = C_i + (1 - p_i) U\_{k-1}\\. One iteration is
the Bornhuetter-Ferguson method, two the Benktander (iterated
Bornhuetter-Ferguson) method, and as the iterations increase the
ultimate tends to the chain ladder's.

## References

Bornhuetter, R. L. and Ferguson, R. E. (1972). The actuary and IBNR.
Proceedings of the Casualty Actuarial Society 59, 181-195.

Mack, T. (2000). Credible claims reserves: the Benktander method. ASTIN
Bulletin 30(2), 333-347.

## See also

[`cape_cod`](https://netsimanalytics.github.io/NetSimR/reference/cape_cod.md),
which estimates the loss ratio from the triangle, and
[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md).

## Examples

``` r
premium <- c(50000, 52000, 54000, 56000, 58000, 60000, 62000, 64000, 66000, 68000)
bornhuetter_ferguson(raa_triangle, premium, loss_ratio = 0.35)
#> Bornhuetter-Ferguson 
#> 
#>  origin  latest premium loss_ratio developed prior_ultimate ultimate   ibnr
#>    1981  18,834  50,000      0.350     1.000         17,500   18,834      0
#>    1982  16,704  52,000      0.350     0.991         18,200   16,870    166
#>    1983  23,466  54,000      0.350     0.974         18,900   23,950    484
#>    1984  27,067  56,000      0.350     0.943         19,600   28,184  1,117
#>    1985  26,180  58,000      0.350     0.905         20,300   28,108  1,928
#>    1986  15,852  60,000      0.350     0.813         21,000   19,782  3,930
#>    1987  12,314  62,000      0.350     0.694         21,700   18,959  6,645
#>    1988  13,112  64,000      0.350     0.546         22,400   23,284 10,172
#>    1989   5,395  66,000      0.350     0.336         23,100   20,728 15,333
#>    1990   2,063  68,000      0.350     0.112         23,800   23,195 21,132
#>   Total 160,987 590,000      0.350                  206,500  221,894 60,907
#>  ultimate_loss_ratio
#>                0.377
#>                0.324
#>                0.444
#>                0.503
#>                0.485
#>                0.330
#>                0.306
#>                0.364
#>                0.314
#>                0.341
#>                0.376

# Benktander, with the chain ladder of simple averages
cl <- chain_ladder(raa_triangle, average = "simple")
bornhuetter_ferguson(cl, premium, loss_ratio = 0.35, iterations = 2)$total
#>              latest             premium          loss_ratio      prior_ultimate 
#>        1.609870e+05        5.900000e+05        3.500000e-01        2.065000e+05 
#>            ultimate                ibnr ultimate_loss_ratio 
#>        2.271235e+05        6.613646e+04        3.849550e-01 
```
