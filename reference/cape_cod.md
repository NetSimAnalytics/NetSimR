# Cape Cod reserves

Bornhuetter-Ferguson reserves whose expected loss ratio is estimated
from the triangle: the latest claims over the premium already earned by
development (the used-up premium).

## Usage

``` r
cape_cod(triangle, premium, decay = 1, ...)
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

- decay:

  The weight of an origin one period away in an origin's loss ratio,
  from 0 to 1 (the default, the classical Cape Cod with one loss ratio).

- ...:

  Arguments passed to
  [`chain_ladder()`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md)
  for the development pattern, such as `average` and `tail`, when
  `triangle` is a triangle.

## Value

An object of class `netsimr_reserve`, as for
[`bornhuetter_ferguson`](https://netsimanalytics.github.io/NetSimR/reference/bornhuetter_ferguson.md),
whose loss ratios are the estimated ones.

## Details

The expected loss ratio is \\LR = \sum_i C_i / \sum_i P_i p_i\\, with
\\p_i = 1 / CDF_i\\ the share developed of origin \\i\\, and each
origin's ultimate is \\C_i + (1 - p_i) P_i LR\\. With `decay` below 1
(the generalised Cape Cod of Gluck), each origin has its own loss ratio,
in which origin \\k\\ weighs \\decay^{\|i - k\|}\\, so that nearby
origins count most; with decay 0 the method is the chain ladder.

## References

Stanard, J. N. (1985). A simulation test of prediction errors of loss
reserve estimation techniques. Proceedings of the Casualty Actuarial
Society 72, 124-148.

Gluck, S. M. (1997). Balancing development and trend in loss reserve
analysis. Proceedings of the Casualty Actuarial Society 84, 482-532.

## Examples

``` r
premium <- c(50000, 52000, 54000, 56000, 58000, 60000, 62000, 64000, 66000, 68000)
cape_cod(raa_triangle, premium)
#> Cape Cod 
#> 
#>  origin  latest premium loss_ratio developed prior_ultimate ultimate   ibnr
#>    1981  18,834  50,000      0.387     1.000         19,350   18,834      0
#>    1982  16,704  52,000      0.387     0.991         20,124   16,888    184
#>    1983  23,466  54,000      0.387     0.974         20,898   24,002    536
#>    1984  27,067  56,000      0.387     0.943         21,672   28,302  1,235
#>    1985  26,180  58,000      0.387     0.905         22,446   28,311  2,131
#>    1986  15,852  60,000      0.387     0.813         23,220   20,197  4,345
#>    1987  12,314  62,000      0.387     0.694         23,994   19,662  7,348
#>    1988  13,112  64,000      0.387     0.546         24,768   24,359 11,247
#>    1989   5,395  66,000      0.387     0.336         25,542   22,349 16,954
#>    1990   2,063  68,000      0.387     0.112         26,316   25,429 23,366
#>   Total 160,987 590,000      0.387                  228,334  228,334 67,347
#>  ultimate_loss_ratio
#>                0.377
#>                0.325
#>                0.444
#>                0.505
#>                0.488
#>                0.337
#>                0.317
#>                0.381
#>                0.339
#>                0.374
#>                0.387
cape_cod(raa_triangle, premium, decay = 0.75)$by_origin$loss_ratio
#>  [1] 0.3953039 0.3978196 0.4091580 0.4145372 0.4073526 0.3876962 0.3734595
#>  [8] 0.3673637 0.3576607 0.3551429
```
