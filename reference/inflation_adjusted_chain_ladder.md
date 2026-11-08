# Chain ladder for claims inflation

The chain ladder of a triangle restated in today's money: past payments
are brought to the level of the latest calendar period by past claims
inflation, developed, and the future payments are then inflated by the
expected future inflation, so that the reserve reflects inflation that
differs from the past's.

## Usage

``` r
inflation_adjusted_chain_ladder(
  triangle,
  past = 0,
  future = if (length(past) == 1) past else 0,
  ...
)
```

## Arguments

- triangle:

  A triangle of cumulative claims, as for
  [`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md);
  paid claims, as incurred claims include case reserves that are not
  inflated by period.

- past:

  The past claims inflation: one rate per calendar period (0.05 for 5% a
  period), or a rate for each step between consecutive calendar periods,
  oldest first (one fewer than the number of calendar periods).

- future:

  The future claims inflation per calendar period: one rate, or a rate
  for each future calendar period, nearest first. By default the same
  single rate as `past` when that is one rate, otherwise 0.

- ...:

  Arguments passed to
  [`chain_ladder()`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md)
  for the restated triangle, such as `average` or `tail`.

## Value

An object of class `netsimr_reserve`: a list with `by_origin`, a data
frame with each origin's latest claims, ultimate and IBNR, the IBNR in
today's money (before future inflation) and the cost of future
inflation; `total`; `chain_ladder`, the chain ladder of the restated
triangle; and `restated`, the triangle in today's money.

## Details

The chain ladder of nominal claims projects the average inflation of the
past into the future. Here each incremental payment of calendar period
\\c\\ is multiplied by the inflation from \\c\\ to the latest calendar
period, the chain ladder of the restated cumulative triangle projects
the future increments in today's money, and each is multiplied by the
future inflation from the latest calendar period to its own. With no
inflation the result is the chain ladder's. A tail is paid in the
calendar period after the triangle's last development period.

## See also

[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md).

## Examples

``` r
# 5% a year in the past, 8% expected in the future
inflation_adjusted_chain_ladder(raa_triangle, past = 0.05, future = 0.08)
#> Chain ladder for claims inflation 
#> 
#>  origin  latest ultimate   ibnr ibnr_today future_inflation
#>    1981  18,834   18,834      0          0                0
#>    1982  16,704   16,857    153        142               11
#>    1983  23,466   24,102    636        573               63
#>    1984  27,067   28,797  1,730      1,529              200
#>    1985  26,180   29,120  2,940      2,531              409
#>    1986  15,852   19,796  3,944      3,390              554
#>    1987  12,314   18,128  5,814      4,907              907
#>    1988  13,112   24,880 11,768      9,725            2,043
#>    1989   5,395   16,870 11,475      9,319            2,156
#>    1990   2,063   19,329 17,266     13,701            3,565
#>   Total 160,987  216,713 55,726     45,817            9,909

# the same inflation for both is close to the plain chain ladder
inflation_adjusted_chain_ladder(raa_triangle, past = 0.05)$total[["ibnr"]]
#> [1] 51775.38
chain_ladder(raa_triangle)$total[["ibnr"]]
#> [1] 52135.23
```
