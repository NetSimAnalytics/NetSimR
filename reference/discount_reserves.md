# Discounted reserves from the chain ladder's payment pattern

The future payments of a paid triangle, as the chain ladder projects
them, by calendar period, discounted with a yield curve: the best
estimate of the liability for incurred claims under IFRS 17, or of the
claims provision under Solvency II (before expenses).

## Usage

``` r
discount_reserves(
  x,
  yield_curve = 0,
  period = 1,
  timing = 0.5,
  tail_periods = 1,
  ...
)
```

## Arguments

- x:

  A triangle of cumulative paid claims, or its
  [`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md).

- yield_curve:

  The annual spot rates, as shares (0.03 for 3%), for terms of 1, 2, 3,
  ... years, or one rate for all terms.

- period:

  The length of a development period in years: 1 (the default) for an
  annual triangle, 0.25 for a quarterly one.

- timing:

  When the payments of a period are made, as a share of the period: 0.5
  (the default) for the middle, 0 for its start, 1 for its end.

- tail_periods:

  The number of periods over which the claims of a tail factor are paid.

- ...:

  Arguments of
  [`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md)
  when `x` is a triangle.

## Value

An object of class `netsimr_discounted`, a list with:

- by_origin:

  a data frame with each origin's reserve (its IBNR: the undiscounted
  future payments), the reserve discounted, the discount and the mean
  term of its payments in years;

- cash_flows:

  a data frame with a row per future calendar period: its payments,
  their term, the spot rate, the discount factor and their present
  value;

- total:

  the reserve, the discounted reserve, the discount and the mean term.

## Details

Each origin's projected payments of each future development period fall
in a future calendar period, the first being the one after the
triangle's latest diagonal. A payment of calendar period \\t\\ is taken
to be made `timing` of the way through it, at \\(t - 1 + \code{timing})
\times \code{period}\\ years, and discounted with the spot rate of that
term, \\(1 + r)^{-\textrm{term}}\\. The claims of a tail factor beyond
the triangle are paid evenly over the `tail_periods` periods after its
last development period.

The spot rates of the yield curve are given for terms of 1, 2, 3, ...
years; a term between two is interpolated linearly, one before the first
takes the first rate and one beyond the last the last rate. One rate
gives a flat curve.

## See also

[`risk_adjustment`](https://netsimanalytics.github.io/NetSimR/reference/risk_adjustment.md),
[`sii_premium_reserve_risk`](https://netsimanalytics.github.io/NetSimR/reference/sii_premium_reserve_risk.md)
and
[`chain_ladder`](https://netsimanalytics.github.io/NetSimR/reference/chain_ladder.md);
[`run_shiny_reserving_tool`](https://netsimanalytics.github.io/NetSimR/reference/run_shiny_reserving_tool.md),
an application for them.

## Examples

``` r
paid <- as_triangle(simulated_motor_claims, "accident_year", "development_year", "paid",
                    cumulative = FALSE)
# a rising yield curve: 3% at one year to 3.8% at five years and beyond
be <- discount_reserves(paid, yield_curve = c(0.030, 0.032, 0.034, 0.036, 0.038))
be$total
#>      reserve   discounted     discount    mean_term 
#> 1.501854e+07 1.403594e+07 9.826068e+05 2.059123e+00 
head(be$cash_flows)
#>   period  payments term spot_rate discount_factor present_value
#> 1      1 5675170.9  0.5     0.030       0.9853293     5591912.1
#> 2      2 3516932.1  1.5     0.031       0.9552389     3359510.4
#> 3      3 2238619.4  2.5     0.033       0.9220388     2064094.0
#> 4      4 1422410.0  3.5     0.035       0.8865613     1261053.7
#> 5      5  923760.7  4.5     0.037       0.8491719      784431.6
#> 6      6  542749.1  5.5     0.038       0.8145440      442093.1
```
