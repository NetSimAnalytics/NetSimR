# Technical premium of an excess of loss layer

The premium of a layer from its expected loss: the total premium covers
the expected loss and a loading for expenses, brokerage and profit, and
with paid reinstatements part of it comes as reinstatement premium, so
the upfront premium is lower.

## Usage

``` r
technical_premium(
  expected_loss,
  limit,
  loading = 0,
  reinstatements = 0,
  reinstatement_rate = 1,
  used = NULL
)
```

## Arguments

- expected_loss:

  The expected loss of the layer.

- limit:

  The limit of the layer.

- loading:

  The share of the total premium for expenses, brokerage and profit,
  from 0 to below 1. Defaults to 0.

- reinstatements:

  The number of paid reinstatements; 0 (the default) for none, `Inf` for
  unlimited.

- reinstatement_rate:

  The rate of each reinstatement, as a share of the upfront premium.
  Defaults to 1 (100%).

- used:

  The expected reinstated cover as a share of the limit; NULL (the
  default) for \\\min(EL / l, n)\\.

## Value

A list with `expected_loss`, `total_premium`, `upfront_premium`,
`reinstatement_premium` (expected), `rate_on_line` (the upfront premium
over the limit), `payback` (the limit over the upfront premium, in
years) and `used`.

## Details

The total premium is \\EL / (1 - loading)\\. With \\n\\ reinstatements
paid at the rate \\r\\ of the upfront premium \\P\\, pro rata as to
amount, the expected reinstatement premium is \\P r u\\, where \\u\\ is
the expected reinstated cover as a share of the limit, \\E\[\min(S, n
l)\] / l\\ for the annual layer loss \\S\\; so \\P = EL / ((1 -
loading)(1 + r u))\\. Without `used`, \\u\\ is taken as \\\min(EL / l,
n)\\, which is exact when the annual losses never exceed the reinstated
cover; the burning cost's layer losses at the rating premium give a
better one (see the examples). For the reinstatement premiums of each
simulated year, see
[`reinstatement_premium`](https://netsimanalytics.github.io/NetSimR/reference/reinstatement_premium.md).

## See also

Other reinsurance pricing functions:
[`burning_cost()`](https://netsimanalytics.github.io/NetSimR/reference/burning_cost.md),
[`credibility_weight()`](https://netsimanalytics.github.io/NetSimR/reference/credibility_weight.md),
[`mbbefd_curve()`](https://netsimanalytics.github.io/NetSimR/reference/mbbefd_curve.md),
[`pareto_rating()`](https://netsimanalytics.github.io/NetSimR/reference/pareto_rating.md),
[`property_exposure_rating()`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md)

## Examples

``` r
technical_premium(expected_loss = 1.2e6, limit = 4e6, loading = 0.25,
                  reinstatements = 1, reinstatement_rate = 1)
#> $expected_loss
#> [1] 1200000
#> 
#> $total_premium
#> [1] 1600000
#> 
#> $upfront_premium
#> [1] 1230769
#> 
#> $reinstatement_premium
#> [1] 369230.8
#> 
#> $rate_on_line
#> [1] 0.3076923
#> 
#> $payback
#> [1] 3.25
#> 
#> $used
#> [1] 0.3
#> 

# the reinstated cover used in each past year, at the rating year's premium
bc <- burning_cost(xl_large_losses, xl_premium[, c("year", "premium")], 1e6, 4e6,
                   inflation = 0.04, rating_premium = 60e6)
used <- mean(pmin(bc$by_year$at_rating_premium, 1 * 4e6)) / 4e6
technical_premium(bc$expected_loss, 4e6, loading = 0.25, reinstatements = 1, used = used)
#> $expected_loss
#> [1] 2782345
#> 
#> $total_premium
#> [1] 3709794
#> 
#> $upfront_premium
#> [1] 2494674
#> 
#> $reinstatement_premium
#> [1] 1215120
#> 
#> $rate_on_line
#> [1] 0.6236686
#> 
#> $payback
#> [1] 1.603416
#> 
#> $used
#> [1] 0.4870854
#> 
```
