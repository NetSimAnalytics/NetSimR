# Credibility of the loss experience

The weight of the experience rating against the exposure rating, by
limited fluctuation credibility: \\Z = \min(1, \sqrt{n / n\_{full}})\\
for \\n\\ losses, with \\n\_{full}\\ the number of losses for full
credibility. The blended rate is \\Z\\ times the experience rate plus
\\1 - Z\\ times the exposure rate.

## Usage

``` r
credibility_weight(n, full = 1082)
```

## Arguments

- n:

  The number of losses in the layer (or above the threshold).

- full:

  The number of losses for full credibility; the default, 1082, gives
  the observed frequency within 5% of the true one with a probability of
  90% for Poisson counts.

## Value

The credibility weight, between 0 and 1.

## See also

Other reinsurance pricing functions:
[`burning_cost()`](https://netsimanalytics.github.io/NetSimR/reference/burning_cost.md),
[`mbbefd_curve()`](https://netsimanalytics.github.io/NetSimR/reference/mbbefd_curve.md),
[`pareto_rating()`](https://netsimanalytics.github.io/NetSimR/reference/pareto_rating.md),
[`property_exposure_rating()`](https://netsimanalytics.github.io/NetSimR/reference/property_exposure_rating.md),
[`technical_premium()`](https://netsimanalytics.github.io/NetSimR/reference/technical_premium.md)

## Examples

``` r
credibility_weight(25)
#> [1] 0.1520045
credibility_weight(25, full = 100)
#> [1] 0.5
```
