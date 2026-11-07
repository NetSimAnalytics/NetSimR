# Exposure rating of property and casualty excess of loss layers

Rates a layer of `limit` excess of `deductible` from a risk profile,
without loss experience. Each band's (or policy's) expected loss, its
premium times its loss ratio, is shared between the layer and the rest
by an exposure curve. `property_exposure_rating()` uses an MBBEFD curve
of the damage as a share of the sum insured
([`mbbefd_curve`](https://netsimanalytics.github.io/NetSimR/reference/mbbefd_curve.md)):
the layer takes \\G((d + l) / M) - G(d / M)\\ of a band with sum insured
\\M\\. `casualty_exposure_rating()` uses the limited expected value of a
claim size distribution: the layer takes \\(LEV(\min(P, d + l)) -
LEV(\min(P, d))) / LEV(P)\\ of a band with policy limit \\P\\.

## Usage

``` r
property_exposure_rating(
  profile,
  deductible,
  limit,
  curve = "Y3",
  loss_ratio = NULL
)

casualty_exposure_rating(
  profile,
  deductible,
  limit,
  severity,
  severity_params,
  loss_ratio = NULL
)
```

## Arguments

- profile:

  A data frame with a row per band of the risk profile (or per policy):
  for property, the columns `sum_insured` (the band's sum insured or
  maximum possible loss per risk, often its average) and `premium`; for
  casualty, `policy_limit` and `premium`. An optional `loss_ratio`
  column gives each band's expected loss ratio; other columns, such as a
  band label, are kept.

- deductible:

  The deductible of the layer, at least 0.

- limit:

  The limit of the layer, above 0; `Inf` for an unlimited layer.

- curve:

  The exposure curve: a Swiss Re curve by name ("Y1", "Y2", "Y3", "Y4"
  or "Lloyds") or by its parameter c, or an MBBEFD curve as
  `c(b = , g = )`.

- loss_ratio:

  The expected ground-up loss ratio: one for all bands, or one per band.
  NULL (the default) takes the profile's `loss_ratio` column.

- severity:

  The claim size distribution of the casualty business, as in
  [`simulate_claims`](https://netsimanalytics.github.io/NetSimR/reference/simulate_claims.md):
  "LogNormal" (meanlog, sdlog), "Gamma" (shape, scale), "Exponential"
  (rate) or "Pareto" (alpha, x_m).

- severity_params:

  Its parameters, in that order or named.

## Value

An object of class `netsimr_exposure_rating`, a list with `bands` (the
profile with each band's loss ratio, expected loss, the shares of the
curve at the deductible and the exhaustion point, the layer's share and
the expected layer loss), `expected_loss` (the layer's), `rate` (the
expected layer loss as a share of the subject premium),
`subject_premium`, `ground_up_loss`, the layer and the curve or
severity.

## See also

[`burning_cost`](https://netsimanalytics.github.io/NetSimR/reference/burning_cost.md)
and
[`pareto_rating`](https://netsimanalytics.github.io/NetSimR/reference/pareto_rating.md),
the experience rating of a layer, and
[`technical_premium`](https://netsimanalytics.github.io/NetSimR/reference/technical_premium.md).

Other reinsurance pricing functions:
[`burning_cost()`](https://netsimanalytics.github.io/NetSimR/reference/burning_cost.md),
[`credibility_weight()`](https://netsimanalytics.github.io/NetSimR/reference/credibility_weight.md),
[`mbbefd_curve()`](https://netsimanalytics.github.io/NetSimR/reference/mbbefd_curve.md),
[`pareto_rating()`](https://netsimanalytics.github.io/NetSimR/reference/pareto_rating.md),
[`technical_premium()`](https://netsimanalytics.github.io/NetSimR/reference/technical_premium.md)

## Examples

``` r
rating <- property_exposure_rating(xl_property_profile, deductible = 2e6, limit = 8e6,
                                   curve = "Y3", loss_ratio = 0.55)
rating
#> Exposure rating (property) of 8,000,000 xs 2,000,000
#>   Curve:              Swiss Re Y3 (c = 3)
#>   Bands:              9
#>   Subject premium:    57,227,000
#>   Ground-up loss:     31,474,850
#>   Expected layer loss: 2,370,411 
#>   Rate:               4.142% of subject premium
rating$bands[, c("band", "sum_insured", "layer_share", "layer_loss")]
#>           band sum_insured layer_share layer_loss
#> 1    0 to 250k      138000   0.0000000        0.0
#> 2 250k to 500k      382000   0.0000000        0.0
#> 3   500k to 1m      750000   0.0000000        0.0
#> 4   1m to 2.5m     1715000   0.0000000        0.0
#> 5   2.5m to 5m     3638000   0.1960620   586509.6
#> 6    5m to 10m     7200000   0.3749192   748320.0
#> 7   10m to 25m    16450000   0.3896002   620340.9
#> 8   25m to 50m    34875000   0.3274763   301507.5
#> 9  50m to 100m    69000000   0.2775672   113733.2

casualty_exposure_rating(xl_casualty_profile, deductible = 1e6, limit = 4e6,
                         severity = "LogNormal", severity_params = c(10, 2), loss_ratio = 0.65)
#> Exposure rating (casualty) of 4,000,000 xs 1,000,000
#>   Curve:              Log-Normal (meanlog = 10, sdlog = 2)
#>   Bands:              6
#>   Subject premium:    49,000,000
#>   Ground-up loss:     31,850,000
#>   Expected layer loss: 3,298,205 
#>   Rate:               6.731% of subject premium
```
