# Exposure Curve from a Pareto severity distribution

Gives the share of the expected claim cost of a Pareto severity
distribution that falls below the amount `x` (the capped mean divided by
the mean), as used to exposure rate a layer.

## Usage

``` r
ExposureCurvePareto(x, scale, shape)

exposure_curve_pareto(x, scale, shape)
```

## Arguments

- x:

  A non-negative real number - the claim amount where the exposure curve
  will be evaluated.

- scale:

  A positive real number - the scale parameter of the Claim Severity's
  Pareto distribution.

- shape:

  A positive real number - the shape parameter of the Claim Severity's
  Pareto distribution.

## Value

The value of the Exposure curve at `x` with Claim Severity from a Pareto
distribution with parameters `scale` and `shape`. The exposure curve
divides by the mean, which is infinite when `shape <= 1`; the function
returns 0 in that case.

## See also

Other exposure curve functions:
[`ExposureCurveGamma()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveGamma.md),
[`ExposureCurveLNorm()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveLNorm.md),
[`ExposureCurveSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedGammaPareto.md),
[`ExposureCurveSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedLNormPareto.md)

## Examples

``` r
ExposureCurvePareto(700,500,1.2)
#> [1] 0.2209001
ExposureCurvePareto(20000,200,1.1)
#> [1] 0.4264024
```
