# Exposure Curve from a LogNormal severity distribution

Gives the share of the expected claim cost of a LogNormal severity
distribution that falls below the amount `x` (the capped mean divided by
the mean), as used to exposure rate a layer.

## Usage

``` r
ExposureCurveLNorm(x, mu, sigma)

exposure_curve_lnorm(x, mu, sigma)
```

## Arguments

- x:

  A non-negative real number - the claim amount where the exposure curve
  will be evaluated.

- mu:

  A real number - the first parameter of the Claim Severity's LogNormal
  distribution.

- sigma:

  A positive real number - the second parameter of the Claim Severity's
  LogNormal distribution.

## Value

The value of the Exposure curve at `x` with Claim Severity from a
LogNormal distribution with parameters `mu` and `sigma`. An infinite `x`
gives 1. The arguments are recycled to a common length. A non-numeric
argument, a negative `x` or a non-positive `sigma` is an error; `NA`
values give `NA`.

## See also

Other exposure curve functions:
[`ExposureCurveGamma()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveGamma.md),
[`ExposureCurvePareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurvePareto.md),
[`ExposureCurveSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedGammaPareto.md),
[`ExposureCurveSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedLNormPareto.md)

## Examples

``` r
ExposureCurveLNorm(2000,6,1.5)
#> [1] 0.5626383
ExposureCurveLNorm(1000,5,1.6)
#> [1] 0.5601327
```
