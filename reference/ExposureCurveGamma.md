# Exposure Curve from a Gamma severity distribution

Gives the share of the expected claim cost of a Gamma severity
distribution that falls below the amount `x` (the capped mean divided by
the mean), as used to exposure rate a layer.

## Usage

``` r
ExposureCurveGamma(x, shape, rate)

exposure_curve_gamma(x, shape, rate)
```

## Arguments

- x:

  A non-negative real number - the claim amount where the exposure curve
  will be evaluated.

- shape:

  A positive real number - the shape parameter of the Claim Severity's
  Gamma distribution.

- rate:

  A positive real number - the rate parameter of the Claim Severity's
  Gamma distribution.

## Value

The value of the Exposure curve at `x` with Claim Severity from a Gamma
distribution with parameters `shape` and `rate`. The arguments are
recycled to a common length. A non-numeric argument, a negative `x` or a
non-positive `shape` or `rate` is an error; `NA` values give `NA`.

## See also

Other exposure curve functions:
[`ExposureCurveLNorm()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveLNorm.md),
[`ExposureCurvePareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurvePareto.md),
[`ExposureCurveSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedGammaPareto.md),
[`ExposureCurveSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedLNormPareto.md)

## Examples

``` r
ExposureCurveGamma(700,1,0.0005)
#> [1] 0.2953119
ExposureCurveGamma(1000,1.5,0.0006)
#> [1] 0.3563244
```
