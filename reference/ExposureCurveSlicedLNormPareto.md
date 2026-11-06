# Exposure Curve from a Sliced LogNormal Pareto severity distribution

Gives the share of the expected claim cost of a sliced LogNormal-Pareto
severity distribution that falls below the amount `x` (the capped mean
divided by the mean), as used to exposure rate a layer.

## Usage

``` r
ExposureCurveSlicedLNormPareto(x, mu, sigma, SlicePoint, shape)

exposure_curve_sliced_lnorm_pareto(x, mu, sigma, SlicePoint, shape)
```

## Arguments

- x:

  A non-negative real number - the claim amount where the exposure curve
  will be evaluated.

- mu:

  A real number - the first parameter of the attritional Claim
  Severity's LogNormal distribution.

- sigma:

  A positive real number - the second parameter of the attritional Claim
  Severity's LogNormal distribution.

- SlicePoint:

  A positive real number - the slice point and the scale parameter of
  the tail Claim Severity's Pareto distribution. An infinite slice point
  gives the LogNormal distribution.

- shape:

  A positive real number - the shape parameter of the tail Claim
  Severity's Pareto distribution.

## Value

The value of the Exposure curve at `x` with an attritional claim
LogNormal distribution with parameters `mu` and `sigma` and a large
claim Pareto distribution with parameters `SlicePoint` and `shape`. The
exposure curve divides by the mean, which is infinite when `shape <= 1`
(and `SlicePoint` is finite); the function returns 0 in that case.

## Details

`shape` is the Pareto shape parameter, usually written alpha; the sliced
Gamma-Pareto functions call the same parameter `PShape`.

## See also

Other exposure curve functions:
[`ExposureCurveGamma()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveGamma.md),
[`ExposureCurveLNorm()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveLNorm.md),
[`ExposureCurvePareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurvePareto.md),
[`ExposureCurveSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedGammaPareto.md)

## Examples

``` r
ExposureCurveSlicedLNormPareto(1200,6,1.5,1000,1.2)
#> [1] 0.2957505
ExposureCurveSlicedLNormPareto(4000,7,1.6,3000,1.4)
#> [1] 0.4846078
```
