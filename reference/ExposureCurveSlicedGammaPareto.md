# Exposure Curve from a Sliced Gamma Pareto severity distribution

Gives the share of the expected claim cost of a sliced Gamma-Pareto
severity distribution that falls below the amount `x` (the capped mean
divided by the mean), as used to exposure rate a layer.

## Usage

``` r
ExposureCurveSlicedGammaPareto(x, GShape, GRate, SlicePoint, PShape)

exposure_curve_sliced_gamma_pareto(x, GShape, GRate, SlicePoint, PShape)
```

## Arguments

- x:

  A non-negative real number - the claim amount where the exposure curve
  will be evaluated.

- GShape:

  A positive real number - the shape parameter of the Claim Severity's
  Gamma distribution.

- GRate:

  A positive real number - the rate parameter of the Claim Severity's
  Gamma distribution.

- SlicePoint:

  A positive real number - the slice point and the scale parameter of
  the Claim Severity's Pareto distribution. An infinite slice point
  gives the Gamma distribution.

- PShape:

  A positive real number - the shape parameter of the Claim Severity's
  Pareto distribution.

## Value

The value of the Exposure curve at `x` with an attritional claim Gamma
distribution with parameters `GShape` and `GRate` and a large claim
Pareto distribution with parameters `SlicePoint` and `PShape`. The
exposure curve divides by the mean, which is infinite when `PShape <= 1`
(and `SlicePoint` is finite); the function returns 0 in that case.

## Details

`PShape` is the Pareto shape parameter, usually written alpha; the
sliced LogNormal-Pareto functions call the same parameter `shape`.

## See also

Other exposure curve functions:
[`ExposureCurveGamma()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveGamma.md),
[`ExposureCurveLNorm()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveLNorm.md),
[`ExposureCurvePareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurvePareto.md),
[`ExposureCurveSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ExposureCurveSlicedLNormPareto.md)

## Examples

``` r
ExposureCurveSlicedGammaPareto(3000,1,0.0005,1000,1.2)
#> [1] 0.3626448
ExposureCurveSlicedGammaPareto(1000,1.1,0.0006,2000,1.6)
#> [1] 0.3313697
ExposureCurveSlicedGammaPareto(2000,1.2,0.0004,3000,1.4)
#> [1] 0.3160543
```
