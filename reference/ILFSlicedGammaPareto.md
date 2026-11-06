# Increased Limit Factor Curve from a Sliced Gamma Pareto severity distribution

Gives the ratio of the sliced Gamma-Pareto capped mean at `xHigh` to
that at `xLow`, the factor that takes the expected cost of a policy
limit of `xLow` to that of a limit of `xHigh`.

## Usage

``` r
ILFSlicedGammaPareto(xLow, xHigh, GShape, GRate, SlicePoint, PShape)

ilf_sliced_gamma_pareto(xLow, xHigh, GShape, GRate, SlicePoint, PShape)
```

## Arguments

- xLow:

  A non-negative real number - the claim amount where the Limit Factor
  Curve will be evaluated from.

- xHigh:

  A non-negative real number - the claim amount where the Limit Factor
  Curve will be evaluated to.

- GShape:

  A positive real number - the shape parameter of the attritional Claim
  Severity's Gamma distribution.

- GRate:

  A positive real number - the rate parameter of the attritional Claim
  Severity's Gamma distribution.

- SlicePoint:

  A positive real number - the slice point and the scale parameter of
  the tail Claim Severity's Pareto distribution. An infinite slice point
  gives the Gamma distribution.

- PShape:

  A positive real number - the shape parameter of the tail Claim
  Severity's Pareto distribution.

## Value

The value of the Increased Limit Factor curve from `xLow` to `xHigh`
with an attritional claim Gamma distribution with parameters `GShape`
and `GRate` and a large claim Pareto distribution with parameters
`SlicePoint` and `PShape`.

## Details

`PShape` is the Pareto shape parameter, usually written alpha; the
sliced LogNormal-Pareto functions call the same parameter `shape`.

## See also

Other ILF functions:
[`ILFGamma()`](https://netsimanalytics.github.io/NetSimR/reference/ILFGamma.md),
[`ILFLNorm()`](https://netsimanalytics.github.io/NetSimR/reference/ILFLNorm.md),
[`ILFPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFPareto.md),
[`ILFSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedLNormPareto.md)

## Examples

``` r
ILFSlicedGammaPareto(2000,3000,1,0.0005,1000,1.2)
#> [1] 1.174344
ILFSlicedGammaPareto(800,1000,1.1,0.0006,2000,1.6)
#> [1] 1.190373
ILFSlicedGammaPareto(1200,2000,1.2,0.0004,3000,1.4)
#> [1] 1.483324
```
