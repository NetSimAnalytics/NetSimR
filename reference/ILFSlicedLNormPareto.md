# Increased Limit Factor Curve from a Sliced LogNormal Pareto severity distribution

Gives the ratio of the sliced LogNormal-Pareto capped mean at `xHigh` to
that at `xLow`, the factor that takes the expected cost of a policy
limit of `xLow` to that of a limit of `xHigh`.

## Usage

``` r
ILFSlicedLNormPareto(xLow, xHigh, mu, sigma, SlicePoint, shape)

ilf_sliced_lnorm_pareto(xLow, xHigh, mu, sigma, SlicePoint, shape)
```

## Arguments

- xLow:

  A non-negative real number - the claim amount where the Limit Factor
  Curve will be evaluated from.

- xHigh:

  A non-negative real number - the claim amount where the Limit Factor
  Curve will be evaluated to.

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

The value of the Increased Limit Factor curve from `xLow` to `xHigh`
with an attritional claim LogNormal distribution with parameters `mu`
and `sigma` and a large claim Pareto distribution with parameters
`SlicePoint` and `shape`.

## Details

`shape` is the Pareto shape parameter, usually written alpha; the sliced
Gamma-Pareto functions call the same parameter `PShape`.

## See also

Other ILF functions:
[`ILFGamma()`](https://netsimanalytics.github.io/NetSimR/reference/ILFGamma.md),
[`ILFLNorm()`](https://netsimanalytics.github.io/NetSimR/reference/ILFLNorm.md),
[`ILFPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFPareto.md),
[`ILFSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/ILFSlicedGammaPareto.md)

## Examples

``` r
ILFSlicedLNormPareto(800,1200,6,1.5,1000,1.2)
#> [1] 1.243914
ILFSlicedLNormPareto(2000,4000,7,1.6,3000,1.4)
#> [1] 1.455948
```
