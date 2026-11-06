# Sliced LogNormal Pareto mean

Gives the expected claim amount of a sliced severity distribution, with
LogNormal attritional claims below the slice point and a Pareto tail
above it.

## Usage

``` r
SlicedLNormParetoMean(mu, sigma, SlicePoint, shape)

sliced_lnorm_pareto_mean(mu, sigma, SlicePoint, shape)
```

## Arguments

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

The mean of the claim severity with an attritional claim LogNormal
distribution with parameters `mu` and `sigma` and a large claim Pareto
distribution with parameters `SlicePoint` and `shape`. The mean is `Inf`
when `shape <= 1` (and `SlicePoint` is finite), as the Pareto tail then
has no finite mean. A non-numeric argument or a non-positive `sigma`,
`SlicePoint` or `shape` is an error; `NA` values give `NA`.

## Details

`shape` is the Pareto shape parameter, usually written alpha; the sliced
Gamma-Pareto functions call the same parameter `PShape`.

## See also

Other sliced distribution functions:
[`SlicedGammaParetoMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoMean.md),
[`dSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedGammaPareto.md),
[`dSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedLNormPareto.md),
[`pSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedGammaPareto.md),
[`pSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedLNormPareto.md),
[`qSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedGammaPareto.md),
[`qSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedLNormPareto.md)

## Examples

``` r
SlicedLNormParetoMean(6,1.5,1000,1.2)
#> [1] 1865.633
SlicedLNormParetoMean(6.5,1.4,2000,1.6)
#> [1] 1629.106
SlicedLNormParetoMean(7,1.6,3000,1.4)
#> [1] 3432.978
```
