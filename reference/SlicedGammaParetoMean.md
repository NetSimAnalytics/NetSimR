# Sliced Gamma Pareto mean

Gives the expected claim amount of a sliced severity distribution, with
Gamma attritional claims below the slice point and a Pareto tail above
it.

## Usage

``` r
SlicedGammaParetoMean(GShape, GRate, SlicePoint, PShape)

sliced_gamma_pareto_mean(GShape, GRate, SlicePoint, PShape)
```

## Arguments

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

The mean of the claim severity with an attritional claim Gamma
distribution with parameters `GShape` and `GRate` and a large claim
Pareto distribution with parameters `SlicePoint` and `PShape`. The mean
is `Inf` when `PShape <= 1` (and `SlicePoint` is finite), as the Pareto
tail then has no finite mean. A non-numeric or non-positive parameter is
an error; `NA` values give `NA`.

## Details

`PShape` is the Pareto shape parameter, usually written alpha; the
sliced LogNormal-Pareto functions call the same parameter `shape`.

## See also

Other sliced distribution functions:
[`SlicedLNormParetoMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoMean.md),
[`dSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedGammaPareto.md),
[`dSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedLNormPareto.md),
[`pSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedGammaPareto.md),
[`pSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedLNormPareto.md),
[`qSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedGammaPareto.md),
[`qSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedLNormPareto.md)

## Examples

``` r
SlicedGammaParetoMean(1,0.0005,1000,1.2)
#> [1] 3819.592
SlicedGammaParetoMean(1.1,0.0006,2000,1.6)
#> [1] 2378.78
SlicedGammaParetoMean(1.2,0.0004,3000,1.4)
#> [1] 4822.453
```
