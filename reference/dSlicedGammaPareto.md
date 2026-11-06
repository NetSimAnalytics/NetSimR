# The probability density function (pdf) of a Sliced Gamma Pareto severity distribution

Gives the probability density at the claim amount `x` of a sliced
severity distribution, with Gamma attritional claims below the slice
point and a Pareto tail above it.

## Usage

``` r
dSlicedGammaPareto(x, GShape, GRate, SlicePoint, PShape)

dsliced_gamma_pareto(x, GShape, GRate, SlicePoint, PShape)
```

## Arguments

- x:

  A real number - the claim amount where the probability density
  function (pdf) will be evaluated. The pdf is 0 for negative `x`.

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

The value of the probability density function (pdf) at `x` with an
attritional claim Gamma distribution with parameters `GShape` and
`GRate` and a large claim Pareto distribution with parameters
`SlicePoint` and `PShape`. A non-numeric argument or a non-positive
parameter is an error; `NA` values give `NA`.

## Details

`PShape` is the Pareto shape parameter, usually written alpha; the
sliced LogNormal-Pareto functions call the same parameter `shape`.

## See also

Other sliced distribution functions:
[`SlicedGammaParetoMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedGammaParetoMean.md),
[`SlicedLNormParetoMean()`](https://netsimanalytics.github.io/NetSimR/reference/SlicedLNormParetoMean.md),
[`dSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedLNormPareto.md),
[`pSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedGammaPareto.md),
[`pSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedLNormPareto.md),
[`qSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedGammaPareto.md),
[`qSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedLNormPareto.md)

## Examples

``` r
dSlicedGammaPareto(3000,1,0.0005,1000,1.2)
#> [1] 6.491832e-05
dSlicedGammaPareto(1000,1.1,0.0006,2000,1.6)
#> [1] 0.0003288887
dSlicedGammaPareto(2000,1.2,0.0004,3000,1.4)
#> [1] 0.0001872061
```
