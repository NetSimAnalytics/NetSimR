# The cumulative distribution function (cdf) of a Sliced Gamma-Pareto severity distribution

Gives the probability that a claim from a sliced severity distribution,
with Gamma attritional claims below the slice point and a Pareto tail
above it, is at most `x`.

## Usage

``` r
pSlicedGammaPareto(x, GShape, GRate, SlicePoint, PShape)

psliced_gamma_pareto(x, GShape, GRate, SlicePoint, PShape)
```

## Arguments

- x:

  A real number - the claim amount where the cumulative distribution
  function (cdf) will be evaluated. The cdf is 0 for negative `x`.

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

The value of the cumulative distribution function (cdf) at `x` with an
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
[`dSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedGammaPareto.md),
[`dSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/dSlicedLNormPareto.md),
[`pSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/pSlicedLNormPareto.md),
[`qSlicedGammaPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedGammaPareto.md),
[`qSlicedLNormPareto()`](https://netsimanalytics.github.io/NetSimR/reference/qSlicedLNormPareto.md)

## Examples

``` r
pSlicedGammaPareto(3000,1,0.0005,1000,1.2)
#> [1] 0.8377042
pSlicedGammaPareto(1000,1.1,0.0006,2000,1.6)
#> [1] 0.4036842
pSlicedGammaPareto(2000,1.2,0.0004,3000,1.4)
#> [1] 0.4601873
```
